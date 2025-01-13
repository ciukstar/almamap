{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Geo
  ( getSettingsGeoCountryR, postSettingsGeoCountryR
  , getSettingsGeoCityR, postSettingsGeoCityR
  , getSettingsGeoBboxR, postSettingsGeoBboxR
  , postSettingsGeoDeleR
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((^..), Each(each), to)
import qualified Control.Lens as L ((^?),(^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, AsValue (_Array, _String), AsNumber (_Double), nth)
import Data.List (sortBy)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text, unpack)
import qualified Data.Text as T (cons, takeWhile)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, delete, Value (unValue), val, where_
    , (^.), (==.)
    )
import Database.Persist (Entity (entityVal), insert_)

import Foundation
    ( Handler, Form, App (appSettings), widgetSnackbar, widgetTopbar, mapboxStyles
    , Route (DataR)
    , DataR
      ( BboxR, SettingsGeoCountryR, SettingsGeoCityR, SettingsGeoBboxR, DisplayR
      , SettingsGeoDeleR, EndpointsR
      )
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay, MsgCountry, MsgCity
      , MsgNext, MsgSave, MsgLatitude, MsgLongitude, MsgZoom, MsgCenter
      , MsgRecordEdited, MsgDele, MsgBbox, MsgEndpoints
      , MsgCancel, MsgConfirmPlease, MsgDeleteAreYouSure, MsgRecordDeleted
      , MsgInvalidFormData, MsgNoGeoRegionSetYet, MsgAdd
      )
    )

import Material3 (md3selectWidget, md3widget)
import Model
    ( msgSuccess, msgError, keyEndpointOverpass
    , DefaultMapStyle (defaultMapStyleStyle)
    , Endpoint
    , MapboxParam
      ( MapboxParam, mapboxParamLon, mapboxParamLat, mapboxParamZoom
      , mapboxParamCity, mapboxParamCountry
      )
    , EntityField (EndpointKey, EndpointVal)
    )
import qualified Model (overpass)

import Network.Wreq (post, FormParam ((:=)), responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Hamlet (Html, shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , redirect, languages, getYesod, addStylesheetRemote, addScriptRemote
    , SomeMessage (SomeMessage), addMessageI, getMessages
    )
import Yesod.Form.Fields (selectField, optionsPairs, doubleField)
import Yesod.Form.Functions (mreq, runFormPost, generateFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postSettingsGeoDeleR :: Handler Html
postSettingsGeoDeleR = do
    ((fr,_),_) <- runFormPost formSettingsGeoDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ void $ from $ table @MapboxParam
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR SettingsGeoCountryR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR SettingsGeoCountryR


postSettingsGeoBboxR :: Text -> Text -> Handler Html
postSettingsGeoBboxR country city = do
    
    mapboxStyle <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area[name="#{country}"] -> .country;                       
                       node(area.country)[place="city"][name="#{city}"];
                       out center;
                       |]
        ]

    let c = r L.^? responseBody . key "elements" . nth 0 . to (\x -> (x L.^? key "lon" . _Double, x L.^? key "lat" . _Double))
        
    let center = case c of
          Just (Just lon, Just lat) -> (lon,lat)
          _otherwise -> (0,0)

    idInputLon <- newIdent
    idInputLat <- newIdent
    idInputZoom <- newIdent

    params <- runDB $ selectOne $ from $ table @MapboxParam
            
    ((fr,fw),et) <- runFormPost $ formMapViewport
        idInputLon idInputLat idInputZoom
        country city
        (entityVal <$> params)

    (fw0,et0) <- generateFormPost formSettingsGeoDelete

    case fr of
      FormSuccess param -> do
          runDB $ delete $ void $ from $ table @MapboxParam
          runDB $ insert_ param
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SettingsGeoBboxR country city
      
      _otherwise -> do
          langs <- languages
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              idMap <- newIdent
              idButtonShowDialogDelete <- newIdent
              idDialogDelete <- newIdent

              mapboxPk <- appMapboxPk . appSettings <$> getYesod

              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

              $(widgetFile "data/settings/geo/center")


getSettingsGeoBboxR :: Text -> Text -> Handler Html
getSettingsGeoBboxR country city = do

    langs <- languages
    
    mapboxStyle <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area[name="#{country}"] -> .country;   
                       node(area.country)[place="city"][name="#{city}"];
                       out center;
                       |]
        ]

    let c = r L.^? responseBody . key "elements" . nth 0
            . to (\x -> (x L.^? key "lon" . _Double, x L.^? key "lat" . _Double))

    let center = case c of
          Just (Just lon, Just lat) -> (lon,lat)
          _otherwise -> (0,0)

    let param = MapboxParam { mapboxParamCountry = country
                            , mapboxParamCity = city
                            , mapboxParamLon = fst center
                            , mapboxParamLat = snd center
                            , mapboxParamZoom = 9
                            }

    idInputLon <- newIdent
    idInputLat <- newIdent
    idInputZoom <- newIdent
        
    (fw,et) <- generateFormPost $ formMapViewport
        idInputLon idInputLat idInputZoom
        country city
        (Just param)

    (fw0,et0) <- generateFormPost formSettingsGeoDelete
    
    msgr <- getMessageRender
    msgs <- getMessages
    
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        idMap <- newIdent
        idButtonShowDialogDelete <- newIdent
        idDialogDelete <- newIdent

        mapboxPk <- appMapboxPk . appSettings <$> getYesod

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
            
        $(widgetFile "data/settings/geo/center")


formSettingsGeoDelete :: Form ()
formSettingsGeoDelete extra = return (pure (), [whamlet|#{extra}|])


formMapViewport :: Text -> Text -> Text
                -> Text -> Text
                -> Maybe MapboxParam -> Form MapboxParam
formMapViewport idInputLon idInputLat idInputZoom country city params extra = do
    
    (lonR,lonV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgLongitude
        , fsId = Just idInputLon, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamLon <$> params)
        
    (latR,latV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgLatitude
        , fsId = Just idInputLat, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamLat <$> params)
        
    (zoomR,zoomV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgZoom
        , fsId = Just idInputZoom, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamZoom <$> params)

    return ( MapboxParam country city <$> lonR <*> latR <*> zoomR
           , [whamlet|
                     ^{extra}
                     
                     <fieldset>
                       <legend>_{MsgCenter}
                       ^{md3widget lonV}
                       ^{md3widget latV}
                       
                     ^{md3widget zoomV}
                     |]
           )


postSettingsGeoCityR :: Text -> Handler Html
postSettingsGeoCityR country = do

    geo <- runDB $ selectOne $ from $ table @MapboxParam
    
    langSuffix <- (T.cons ':' . T.takeWhile (/= '-') <$>) .  LS.head <$> languages :: Handler (Maybe Text)

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
        [ "data" := renderMarkup [shamlet|
                        [out:json];
                        area[name="#{country}"] -> .country;
                        $maybe lang <- langSuffix
                          node(area.country)[place="city"][name]["name#{lang}"];
                          convert tags ::id=id(),"name"=t["name"],"name#{lang}"=t["name#{lang}"];
                        $nothing
                          node(area.country)[place="city"][name];
                          convert tags ::id=id(),"name"=t["name"];
                        out tags;
                        |]
        ]

    let cities = sortBy (\(a,_) (b,_) -> compare a b) $ (\(m,a) -> (fromMaybe a m,a)) <$>
            r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . to (\s -> ( case langSuffix of
                            Just lang -> s L.^? key (fromText ("name" <> lang)) . _String
                            Nothing -> Nothing
                        , s L.^. key "name" . _String
                        ))
    
    ((fr,fw),et) <- runFormPost $ formCity cities geo
    case fr of
      FormSuccess city -> redirect $ DataR $ SettingsGeoBboxR country city

      _otherwise -> do
          (fw0,et0) <- generateFormPost formSettingsGeoDelete
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              idButtonShowDialogDelete <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/settings/geo/city")


getSettingsGeoCityR :: Text -> Handler Html
getSettingsGeoCityR country = do

    geo <- runDB $ selectOne $ from $ table @MapboxParam
    
    langSuffix <- (T.cons ':' . T.takeWhile (/= '-') <$>) .  LS.head <$> languages

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
        [ "data" := renderMarkup [shamlet|
                        [out:json];
                        area[name="#{country}"] -> .country;
                        $maybe lang <- langSuffix
                          node(area.country)[place="city"][name]["name#{lang}"];
                          convert tags ::id=id(),"name"=t["name"],"name#{lang}"=t["name#{lang}"];
                        $nothing
                          node(area.country)[place="city"][name];
                          convert tags ::id=id(),"name"=t["name"];
                        out tags;
                        |]
        ]

    let cities = sortBy (\(a,_) (b,_) -> compare a b) $ (\(m,a) -> (fromMaybe a m,a)) <$>
            r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . to (\s -> ( case langSuffix of
                            Just lang -> s L.^? key (fromText ("name" <> lang)) . _String
                            Nothing -> Nothing
                        , s L.^. key "name" . _String
                        ))
    
    (fw,et) <- generateFormPost $ formCity cities geo
    (fw0,et0) <- generateFormPost formSettingsGeoDelete
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        idButtonShowDialogDelete <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/settings/geo/city")
    

postSettingsGeoCountryR :: Handler Html
postSettingsGeoCountryR = do

    geo <- runDB $ selectOne $ from $ table @MapboxParam
    
    langSuffix <- (T.cons ':' . T.takeWhile (/= '-') <$>) .  LS.head <$> languages

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
         [ "data" := renderMarkup [shamlet|
                        [out:json];
                        $maybe lang <- langSuffix
                          node[place=country][name]["name#{lang}"];
                          convert tags ::id=id(),"name"=t["name"],"name#{lang}"=t["name#{lang}"];
                        $nothing
                          node[place=country][name];
                          convert tags ::id=id(),"name"=t["name"];
                        out tags;
                        |]
         ]

    let countries = sortBy (\(a,_) (b,_) -> compare a b) $ (\(m,a) -> (fromMaybe a m,a)) <$>
            r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . to (\s -> ( case langSuffix of
                            Just lang -> s L.^? key (fromText ("name" <> lang)) . _String
                            Nothing -> Nothing
                        , s L.^. key "name" . _String
                        ))

    ((fr,fw),et) <- runFormPost $ formCountry countries geo
    
    case fr of
      FormSuccess city -> redirect $ DataR $ SettingsGeoCityR city
          
      _otherwise -> do
          (fw0,et0) <- generateFormPost formSettingsGeoDelete
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              idButtonShowDialogDelete <- newIdent
              idGeoRegionFormWrapper <- newIdent
              idFigureNoGeoSettings <- newIdent
              idButtonShowGeoRegionForm <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/settings/geo/country")


formCity :: [(Text,Text)] -> Maybe (Entity MapboxParam) -> Form Text
formCity cities geo extra = do
    (cityR,cityV) <- mreq (selectField (optionsPairs cities)) FieldSettings
        { fsLabel = SomeMessage MsgCity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamCity . entityVal <$> geo)

    return (cityR, [whamlet|^{extra} ^{md3selectWidget cityV}|])


getSettingsGeoCountryR :: Handler Html
getSettingsGeoCountryR = do

    geo <- runDB $ selectOne $ from $ table @MapboxParam
    
    langSuffix <- (T.cons ':' . T.takeWhile (/= '-') <$>) .  LS.head <$> languages

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    r <- liftIO $ post (unpack overpass)
         [ "data" := renderMarkup [shamlet|
                        [out:json];
                        $maybe lang <- langSuffix
                          node[place=country][name]["name#{lang}"];
                          convert tags ::id=id(),"name"=t["name"],"name#{lang}"=t["name#{lang}"];
                        $nothing
                          node[place=country][name];
                          convert tags ::id=id(),"name"=t["name"];
                        out tags;
                        |]
         ]
         
    let countries = sortBy (\(a,_) (b,_) -> compare a b) $ (\(m,a) -> (fromMaybe a m,a)) <$>
            r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . to (\s -> ( case langSuffix of
                            Just lang -> s L.^? key (fromText ("name" <> lang)) . _String
                            Nothing -> Nothing
                        , s L.^. key "name" . _String
                        ))
    
    (fw,et) <- generateFormPost $ formCountry countries geo
    (fw0,et0) <- generateFormPost formSettingsGeoDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        idButtonShowDialogDelete <- newIdent
        idGeoRegionFormWrapper <- newIdent
        idFigureNoGeoSettings <- newIdent
        idButtonShowGeoRegionForm <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/settings/geo/country")


formCountry :: [(Text,Text)] -> Maybe (Entity MapboxParam) -> Form Text
formCountry countries geo extra = do
    (countryR,countryV) <- mreq (selectField (optionsPairs countries)) FieldSettings
        { fsLabel = SomeMessage MsgCountry
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamCountry . entityVal <$> geo)

    return ( countryR
           , [whamlet|
                     ^{extra}
                     ^{md3selectWidget countryV}
                     |]
           )
