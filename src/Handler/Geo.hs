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
import qualified Control.Lens as L ((^?))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, AsValue (_Array, _String), AsNumber (_Double), nth)
import Data.List (sort)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text, unpack)
import qualified Data.Text as T

import Database.Esqueleto.Experimental (selectOne, from, table, delete)
import Database.Persist (Entity (entityVal), insert_)

import Foundation
    ( Handler, Form, App (appSettings), widgetSnackbar, widgetTopbar, mapboxStyles
    , Route (DataR)
    , DataR
      ( BboxR, SettingsGeoCountryR, SettingsGeoCityR, SettingsGeoBboxR, DisplayR
      , SettingsGeoDeleR
      )
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay, MsgCountry, MsgCity, MsgRegion
      , MsgNext, MsgSave, MsgLatitude, MsgLongitude, MsgZoom, MsgCenter
      , MsgRecordEdited, MsgBoundingBox, MsgDele
      , MsgCancel, MsgConfirmPlease, MsgDeleteAreYouSure, MsgRecordDeleted
      , MsgInvalidFormData, MsgNoGeoRegionSetYet, MsgAdd
      )
    )

import Material3 (md3selectWidget, md3widget)
import Model
    ( overpass, msgSuccess, msgError
    , DefaultMapStyle (defaultMapStyleStyle)
    , MapboxParam
      ( MapboxParam, mapboxParamLon, mapboxParamLat, mapboxParamZoom
      , mapboxParamCity, mapboxParamCountry, mapboxParamLang
      )
    )

import Network.Wreq (post, FormParam ((:=)), responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Hamlet (Html)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , redirect, languages, getYesod, addStylesheetRemote, addScriptRemote
    , SomeMessage (SomeMessage), addMessageI, MonadHandler (liftHandler)
    , getMessages
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

    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area["name#{lang}"="#{country}"];
                       node(area)[place="city"]["name#{lang}"="#{city}"];
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
    
    mapboxStyle <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )

    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) . LS.head <$> languages

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area["name#{lang}"="#{country}"];
                       node(area)[place="city"]["name#{lang}"="#{city}"];
                       out center;
                       |]
        ]

    let c = r L.^? responseBody . key "elements" . nth 0
            . to (\x -> (x L.^? key "lon" . _Double, x L.^? key "lat" . _Double))

    let center = case c of
          Just (Just lon, Just lat) -> (lon,lat)
          _otherwise -> (0,0)

    language <- LS.head <$> languages

    let param = MapboxParam { mapboxParamCountry = country
                            , mapboxParamCity = city
                            , mapboxParamLang = language
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

    lang <- liftHandler $ LS.head <$> languages
    
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

    return ( MapboxParam country city lang <$> lonR <*> latR <*> zoomR
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

    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area["name#{lang}"="#{country}"];
                       node(area)[place="city"]["name#{lang}"];
                       out tags;
                       |]
        ]

    let cities = sort $ r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . key (fromText ("name" <> lang))
            . _String
    
    ((fr,fw),et) <- runFormPost $ formCity cities
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
    
    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages

    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       area["name#{lang}"="#{country}"];
                       node(area)[place="city"]["name#{lang}"];
                       out tags;
                       |]
        ]

    let cities = sort $ r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . key (fromText ("name" <> lang))
            . _String
    
    (fw,et) <- generateFormPost $ formCity cities
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

    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages
    
    r <- liftIO $ post (unpack overpass)
        [ "data" := [st|
                       [out:json];
                       node[place=country]["name#{lang}"];
                       out tags;
                       |]
        ]

    let countries = sort $ r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . key (fromText ("name" <> lang))
            . _String

    ((fr,fw),et) <- runFormPost $ formCountry countries
    
    case fr of
      FormSuccess city -> redirect $ DataR $ SettingsGeoCityR city
          
      _otherwise -> do
          geo <- runDB $ selectOne $ from $ table @MapboxParam
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


formCity :: [Text] -> Form Text
formCity cities extra = do
    (cityR,cityV) <- mreq (selectField (optionsPairs ((\x -> (x,x)) <$> cities))) FieldSettings
        { fsLabel = SomeMessage MsgCity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    return (cityR, [whamlet|^{extra} ^{md3selectWidget cityV}|])


getSettingsGeoCountryR :: Handler Html
getSettingsGeoCountryR = do

    geo <- runDB $ selectOne $ from $ table @MapboxParam
    
    lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages

    r <- liftIO $ post (unpack overpass)
         [ "data" := [st|
                        [out:json];
                        node[place=country]["name#{lang}"];
                        out tags;
                        |]
         ]

    let countries = sort $ r ^.. responseBody . key "elements" . _Array . each . key "tags"
            . key (fromText ("name" <> lang))
            . _String

    (fw,et) <- generateFormPost $ formCountry countries
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


formCountry :: [Text] -> Form Text
formCountry countries extra = do
    (countryR,countryV) <- mreq (selectField (optionsPairs ((\x -> (x,x)) <$> countries))) FieldSettings
        { fsLabel = SomeMessage MsgCountry
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    return (countryR, [whamlet|^{extra} ^{md3selectWidget countryV}|])
