{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Settings
  ( getSettingsR, postSettingsR
  , getSettingsGeoCityR, postSettingsGeoCityR
  , getSettingsGeoBboxR, postSettingsGeoBboxR
  ) where

import Control.Lens ((^..), Each(each), to)
import qualified Control.Lens as L ((^?))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, AsValue (_Array, _String), AsNumber (_Double), nth)
import Data.Bifunctor (Bifunctor(second))
import Data.List (sort)
import qualified Data.List.Safe as LS (head)
import Data.Text (Text, unpack)
import qualified Data.Text as T

import Database.Esqueleto.Experimental (selectOne, from, table, delete)
import Database.Persist (Entity (Entity, entityVal), insert_)

import Foundation
    ( Handler, Form, App (appSettings), widgetTopbar
    , Route (DataR)
    , DataR (SettingsR, SettingsGeoCityR, SettingsGeoBboxR)
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay, MsgCountry, MsgCity, MsgRegion
      , MsgNext, MsgSave, MsgLatitude, MsgLongitude, MsgZoom, MsgCenter
      , MsgStyle, MsgStyleStreets, MsgStyleOutdoors, MsgStyleLight, MsgStyleDark
      , MsgStyleSatellite, MsgStyleSatelliteStreets, MsgStyleNavigationDay
      , MsgStyleNavigationNight, MsgRecordEdited
      )
    )

import Material3 (md3selectWidget, md3widget)
import Model
    ( overpass, msgSuccess
    , MapboxParam
      ( MapboxParam, mapboxParamLon, mapboxParamLat, mapboxParamZoom
      , mapboxParamStyle
      )
    )

import Network.Wreq (post, FormParam ((:=)), responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Hamlet (Html)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , redirect, languages, getYesod, addStylesheetRemote, addScriptRemote
    , SomeMessage (SomeMessage), addMessageI
    )
import Yesod.Form.Fields (selectField, optionsPairs, doubleField, selectFieldList)
import Yesod.Form.Functions (mreq, runFormPost, generateFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


mapboxStyles :: [(AppMessage, Text)]
mapboxStyles = second ("mapbox://styles/mapbox/" <>) <$>
    [ (MsgStyleStreets, "streets-v12")
    , (MsgStyleOutdoors, "outdoors-v12")
    , (MsgStyleLight, "light-v11")
    , (MsgStyleDark, "dark-v11")
    , (MsgStyleSatellite, "satellite-v9")
    , (MsgStyleSatelliteStreets, "satellite-streets-v12")
    , (MsgStyleNavigationDay, "navigation-day-v1")
    , (MsgStyleNavigationNight, "navigation-night-v1")
    ]


mapboxStyle :: Text
mapboxStyle = snd (mapboxStyles !! 3)


postSettingsGeoBboxR :: Text -> Text -> Handler Html
postSettingsGeoBboxR country city = do

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
    idInputStyle <- newIdent

    params <- runDB $ selectOne $ from $ table @MapboxParam
            
    ((fr,fw),et) <- runFormPost $ formMapViewport idInputLon idInputLat idInputZoom idInputStyle (entityVal <$> params)

    case fr of
      FormSuccess param -> do
          runDB $ delete $ void $ from $ table @MapboxParam
          runDB $ insert_ param
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SettingsGeoBboxR country city
      
      _otherwise -> do    
          msgr <- getMessageRender

          let mapStyles = snd <$> mapboxStyles

          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              idMap <- newIdent

              mapboxPk <- appMapboxPk . appSettings <$> getYesod

              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

              $(widgetFile "data/settings/geo/bbox")


getSettingsGeoBboxR :: Text -> Text -> Handler Html
getSettingsGeoBboxR country city = do

    params <- runDB $ selectOne $ from $ table @MapboxParam

    param <- case params of
      Just (Entity _ p) -> return p
      
      Nothing -> do
          lang <- maybe "" (T.cons ':' . T.takeWhile (/= '-')) .  LS.head <$> languages

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

          return MapboxParam { mapboxParamLon = fst center
                             , mapboxParamLat = snd center
                             , mapboxParamZoom = 9
                             , mapboxParamStyle = "mapbox://styles/mapbox/dark-v11"
                             }

    idInputLon <- newIdent
    idInputLat <- newIdent
    idInputZoom <- newIdent
    idInputStyle <- newIdent
        
    (fw,et) <- generateFormPost $ formMapViewport idInputLon idInputLat idInputZoom idInputStyle (Just param)
    
    msgr <- getMessageRender

    let center = (mapboxParamLon param, mapboxParamLat param)
    let mapStyles = snd <$> mapboxStyles
    
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        idMap <- newIdent

        mapboxPk <- appMapboxPk . appSettings <$> getYesod

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
            
        $(widgetFile "data/settings/geo/bbox")


formMapViewport :: Text -> Text -> Text -> Text
                -> Maybe MapboxParam -> Form MapboxParam
formMapViewport idInputLon idInputLat idInputZoom idInputStyle params extra = do
    
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
            
    (styleR,styleV) <- mreq (selectFieldList mapboxStyles) FieldSettings
        { fsLabel = SomeMessage MsgStyle
        , fsId = Just idInputStyle, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (mapboxParamStyle <$> params)

    return ( MapboxParam <$> lonR <*> latR <*> zoomR <*> styleR
           , [whamlet|
                     ^{extra}
                     
                     <fieldset>
                       <legend>_{MsgCenter}
                       ^{md3widget lonV}
                       ^{md3widget latV}
                       
                     ^{md3widget zoomV}
                       
                     ^{md3selectWidget styleV}
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
          msgr <- getMessageRender
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
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
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/geo/city")
    

postSettingsR :: Handler Html
postSettingsR = do

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
          msgr <- getMessageRender
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              $(widgetFile "data/settings/settings")


formCity :: [Text] -> Form Text
formCity cities extra = do
    (cityR,cityV) <- mreq (selectField (optionsPairs ((\x -> (x,x)) <$> cities))) FieldSettings
        { fsLabel = SomeMessage MsgCity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    return (cityR, [whamlet|^{extra} ^{md3selectWidget cityV}|])


getSettingsR :: Handler Html
getSettingsR = do

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
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/settings")


formCountry :: [Text] -> Form Text
formCountry countries extra = do
    (countryR,countryV) <- mreq (selectField (optionsPairs ((\x -> (x,x)) <$> countries))) FieldSettings
        { fsLabel = SomeMessage MsgCountry
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    return (countryR, [whamlet|^{extra} ^{md3selectWidget countryV}|])
