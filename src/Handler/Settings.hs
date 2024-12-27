{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Settings
  ( getSettingsR, postSettingsR
  , getSettingsGeoCityR, postSettingsGeoCityR
  , getSettingsGeoBboxR
  ) where

import Control.Lens ((^..), Each(each))
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (key, AsValue (_Array, _String))
import Data.List (sort)
import qualified Data.List.Safe as LS (head)
import Data.Text (Text, unpack)
import qualified Data.Text as T

import Foundation
    ( Handler, Form, App (appSettings), widgetTopbar
    , Route (DataR)
    , DataR (SettingsR, SettingsGeoCityR, SettingsGeoBboxR)
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay, MsgCountry, MsgCity, MsgRegion
      , MsgNext, MsgSave
      )
    )

import Material3 (md3selectWidget)
import Model (overpass)

import Network.Wreq (post, FormParam ((:=)), responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Hamlet (Html)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , redirect, languages, getYesod, addStylesheetRemote, addScriptRemote
    , SomeMessage (SomeMessage)
    )
import Yesod.Form.Fields (selectField, optionsPairs)
import Yesod.Form.Functions (mreq, runFormPost, generateFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess)
    )


center :: (Double, Double)
center = (76.9406462, 43.2239423)

style :: Text
style = "mapbox://styles/mapbox/dark-v11"


getSettingsGeoBboxR :: Text -> Text -> Handler Html
getSettingsGeoBboxR country city = do
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        idMap <- newIdent

        mapboxPk <- appMapboxPk . appSettings <$> getYesod

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
            
        $(widgetFile "data/settings/geo/bbox")


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
