{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Settings (getSettingsR) where

import Control.Lens ((^..), Each(each))
import Control.Monad.IO.Class (liftIO)

import Data.List (sort)
import Data.Aeson.Lens (key, AsValue (_Array, _String))
import Data.Text (Text)

import Foundation
    ( Handler, Form, widgetTopbar
    , Route (DataR)
    , DataR (SettingsR)
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay, MsgCountry
      , MsgCity, MsgRegion, MsgNext
      )
    )

import Material3 (md3selectWidget)

import Network.Wreq (post, FormParam ((:=)), responseBody)

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, whamlet
    , SomeMessage (SomeMessage)
    )
import Yesod.Form.Fields (selectField, optionsPairs)
import Yesod.Form.Functions (generateFormGet', mreq)
import Yesod.Form.Types (FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs))


getSettingsGeoR :: Text -> Handler Html
getSettingsGeoR = undefined


getSettingsR :: Handler Html
getSettingsR = do

    r <- liftIO $ post "https://overpass-api.de/api/interpreter"
        [ "data" := [st|
                       [out:json];
                       rel["admin_level"="2"][boundary=administrative]["name:en"];
                       out tags;
                       |]
        ]

    let countries = sort $ r ^.. responseBody . key "elements" . _Array . each . key "tags" . key "name:en" . _String
    
    (fw,et) <- generateFormGet' $ formCountries countries
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/settings")


formCountries :: [Text] -> Form Text
formCountries countries extra = do
    (countryR,countryV) <- mreq (selectField (optionsPairs ((\x -> (x,x)) <$> countries))) FieldSettings
        { fsLabel = SomeMessage MsgCountry
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    return (countryR, [whamlet|^{extra} ^{md3selectWidget countryV}|])
