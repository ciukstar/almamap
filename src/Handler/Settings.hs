{-# LANGUAGE TemplateHaskell #-}

module Handler.Settings (getSettingsR) where

import Foundation
    ( Handler, widgetTopbar
    , Route (DataR)
    , DataR (SettingsR)
    , AppMessage
      ( MsgSettings, MsgGeoRegion, MsgDisplay
      )
    )
import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender
    )


getSettingsR :: Handler Html
getSettingsR = do
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/settings")
