{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  , getFetchR
  ) where

import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson.Lens (key, AsValue (_String), nth)
import Data.Text (unpack)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), selectOne, from, table, countRows, select
    , (^.), (==.), (:&)((:&))
    , innerJoin, on, where_, val, just, valList
    )

import Foundation
    ( App (appSettings), Handler, widgetSnackbar
    , Route(StaticR, FetchR)
    , AppMessage
      ( MsgHome, MsgClose, MsgCouldNotGetPosition
      )
    )

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_attractions_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_park_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_restaurant_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_balance_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_assistant_navigation_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, TypedContent, selectRep, provideJson, getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)


getHomeR :: Handler Html
getHomeR = do

    mapboxPk <- appMapboxPk . appSettings <$> getYesod

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome
        
        idMap <- newIdent
        idControlButtons <- newIdent
        idButtonZoomIn <- newIdent
        idButtonZoomOut <- newIdent
        idButtonMyLocation <- newIdent
        idButtonSwitch <- newIdent
        idDialogOverview <- newIdent
        idDialogOverviewTitle <- newIdent
        idDialogOverviewContent <- newIdent
        idButttonCloseDialog <- newIdent

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
        
        $(widgetFile "homepage")


getFetchR :: Handler TypedContent
getFetchR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)
    
    selectRep $ do
        provideJson $
            r L.^? WL.responseBody . key "claims" . key "P18"
            . nth 0 . key "mainsnak" . key "datavalue" . key "value"
            . _String
