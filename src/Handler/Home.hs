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
    , innerJoin, on, where_, val, just, valList, in_, groupBy
    )

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, widgetTopbar
    , Route(AuthR, HomeR, DataR, StaticR, FetchR)
    , AppMessage
      ( MsgHome, MsgClose
      )
    )

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_attractions_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, TypedContent, selectRep, provideJson
    )
import Yesod.Core.Handler
    ( getMessageRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)




getHomeR :: Handler Html
getHomeR = do

    mapboxPk <- appMapboxPk . appSettings <$> getYesod
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome
        
        idMap <- newIdent
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
