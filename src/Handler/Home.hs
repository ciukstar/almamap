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
import Data.Text (Text, unpack)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), selectOne, from, table, countRows, select
    , (^.), (==.), (:&)((:&))
    , innerJoin, on, where_, val, just
    )

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, widgetMainMenu
    , Route(StaticR, FetchR)
    , AppMessage
      ( MsgClose, MsgCouldNotGetPosition, MsgAppName, MsgStyleStreets
      , MsgStyleOutdoors, MsgStyleLight, MsgStyleDark, MsgStyleSatellite
      , MsgStyleSatelliteStreets, MsgStyleNavigationDay, MsgStyleNavigationNight
      , MsgCancel, MsgSearch, MsgRestaurants, MsgShops
      )
    )

import Model (keyThemeLight, keyThemeDark)

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_attractions_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_park_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_restaurant_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_balance_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_assistant_navigation_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_compass_needle_svg
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)
import Data.Bifunctor (Bifunctor(second, first))


getHomeR :: Handler Html
getHomeR = do

    mapboxPk <- appMapboxPk . appSettings <$> getYesod

    
    let styles :: [(Int,(AppMessage, (Text,Text)))]
        styles = zip [1::Int ..] . (second (first ("mapbox://styles/mapbox/" <>)) <$>) $
            [ (MsgStyleStreets, ("streets-v12",keyThemeLight))
            , (MsgStyleOutdoors, ("outdoors-v12",keyThemeLight))
            , (MsgStyleLight, ("light-v11",keyThemeLight))
            , (MsgStyleDark, ("dark-v11",keyThemeDark))
            , (MsgStyleSatellite, ("satellite-v9",keyThemeLight))
            , (MsgStyleSatelliteStreets, ("satellite-streets-v12",keyThemeLight))
            , (MsgStyleNavigationDay, ("navigation-day-v1",keyThemeLight))
            , (MsgStyleNavigationNight, ("navigation-night-v1",keyThemeDark))
            ]

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName
        
        idOverlay <- newIdent
        idMap <- newIdent
        idControlsTopLeft <- newIdent
        idButtonLayers <- newIdent
        idMenuLayers <- newIdent
        idControlButtons <- newIdent
        idButtonZoomIn <- newIdent
        idButtonZoomOut <- newIdent
        idButtonMyLocation <- newIdent
        idButtonSwitch <- newIdent

        idButtonSearchTrigger <- newIdent
        idDialogSearch <- newIdent
        idButtonCloseSearchDialog <- newIdent

        idButtonSearchByCategoryTrigger <- newIdent
        idDialogSearchByCategory <- newIdent
        idButtonCloseSearchByCategoryDialog <- newIdent
        
        
        idButtonMainMenu <- newIdent
        idDialogMainMenu <- newIdent
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
