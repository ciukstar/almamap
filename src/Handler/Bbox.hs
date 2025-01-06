{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Bbox
  ( getBboxR, postBboxR
  ) where

import Control.Monad (void)

import Data.Aeson (ToJSON (toJSON))
import Data.Bifunctor (Bifunctor(second))
import Data.Text (Text)

import Database.Esqueleto.Experimental (selectOne, from, table, delete)
import Database.Persist (Entity (Entity), insert_)

import Foundation
    ( Handler, Form, App (appSettings), widgetTopbar, widgetSnackbar
    , Route (DataR)
    , DataR (BboxR, SettingsR, DisplayR)
    , AppMessage
      ( MsgBoundingBox, MsgDisplay, MsgSettings, MsgGeoRegion, MsgStyleStreets
      , MsgStyleOutdoors, MsgStyleLight, MsgStyleDark, MsgStyleSatellite
      , MsgStyleSatelliteStreets, MsgStyleNavigationDay, MsgStyleNavigationNight
      , MsgSave, MsgWest, MsgSouth, MsgNorth, MsgEast, MsgSouthWest, MsgNorthEast
      , MsgRecordEdited
      )
    )
    
import Material3 (md3widget)

import Model (Bbox(Bbox, bboxMinLon, bboxMinLat, bboxMaxLon, bboxMaxLat), msgSuccess, defaultBbox)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), setTitleI, newIdent, getYesod
    , addStylesheetRemote, addScriptRemote, getMessageRender, addMessageI
    , redirect, getMessages
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (doubleField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
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


postBboxR :: Handler Html
postBboxR = do
    
    bbox <- do
        bbox <- runDB $ selectOne $ from $ table @Bbox
        case bbox of
          Just (Entity _ (Bbox minLon minLat maxLon maxLat)) -> return ((minLon,minLat),(maxLon,maxLat))
          Nothing -> return ( (bboxMinLon defaultBbox, bboxMinLat defaultBbox)
                            , (bboxMaxLon defaultBbox, bboxMaxLat defaultBbox)
                            )

    idInputMinLon <- newIdent
    idInputMinLat <- newIdent
    idInputMaxLon <- newIdent
    idInputMaxLat <- newIdent

    ((fr,fw),et) <- runFormPost $ formBbox idInputMinLon idInputMinLat idInputMaxLon idInputMaxLat (Just bbox)

    case fr of
      FormSuccess r -> do
          runDB $ delete $ void $ from $ table @Bbox
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR BboxR
      
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgBoundingBox
              idOverlay <- newIdent
              idMapboxMap <- newIdent

              mapboxPk <- appMapboxPk . appSettings <$> getYesod

              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

              $(widgetFile "data/settings/bbox/bbox")


getBboxR :: Handler Html
getBboxR = do
    
    bbox <- do
        bbox <- runDB $ selectOne $ from $ table @Bbox
        case bbox of
          Just (Entity _ (Bbox minLon minLat maxLon maxLat)) -> return ((minLon,minLat),(maxLon,maxLat))
          Nothing -> return ( (bboxMinLon defaultBbox, bboxMinLat defaultBbox)
                            , (bboxMaxLon defaultBbox, bboxMaxLat defaultBbox)
                            )

    idInputMinLon <- newIdent
    idInputMinLat <- newIdent
    idInputMaxLon <- newIdent
    idInputMaxLat <- newIdent

    (fw,et) <- generateFormPost $ formBbox idInputMinLon idInputMinLat idInputMaxLon idInputMaxLat (Just bbox)
        
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBoundingBox
        idOverlay <- newIdent
        idMapboxMap <- newIdent

        mapboxPk <- appMapboxPk . appSettings <$> getYesod

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
        
        $(widgetFile "data/settings/bbox/bbox")


formBbox :: Text -> Text -> Text -> Text
         -> Maybe ((Double, Double),(Double, Double)) -> Form Bbox
formBbox idMinLon idMinLat idMaxLon idMaxLat bbox extra = do
    
    (minLonR,minLonV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgWest
        , fsId = Just idMinLon, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (fst . fst <$> bbox)
    
    (minLatR,minLatV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgSouth
        , fsId = Just idMinLat, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (snd . fst <$> bbox)
    
    (maxLonR,maxLonV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgEast
        , fsId = Just idMaxLon, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (fst . snd <$> bbox)
    
    (maxLatR,maxLatV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgNorth
        , fsId = Just idMaxLat, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (snd . snd <$> bbox)

    let r = Bbox <$> minLonR <*> minLatR <*> maxLonR <*> maxLatR
    let w = [whamlet|
                    ^{extra}
                    
                    <fieldset>
                      <legend>_{MsgSouthWest}
                      ^{md3widget minLonV}
                      ^{md3widget minLatV}
                      
                    <fieldset>
                      <legend>_{MsgNorthEast}
                      ^{md3widget maxLonV}
                      ^{md3widget maxLatV}
                    |]

    return (r,w)
