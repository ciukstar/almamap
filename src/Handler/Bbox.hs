{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Bbox
  ( getBboxR, postBboxR
  , postBboxDeleR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Data.Aeson (ToJSON (toJSON))
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Text (Text)

import Database.Esqueleto.Experimental (selectOne, from, table, delete)
import Database.Persist (Entity, entityVal, insert_)

import Foundation
    ( Handler, Form, App (appSettings), widgetTopbar, widgetSnackbar, mapboxStyles
    , Route (DataR)
    , DataR (BboxR, SettingsGeoCountryR, DisplayR, BboxDeleR)
    , AppMessage
      ( MsgBoundingBox, MsgDisplay, MsgSettings, MsgGeoRegion, MsgRecordEdited
      , MsgSave, MsgWest, MsgSouth, MsgNorth, MsgEast, MsgSouthWest, MsgNorthEast
      , MsgDele, MsgConfirmPlease, MsgDeleteAreYouSure, MsgCancel, MsgInvalidFormData
      , MsgRecordDeleted, MsgAdd, MsgNoBoundingBoxSetYet
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Bbox(Bbox, bboxMinLon, bboxMinLat, bboxMaxLon, bboxMaxLat)
    , DefaultMapStyle (defaultMapStyleStyle), MapboxParam (MapboxParam)
    )

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


postBboxDeleR :: Handler Html
postBboxDeleR = do
    ((fr,_),_) <- runFormPost formBboxDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ void $ from $ table @Bbox
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR BboxR
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR BboxR


postBboxR :: Handler Html
postBboxR = do
    
    mapboxStyle <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )
    
    bbox <- runDB $ selectOne $ from $ table @Bbox
    params <- runDB $ selectOne $ from $ table @MapboxParam
    let center = (\(MapboxParam _ _ lon lat _) -> (lon,lat)) . entityVal <$> params
    let zoom = (\(MapboxParam _ _ _ _ z) -> z) . entityVal <$> params

    idInputMinLon <- newIdent
    idInputMinLat <- newIdent
    idInputMaxLon <- newIdent
    idInputMaxLat <- newIdent

    ((fr,fw),et) <- runFormPost $ formBbox idInputMinLon idInputMinLat idInputMaxLon idInputMaxLat bbox
    (fw0,et0) <- generateFormPost formBboxDelete

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
              idBboxFormWrapper <- newIdent
              idFigureShowBboxForm <- newIdent
              idButtonShowBboxForm <- newIdent
              idButtonShowDialogDelete <- newIdent
              idDialogDelete <- newIdent

              mapboxPk <- appMapboxPk . appSettings <$> getYesod

              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

              $(widgetFile "data/settings/bbox/bbox")


getBboxR :: Handler Html
getBboxR = do
    
    mapboxStyle <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )

    bbox <- runDB $ selectOne $ from $ table @Bbox
    params <- runDB $ selectOne $ from $ table @MapboxParam
    let center = (\(MapboxParam _ _ lon lat _) -> (lon,lat)) . entityVal <$> params
    let zoom = (\(MapboxParam _ _ _ _ z) -> z) . entityVal <$> params

    idInputMinLon <- newIdent
    idInputMinLat <- newIdent
    idInputMaxLon <- newIdent
    idInputMaxLat <- newIdent

    (fw,et) <- generateFormPost $ formBbox idInputMinLon idInputMinLat idInputMaxLon idInputMaxLat bbox
    (fw0,et0) <- generateFormPost formBboxDelete
        
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBoundingBox
        idOverlay <- newIdent
        idMapboxMap <- newIdent
        idButtonShowBboxForm <- newIdent
        idBboxFormWrapper <- newIdent
        idFigureShowBboxForm <- newIdent
        idButtonShowDialogDelete <- newIdent
        idDialogDelete <- newIdent

        mapboxPk <- appMapboxPk . appSettings <$> getYesod

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"
        
        $(widgetFile "data/settings/bbox/bbox")


formBbox :: Text -> Text -> Text -> Text
         -> Maybe (Entity Bbox) -> Form Bbox
formBbox idMinLon idMinLat idMaxLon idMaxLat bbox extra = do
    
    (minLonR,minLonV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgWest
        , fsId = Just idMinLon, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (bboxMinLon . entityVal <$> bbox)
    
    (minLatR,minLatV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgSouth
        , fsId = Just idMinLat, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (bboxMinLat . entityVal <$> bbox)
    
    (maxLonR,maxLonV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgEast
        , fsId = Just idMaxLon, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (bboxMaxLon . entityVal <$> bbox)
    
    (maxLatR,maxLatV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgNorth
        , fsId = Just idMaxLat, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (bboxMaxLat . entityVal <$> bbox)

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


formBboxDelete :: Form ()
formBboxDelete extra = return (pure (), [whamlet|#{extra}|])
