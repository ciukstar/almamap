{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Endpoints
  ( getEndpointsR, postEndpointsR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Database.Persist (Entity (entityVal), insert_)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, delete, val, where_
    , (^.), (==.)
    )

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR)
    , DataR (BboxR, SettingsGeoCountryR, DisplayR, EndpointsR)
    , AppMessage
      ( MsgSettings, MsgBbox, MsgGeoRegion, MsgEndpoints, MsgDisplay
      , MsgSave, MsgNominatim, MsgRecordEdited, MsgOverpass
      )
    )
    
import Material3 (md3widget)
import Model
    ( msgSuccess, keyEndpointOverpass, keyEndpointNominatim
    , Endpoint (Endpoint, endpointVal)
    , EntityField (EndpointKey)
    )
import qualified Model (overpass, nominatim)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessageRender, newIdent, getMessages
    , SomeMessage (SomeMessage), redirect, addMessageI
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (textField)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FormResult (FormSuccess)
    )


postEndpointsR :: Handler Html
postEndpointsR = do

    overpass <- runDB $ selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return x
        
    nominatim <- runDB $ selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointNominatim
        return x
    
    ((fr,fw),et) <- runFormPost $ formEndpoints overpass nominatim
    case fr of
      FormSuccess (o,n) -> do
          runDB $ delete $ void $ from $ table @Endpoint
          runDB $ insert_ o
          runDB $ insert_ n
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR EndpointsR
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              $(widgetFile "data/settings/endpoints/endpoints")


getEndpointsR :: Handler Html
getEndpointsR = do

    overpass <- runDB $ selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return x
        
    nominatim <- runDB $ selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointNominatim
        return x
    
    (fw,et) <- generateFormPost $ formEndpoints overpass nominatim
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/endpoints/endpoints")


formEndpoints :: Maybe (Entity Endpoint) -> Maybe (Entity Endpoint) -> Form (Endpoint, Endpoint)
formEndpoints overpass nominatim extra = do
    
    (overpassR,overpassV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgOverpass
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (endpointVal . entityVal <$> overpass <|> Just Model.overpass)
    
    (nominatimR,nominatimV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgNominatim
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (endpointVal . entityVal <$> nominatim <|> Just Model.nominatim)
        
    let r = ((,) . Endpoint keyEndpointOverpass <$> overpassR) <*> (Endpoint keyEndpointNominatim <$> nominatimR)
    let w = [whamlet|
                    #{extra}
                    ^{md3widget overpassV}
                    ^{md3widget nominatimV}
                    |]
    return (r,w)
