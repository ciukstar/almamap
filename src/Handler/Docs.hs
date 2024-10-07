{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Docs
  ( getDocsR
  ) where

import Foundation
    ( Handler
    , widgetSnackbar, widgetTopbar
    , Route (StaticR)
    , AppMessage
      ( MsgAppName, MsgDocumentation, MsgEntityRelationshipDiagram
      , MsgBasicEntities, MsgTaskStatusStateTransition, MsgStateMachineDiagram
      )
    )
    
import Settings (widgetFile)


import Text.Hamlet (Html)

import Yesod
    ( getMessageRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDocumentation
        idOverlay <- newIdent
        $(widgetFile "docs/docs")
