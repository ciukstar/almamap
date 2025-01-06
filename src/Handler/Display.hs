{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Display
  ( getDisplayR, postDisplayR
  ) where

import Control.Monad (void)

import Data.Bifunctor (Bifunctor(second))
import Database.Persist (Entity (entityVal), insert_)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, delete)


import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR)
    , DataR (BboxR, SettingsR, DisplayR)
    , AppMessage
      ( MsgSettings, MsgBoundingBox, MsgGeoRegion, MsgDisplay
      , MsgDefaultTheme, MsgDefaultMapStyle, MsgSave, MsgTheme, MsgThemeDark
      , MsgThemeLight, MsgStyleStreets, MsgStyleOutdoors, MsgStyleLight
      , MsgStyleDark, MsgStyleSatellite, MsgStyleSatelliteStreets
      , MsgStyleNavigationDay, MsgStyleNavigationNight, MsgStyle, MsgRecordEdited
      )
    )
    
import Material3 (md3selectWidget, md3radioField)
import Model
    ( DefaultTheme (DefaultTheme, defaultThemeTheme)
    , DefaultMapStyle (DefaultMapStyle, defaultMapStyleStyle), msgSuccess
    )

import Settings (widgetFile)

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (julius)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessageRender, newIdent, getMessages
    , SomeMessage (SomeMessage), redirect, addMessageI
    )
import Yesod.Core.Widget (whamlet, toWidget)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields (selectField, optionsPairs)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FieldView (fvInput, fvId), FormResult (FormSuccess)
    )


postDisplayR :: Handler Html
postDisplayR = do

    theme <- runDB $ selectOne $ from $ table @DefaultTheme
    style <- runDB $ selectOne $ from $ table @DefaultMapStyle
    
    ((fr,fw),et) <- runFormPost $ formDisplay theme style
    case fr of
      FormSuccess (t,s) -> do
          runDB $ delete $ void $ from $ table @DefaultTheme
          runDB $ insert_ t
          runDB $ delete $ void $ from $ table @DefaultMapStyle
          runDB $ insert_ s
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR DisplayR
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              $(widgetFile "data/settings/display/display")


getDisplayR :: Handler Html
getDisplayR = do

    theme <- runDB $ selectOne $ from $ table @DefaultTheme
    style <- runDB $ selectOne $ from $ table @DefaultMapStyle
    
    (fw,et) <- generateFormPost $ formDisplay theme style
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSettings
        idOverlay <- newIdent
        $(widgetFile "data/settings/display/display")


formDisplay :: Maybe (Entity DefaultTheme) -> Maybe (Entity DefaultMapStyle)
            -> Form (DefaultTheme, DefaultMapStyle)
formDisplay theme style extra = do

    let themeOptions = optionsPairs [(MsgThemeDark, "dark"),(MsgThemeLight, "light")]
    
    (themeR,themeV) <- mreq (md3radioField themeOptions) FieldSettings
        { fsLabel = SomeMessage MsgTheme
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (defaultThemeTheme . entityVal <$> theme)

    let styleOptions = optionsPairs $ second ("mapbox://styles/mapbox/" <>) <$>
            [ (MsgStyleStreets, "streets-v12")
            , (MsgStyleOutdoors, "outdoors-v12")
            , (MsgStyleLight, "light-v11")
            , (MsgStyleDark, "dark-v11")
            , (MsgStyleSatellite, "satellite-v9")
            , (MsgStyleSatelliteStreets, "satellite-streets-v12")
            , (MsgStyleNavigationDay, "navigation-day-v1")
            , (MsgStyleNavigationNight, "navigation-night-v1")
            ]

    (styleR,styleV) <- mreq (selectField styleOptions) FieldSettings
        { fsLabel = SomeMessage MsgStyle
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (defaultMapStyleStyle . entityVal <$> style)
        
    let r = ((,) . DefaultTheme <$> themeR) <*> (DefaultMapStyle <$> styleR)
    let w = do
            toWidget [cassius|
                             ##{fvId themeV}
                                 display: flex
                                 flex-direction: column
                                 gap: 1rem
                             |]
            toWidget [julius|
                            Array.from(
                              document.getElementById(#{fvId themeV}).querySelectorAll('input[type=radio]')
                            ).forEach(x => {
                              x.addEventListener('click',function (e) {
                                ui('mode', e.target.value == '1' ? 'dark' : 'light');
                              });
                            });
                            |]
            [whamlet|
                    #{extra}

                    <fieldset>
                      <legend>_{MsgDefaultTheme}
                      ^{fvInput themeV}

                    <fieldset>
                      <legend>_{MsgDefaultMapStyle}
                      ^{md3selectWidget styleV}
                    |]
    return (r,w)
