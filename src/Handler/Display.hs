{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Display
  ( getDisplayR, postDisplayR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Database.Persist (Entity (entityVal), insert_)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, delete)

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar, mapboxStyles
    , Route (DataR)
    , DataR (BboxR, SettingsGeoCountryR, DisplayR, EndpointsR)
    , AppMessage
      ( MsgSettings, MsgBbox, MsgGeoRegion, MsgDisplay, MsgEndpoints
      , MsgDefaultTheme, MsgDefaultMapStyle, MsgSave, MsgTheme, MsgThemeDark
      , MsgThemeLight, MsgStyle, MsgRecordEdited, MsgTextColor
      )
    )
    
import Material3 (md3selectWidget, md3radioField)
import Model
    ( msgSuccess
    , DefaultTheme (DefaultTheme, defaultThemeTheme)
    , DefaultMapStyle (DefaultMapStyle, defaultMapStyleStyle, defaultMapStyleTextColor)
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
import Yesod.Form.Fields (selectField, optionsPairs, colorField)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , FieldView (fvInput, fvId, fvLabel), FormResult (FormSuccess)
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

    let styleOptions = optionsPairs $ (\((m,_),s) -> (m,s)) <$> mapboxStyles

    (styleR,styleV) <- mreq (selectField styleOptions) FieldSettings
        { fsLabel = SomeMessage MsgStyle
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (defaultMapStyleStyle . entityVal <$> style)

    (colorR,colorV) <- mreq colorField FieldSettings
        { fsLabel = SomeMessage MsgTextColor
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } ((defaultMapStyleTextColor . entityVal <$> style) <|> Just "#808080")
        
    let r = ((,) . DefaultTheme <$> themeR) <*> (DefaultMapStyle <$> styleR <*> colorR)
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
                      
                      <button.transparent.border>
                        <i>palette
                        <span>#{fvLabel colorV}
                        ^{fvInput colorV}
                    |]
    return (r,w)
