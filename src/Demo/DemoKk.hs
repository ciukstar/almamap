{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoKk (fillDemoKk) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( User (User, userEmail, userPassword, userAdmin, userName)
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , MapboxParam
      ( MapboxParam, mapboxParamLon, mapboxParamLat, mapboxParamZoom
      , mapboxParamCountry, mapboxParamCity
      )
    , Bbox (Bbox, bboxMinLon, bboxMinLat, bboxMaxLon, bboxMaxLat)
    , DefaultTheme (DefaultTheme, defaultThemeTheme)
    , DefaultMapStyle (DefaultMapStyle, defaultMapStyleStyle, defaultMapStyleTextColor)
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoKk :: MonadIO m => ReaderT SqlBackend m ()
fillDemoKk = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "erkejanb"
    uid1 <- insert $ User { userEmail = "erkejanb@xmail.kz"
                          , userPassword = Just pass1
                          , userName = Just "Еркежан Байбөрі"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "igisinz"
    uid2 <- insert $ User { userEmail = "igisinz@xmail.kz"
                          , userPassword = Just pass2
                          , userName = Just "Игісін Зікірия"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "narmaphabetn"
    uid3 <- insert $ User { userEmail = "narmaphabetn@xmail.kz"
                          , userPassword = Just pass3
                          , userName = Just "Нармағамбет Нысынбай"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "yriyay"
    uid4 <- insert $ User { userEmail = "yriyay@xmail.kz"
                          , userPassword = Just pass4
                          , userName = Just "Үрия Үстем"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    insert_ DefaultTheme { defaultThemeTheme = "dark" }

    insert_ DefaultMapStyle { defaultMapStyleStyle = "mapbox://styles/mapbox/dark-v11" 
                            , defaultMapStyleTextColor = "#FFFFFF"
                            }

    insert_ Bbox { bboxMinLon = 76.62533582272056
                 , bboxMinLat = 43.01706289821615
                 , bboxMaxLon = 77.25985266747443
                 , bboxMaxLat = 43.42148919080813
                 }

    insert_ MapboxParam { mapboxParamCountry = "Kazakhstan"
                        , mapboxParamCity = "Almaty"
                        , mapboxParamLon = -0.12
                        , mapboxParamLat = 51.50
                        , mapboxParamZoom = 9
                        }
    
    return ()
