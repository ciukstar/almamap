{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRo (fillDemoRo) where

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
      , mapboxParamCity, mapboxParamCountry
      )
    , Bbox (Bbox, bboxMinLon, bboxMinLat, bboxMaxLon, bboxMaxLat)
    , DefaultTheme (DefaultTheme, defaultThemeTheme)
    , DefaultMapStyle (DefaultMapStyle, defaultMapStyleStyle, defaultMapStyleTextColor)
    , Endpoint (Endpoint, endpointKey, endpointVal)
    , keyEndpointOverpass, overpass, keyEndpointNominatim, nominatim
    )

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRo :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRo = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "raduam"
    uid1 <- insert $ User { userEmail = "raduam@xmailx.ro"
                          , userPassword = Just pass1
                          , userName = Just "Radu Ana-Maria"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "ionescuav"
    uid2 <- insert $ User { userEmail = "ionescuav@xmailx.ro"
                          , userPassword = Just pass2
                          , userName = Just "Ionescu Alexandru Victor"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "rususa"
    uid3 <- insert $ User { userEmail = "rususa@xmailx.ro"
                          , userPassword = Just pass3
                          , userName = Just "Rusu Ştefan Alexandru"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "mateiaa@mail.ro"
    uid4 <- insert $ User { userEmail = "mateiaa@xmailx.ro"
                          , userPassword = Just pass4
                          , userName = Just "Matei Andreea Alexandra"
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

    insert_ Bbox { bboxMinLon = 25.86926392228915
                 , bboxMinLat = 44.28352846668534
                 , bboxMaxLon = 26.277837394104267
                 , bboxMaxLat = 44.55125966926411
                 }
    
    insert_ MapboxParam { mapboxParamCountry = "România"
                        , mapboxParamCity = "București"
                        , mapboxParamLon = 26.1027202
                        , mapboxParamLat = 44.4361414
                        , mapboxParamZoom = 9
                        }

    insert_ Endpoint { endpointKey = Model.keyEndpointOverpass
                     , endpointVal = Model.overpass
                     }

    insert_ Endpoint { endpointKey = Model.keyEndpointNominatim
                     , endpointVal = Model.nominatim
                     }
    
    return ()
