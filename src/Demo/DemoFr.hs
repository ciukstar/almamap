{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoFr (fillDemoFr) where

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


fillDemoFr :: MonadIO m => ReaderT SqlBackend m ()
fillDemoFr = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "bernardj"
    uid1 <- insert $ User { userEmail = "bernardj@xmailx.fr"
                          , userPassword = Just pass1
                          , userName = Just "Bernard Jade"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "thomasgr"
    uid2 <- insert $ User { userEmail = "thomasgr@xmailx.fr"
                          , userPassword = Just pass2
                          , userName = Just "Thomas Gabriel Raphaël"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "richardal"
    uid3 <- insert $ User { userEmail = "richardal@xmailx.fr"
                          , userPassword = Just pass3
                          , userName = Just "Richard Arthur Louis"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "duboisaa"
    uid4 <- insert $ User { userEmail = "duboisaa@xmailx.fr"
                          , userPassword = Just pass4
                          , userName = Just "Dubois Alice Ambre"
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

    insert_ Bbox { bboxMinLon = 2.215564530851225
                 , bboxMinLat = 48.773829267918444
                 , bboxMaxLon = 2.469044413794336
                 , bboxMaxLat = 48.926853772696916
                 }
    
    insert_ MapboxParam { mapboxParamCountry = "France"
                        , mapboxParamCity = "Paris"
                        , mapboxParamLon = 2.3483915
                        , mapboxParamLat = 48.8534951
                        , mapboxParamZoom = 9
                        }

    insert_ Endpoint { endpointKey = Model.keyEndpointOverpass
                     , endpointVal = Model.overpass
                     }

    insert_ Endpoint { endpointKey = Model.keyEndpointNominatim
                     , endpointVal = Model.nominatim
                     }
    
    return ()
