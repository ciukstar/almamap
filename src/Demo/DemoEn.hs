{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

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


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "marylopez"
    uid1 <- insert $ User { userEmail = "marylopez@xmail.edu"
                          , userPassword = Just pass1
                          , userName = Just "Mary Lopez"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "jjohnson"
    uid2 <- insert $ User { userEmail = "jjohnson@xmail.edu"
                          , userPassword = Just pass2
                          , userName = Just "John Johnson"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "jmaulsby"
    uid3 <- insert $ User { userEmail = "jmaulsby@xmail.edu"
                          , userPassword = Just pass3
                          , userName = Just "Julian Maulsby"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "vschoen"
    uid4 <- insert $ User { userEmail = "vschoen@xmail.edu"
                          , userPassword = Just pass4
                          , userName = Just "Valentina Schoen"
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

    insert_ Bbox { bboxMinLon = -0.2739597071966955
                 , bboxMinLat = 51.411529657088664
                 , bboxMaxLon = 0.006976602593198322
                 , bboxMaxLat = 51.56453199636596
                 }
    
    insert_ MapboxParam { mapboxParamCountry = "United Kingdom"
                        , mapboxParamCity = "London"
                        , mapboxParamLon = -0.12
                        , mapboxParamLat = 51.50
                        , mapboxParamZoom = 9
                        }

    insert_ Endpoint { endpointKey = Model.keyEndpointOverpass
                     , endpointVal = Model.overpass
                     }

    insert_ Endpoint { endpointKey = Model.keyEndpointNominatim
                     , endpointVal = Model.nominatim
                     }
    
    return ()
