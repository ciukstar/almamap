{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRu (fillDemoRu) where

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
    , Endpoint (Endpoint, endpointKey, endpointVal)
    , keyEndpointOverpass, overpass, keyEndpointNominatim, nominatim
    )
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRu = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    uid1 <- insert $ User { userEmail = "bulanovalm@mail.ru"
                          , userPassword = Just pass1
                          , userName = Just "Буланова Любовь Михайловна"
                          , userAdmin = True
                          }

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "petrovia"
    uid2 <- insert $ User { userEmail = "petrovia@mail.ru"
                          , userPassword = Just pass2
                          , userName = Just "Петров Иван Александрович"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "smirnovav"
    uid3 <- insert $ User { userEmail = "smirnovav@mail.ru"
                          , userPassword = Just pass3
                          , userName = Just "Смирнов Андрей Васильевич"
                          , userAdmin = False
                          }

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "sergeevaav"
    uid4 <- insert $ User { userEmail = "sergeevaav@mail.ru"
                          , userPassword = Just pass4
                          , userName = Just "Сергеева Александра Владимировна"
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

    insert_ Bbox { bboxMinLon = 36.25493725352848
                 , bboxMinLat = 55.0432521465201
                 , bboxMaxLon = 38.188841952029804
                 , bboxMaxLat = 56.000713384146735
                 }

    insert_ MapboxParam { mapboxParamCountry = "Россия"
                        , mapboxParamCity = "Москва"
                        , mapboxParamLon = 37.6174782
                        , mapboxParamLat = 55.7505412
                        , mapboxParamZoom = 9
                        }

    insert_ Endpoint { endpointKey = Model.keyEndpointOverpass
                     , endpointVal = Model.overpass
                     }

    insert_ Endpoint { endpointKey = Model.keyEndpointNominatim
                     , endpointVal = Model.nominatim
                     }

    return ()
