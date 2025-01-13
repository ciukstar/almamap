{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeApplications           #-}

module Model where

import ClassyPrelude.Yesod
    ( Typeable, mkMigrate, mkPersist, persistFileWith
    , share, sqlSettings
    )

import Data.Aeson
    ( Value, ToJSON, toJSON )
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq ((/=))
import Data.Fixed (Fixed (MkFixed))
import Data.Function ((.))
import Data.Maybe (Maybe (Just), maybe)
import Data.Text (Text)
import qualified Data.Text as T (cons, takeWhile)
import Data.Time.Clock
    ( NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)

import Database.Persist.Quasi ( lowerCaseSettings )

import GHC.Float (Double, int2Double, truncateDouble)
import GHC.Integer (Integer)
import GHC.Num ((*))
import GHC.Real ((/), (^))

import Prelude (truncate)

import Text.Hamlet (Html)

import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")



instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }


instance ToJSON Bbox where
    toJSON :: Bbox -> Value
    toJSON (Bbox minLon minLat maxLon maxLat) = toJSON ((minLon, minLat), (maxLon, maxLat))


overpass :: Text
overpass = "https://overpass-api.de/api/interpreter"

keyEndpointOverpass :: Text
keyEndpointOverpass = "overpass"

    
nominatim :: Text
nominatim = "https://nominatim.openstreetmap.org/search"


keyEndpointNominatim :: Text
keyEndpointNominatim = "nominatim"


mediae :: [(Text,Text)]
mediae = [("s","small"),("m","medium"),("l","large")] :: [(Text,Text)]

langs :: [(Text,Text)]
langs = [("en","EN"),("fr","FR"),("kk","KZ"),("ro","RO"),("ru","RU")]

msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"

keyThemeMode :: Text
keyThemeMode = "almamap_theme_mode"

paramTaskStatus :: Text
paramTaskStatus = "status"

paramUserId :: Text
paramUserId = "uid"

paramLang :: Text
paramLang = "lang"

paramBacklink :: Text
paramBacklink = "backlink"

keyThemeLight :: Text
keyThemeLight = "light"

keyThemeDark :: Text
keyThemeDark = "dark"

paramTheme :: Text
paramTheme = "theme"

eventChangeTheme :: Text
eventChangeTheme = "changetheme"

      
langSuffix :: Maybe Text -> Text
langSuffix = maybe "" (T.cons ':' . T.takeWhile (/= '-'))


nominalDiffTimeToHours :: NominalDiffTime -> Double
nominalDiffTimeToHours =
    (/ 3600.0) . int2Double . truncate . nominalDiffTimeToSeconds


hoursToNominalDiffTime :: Double -> NominalDiffTime
hoursToNominalDiffTime =
    secondsToNominalDiffTime . MkFixed . (* (^) @Integer @Integer 10 12) . truncateDouble @Integer . (* 3600)
