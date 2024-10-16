{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Restaurants
  ( getRestaurantsR
  ) where

import Control.Applicative ((<|>))
import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
    ( FromJSON (parseJSON), ToJSON (toJSON), withObject, object, encode
    , (.:), (.:?), (.=)
    )
import qualified Data.Aeson as A (Value) 
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.List (sortBy)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S (Set, fromList, size)
import Data.Text (Text, unpack, intercalate)
import Data.Text as T (splitOn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar
    , Route(RestaurantsR, HomeR)
    , AppMessage
      ( MsgClose, MsgRestaurants, MsgSearchForRestaurants, MsgCuisine
      , MsgDescription, MsgAddress, MsgOpeningHours, MsgPhone, MsgShowOnMap
      )
    )

import Model (overpass)

import Network.Wreq (post, FormParam((:=)))
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Hamlet (shamlet)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent, provideRep
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (textField)
import Text.Blaze.Renderer.Text (renderMarkup)


getRestaurantsR :: Handler TypedContent
getRestaurantsR = do

    q <- runInputGet $ iopt textField "q"
    cuisine <- runInputGet $ iopt textField "cuisine"
    
    let query = renderMarkup
            [shamlet|
                [out:json];

                rel["ISO3166-2"="KZ-75"] -> .rel;
                .rel map_to_area -> .city;

                node["amenity"="restaurant"]["name"](area.city) -> ._;
                $maybe x <- q
                  node._["name"~"#{x}",i] -> ._;
                $maybe x <- cuisine
                  node._["cuisine"~"#{x}",i] -> ._;

                ._ out body;
            |]

    liftIO $ print query
            
    msgr <- getMessageRender
    msgs <- getMessages

    r <- liftIO $ post (unpack overpass) ["data" := query]
            
    let restaurants :: [Restaurant]
        restaurants = sortBy (\a b -> compare (restaurantName a) (restaurantName b)) . fromMaybe []
            $ parseMaybe parseJSON =<< (r L.^? WL.responseBody . key "elements")

    let cuisines :: S.Set Text
        cuisines = S.fromList $ concat $ mapMaybe restaurantCuisine restaurants

    selectRep $ do
            
        provideRep $ defaultLayout $ do
            setTitleI MsgRestaurants

            idFormSearch <- newIdent
            idFieldSearch <- newIdent
            idInputSearch <- newIdent
            idListSearchResults <- newIdent

            idMainSection <- newIdent

            idDetailsCuisine <- newIdent

            idDialogDetails <- newIdent
            idDialogDetailsTitle <- newIdent
            idDialogDetailsContent <- newIdent
            idDetailsMap <- newIdent
            idMap <- newIdent 
            idButttonCloseDialogDetails <- newIdent
            idButttonCancelDialogDetails <- newIdent

            mapboxPk <- appMapboxPk . appSettings <$> getYesod

            addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
            addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

            $(widgetFile "restaurants/restaurants")
            
        provideJson restaurants


instance ToJSON Restaurant where
    toJSON :: Restaurant -> A.Value
    toJSON (Restaurant rid lat lon name descr cuisine openingHours addr phone) =
        object [ "id" .= rid
               , "lat" .= lat
               , "lon" .= lon
               , "name" .= name
               , "descr" .= descr
               , "cuisine" .= cuisine
               , "openingHours" .= openingHours
               , "addr" .= addr
               , "phone" .= phone
               ]


instance FromJSON Restaurant where
    parseJSON :: A.Value -> Parser Restaurant
    parseJSON = withObject "element" $ \e -> do
        t <- e .: "tags"
        Restaurant <$> e .: "id"
            <*> e .: "lat"
            <*> e .: "lon"
            <*> t .: "name"
            <*> t .:? "description"
            <*> ( (splitOn ";" <$>) <$> (t .:? "cuisine"))
            <*> t .:? "opening_hours"
            <*> ( (\a b c -> intercalate ", " (catMaybes [a,b,c]))
                  <$> t .:? "addr:street"
                  <*> t .:? "addr:housenumber"
                  <*> t .:? "addr:city"
                )
            <*> (t .:? "phone" <|> t .:? "contact:phone") 


data Restaurant = Restaurant
    { restaurantId :: Int
    , restaurantLat :: Double
    , restaurantLon :: Double
    , restaurantName :: Text
    , restaurantDescr :: Maybe Text
    , restaurantCuisine :: Maybe [Text]
    , restaurantOpeningHours :: Maybe Text
    , restaurantAddr :: Text
    , restaurantPhone :: Maybe Text
    } deriving Show
