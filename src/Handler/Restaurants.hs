{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Restaurants
  ( getRestaurantsR
  , getRestaurantR
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
    ( App (appSettings), Handler, widgetSnackbar, widgetMainMenu, widgetTopbar
    , Route(ShopsR, RestaurantR, RestaurantsR, HomeR, StaticR, FetchR, FetchP18PhotoR)
    , AppMessage
      ( MsgClose, MsgCouldNotGetPosition, MsgAppName, MsgStyleStreets
      , MsgStyleOutdoors, MsgStyleLight, MsgStyleDark, MsgStyleSatellite
      , MsgStyleSatelliteStreets, MsgStyleNavigationDay, MsgStyleNavigationNight
      , MsgRestaurants, MsgShops, MsgNoLocationsWereFound, MsgRestaurant
      , MsgSearchByNameOrAddress, MsgSearchForRestaurants, MsgSearchForShops
      , MsgCuisine, MsgDescription, MsgAddress, MsgOpeningHours, MsgPhone, MsgShowOnMap
      )
    )

import Model (overpass, keyThemeLight, keyThemeDark)

import Network.Wreq (post, FormParam((:=)))
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_attractions_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_park_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_restaurant_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_balance_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_assistant_navigation_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_compass_needle_svg
    )

import Text.Hamlet (Html, shamlet)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent, provideRep, MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields (intField, textField)
import Text.Blaze.Renderer.Text (renderMarkup)


getRestaurantR :: Handler Html
getRestaurantR = undefined {-- do

    rid <- runInputGet $ ireq intField "rid" :: Handler Int
    
    let query :: Text
        query = [st|
                   [out:json];

                   rel["ISO3166-2"="KZ-75"] -> .rel;
                   .rel map_to_area -> .city;

                   node(#{rid})(area.city);

                   out body;
                   |]

    r <- liftIO $ post (unpack overpass) ["data" := query]

    let restaurant = parseMaybe parseJSON =<< (r L.^? WL.responseBody . key "elements" . nth 0)
          

    liftIO $ print $ r L.^? WL.responseBody
            
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRestaurant
        
        $(widgetFile "restaurants/restaurant") --}


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
            idMap <- newIdent 
            idButttonCloseDialogDetails <- newIdent
            idButttonCancelDialogDetails <- newIdent

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
