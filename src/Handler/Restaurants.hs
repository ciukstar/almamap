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
    ( FromJSON (parseJSON), (.:), (.:?)
    , withObject
    )
import qualified Data.Aeson as A (Value) 
import Data.Aeson.Lens (key, AsValue (_String), nth)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack, intercalate)
import Data.Text as T (null)

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

import Text.Hamlet (Html)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (intField)


getRestaurantR :: Handler Html
getRestaurantR = do

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
        
        $(widgetFile "restaurants/restaurant")


getRestaurantsR :: Handler Html
getRestaurantsR = do

    let queryDefault :: Text
        queryDefault = [st|
                            [out:json];

                            rel["ISO3166-2"="KZ-75"] -> .rel;
                            .rel map_to_area -> .city;

                            node["amenity"="restaurant"]["name"](area.city);

                            out body;
                        |]

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRestaurants
        
        idFieldSearch <- newIdent
        idInputSearch <- newIdent
        idListSearchResults <- newIdent

        idDialogDetails <- newIdent
        idDialogDetailsTitle <- newIdent
        idDialogDetailsContent <- newIdent
        idMap <- newIdent 
        idButttonCloseDialogDetails <- newIdent
        
        $(widgetFile "restaurants/restaurants")


instance FromJSON Restaurant where
    parseJSON :: A.Value -> Parser Restaurant
    parseJSON = withObject "element" $ \e -> do
        t <- e .: "tags"
        Restaurant <$> e .: "id"
            <*> e .: "lat"
            <*> e .: "lon"
            <*> t .: "name"
            <*> t .:? "description"
            <*> t .:? "cuisine"
            <*> t .:? "opening_hours"
            <*> ( (\a b c -> intercalate ", " (catMaybes [a,b,c]))
                  <$> t .:? "addr:street"
                  <*> t .:? "addr:housenumber"
                  <*> t .:? "addr:city"
                )
            <*> (t .:? "phone" <|> t .:? "contact:phone") 


data Restaurant = Restaurant
    { id :: Int
    , lat :: Double
    , lon :: Double
    , name :: Text
    , descr :: Maybe Text
    , cuisine :: Maybe Text
    , openingHours :: Maybe Text
    , addr :: Text
    , phone :: Maybe Text
    }
