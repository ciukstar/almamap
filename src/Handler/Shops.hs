{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Shops (getShopsR) where

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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S (Set, fromList, size)
import Data.Text (Text, unpack)
import Data.Text as T (splitOn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Database.Esqueleto.Experimental (selectOne, from, table)
import Database.Persist (Entity(Entity), entityVal)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, mapboxStyles
    , Route(ShopsR, HomeR)
    , AppMessage
      ( MsgClose, MsgSearchForShops, MsgDescription, MsgAddress, MsgType
      , MsgOpeningHours, MsgPhone, MsgShowOnMap, MsgBrand, MsgShops
      , MsgLoadMore, MsgNoShopsWereFoundForSearchTerms, MsgNoShops
      )
    )

import Model
    ( overpass, defaultBbox
    , Bbox (bboxMinLat, bboxMinLon, bboxMaxLat, bboxMaxLon)
    , DefaultMapStyle (defaultMapStyleStyle)
    )

import Network.Wreq (post, FormParam((:=)))
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))

import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Hamlet (shamlet)
import Text.Julius (rawJS)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent, provideRep
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (textField, intField)
import Yesod.Persist.Core (YesodPersist(runDB))


page :: Int
page = 100


getShopsR :: Handler TypedContent
getShopsR = do 
    
    style <- fromMaybe "" . ((<|> snd <$> LS.head  mapboxStyles) . (defaultMapStyleStyle . entityVal <$>))
                   <$> runDB ( selectOne $ from $ table @DefaultMapStyle )
    
    offset <- fromMaybe @Int 0 <$> runInputGet (iopt intField "offset")
    q <- runInputGet $ iopt textField "q"
    typ <- runInputGet $ iopt textField "type"
    brand <- runInputGet $ iopt textField "brand"

    bbox <- do
        bbox <- runDB $ selectOne $ from $ table @Bbox
        case bbox of
          Just (Entity _ b) -> return b
          Nothing -> return defaultBbox
    
    let query = renderMarkup
            [shamlet|
                [bbox:#{bboxMinLat bbox},#{bboxMinLon bbox},#{bboxMaxLat bbox},#{bboxMaxLon bbox}]
                [out:json];

                node["shop"]["name"] -> ._;
                $maybe x <- q
                  node._["name"~"#{x}",i] -> ._;
                $maybe x <- typ
                  node._["shop"="#{x}"] -> ._;
                $maybe x <- brand
                  node._["brand"="#{x}"] -> ._;

                ._ out body;
            |]

    r <- liftIO $ post (unpack overpass) ["data" := query]
            
    let allShops :: [Shop]
        allShops = sortBy (\a b -> compare (shopName a) (shopName b)) . fromMaybe []
            $ parseMaybe parseJSON =<< (r L.^? WL.responseBody . key "elements")

    let types :: S.Set Text
        types = S.fromList $ concat $ mapMaybe shopType allShops

    let brands :: S.Set Text
        brands = S.fromList $ mapMaybe shopBrand allShops

    let shops = (\s -> (icon s, label s, s)) <$> take page (drop offset allShops)
            
    msgr <- getMessageRender
    msgs <- getMessages
    
    selectRep $ do
            
        provideRep $ defaultLayout $ do
            setTitleI MsgShops

            idFormSearch <- newIdent
            idFieldSearch <- newIdent
            idInputSearch <- newIdent
            idButtonSearch <- newIdent

            idMainSection <- newIdent
            idSearchResultsArea <- newIdent
            idListSearchResults <- newIdent
            idDivLoadMore <- newIdent

            idDetailsType <- newIdent
            idDetailsBrand <- newIdent

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

            $(widgetFile "shops/shops")
            
        provideJson shops

  where

      label :: Shop -> Maybe Text
      label (Shop _ _ _ _ descr _ _ _ _ openingHours addr _) =
          (descr <|> addr) <|> openingHours
      
      icon :: Shop -> Text      
      icon (Shop _ _ _ _ _ (Just ["electronics"]) _ _ _ _ _ _) = "cable"
      icon (Shop _ _ _ _ _ (Just ["alcohol"]) _ _ _ _ _ _) = "liquor"
      icon (Shop _ _ _ _ _ (Just ["sports"]) _ _ _ _ _ _) = "sports_tennis"
      icon (Shop _ _ _ _ _ (Just ["books"]) _ _ _ _ _ _) = "auto_stories"
      icon (Shop _ _ _ _ _ (Just ["bicycle"]) _ _ _ _ _ _) = "pedal_bike"
      icon (Shop _ _ _ _ _ (Just ["beverages"]) _ _ _ _ _ _) = "emoji_food_beverage"
      icon (Shop _ _ _ _ _ (Just ["bed"]) _ _ _ _ _ _) = "bed"
      icon (Shop _ _ _ _ _ (Just ["beauty"]) _ _ _ _ _ _) = "health_and_beauty"
      icon (Shop _ _ _ _ _ (Just ["bakery"]) _ _ _ _ _ _) = "bakery_dining"
      icon (Shop _ _ _ _ _ (Just ["baby_goods"]) _ _ _ _ _ _) = "bedroom_baby"
      icon (Shop _ _ _ _ _ (Just ["appliance"]) _ _ _ _ _ _) = "iron"
      icon (Shop _ _ _ _ _ (Just ["anime"]) _ _ _ _ _ _) = "comic_bubble"
      icon (Shop _ _ _ _ _ (Just ["computer"]) _ _ _ _ _ _) = "computer"
      icon (Shop _ _ _ _ _ (Just ["hairdresser"]) _ _ _ _ _ _) = "styler"
      icon (Shop _ _ _ _ _ (Just ["fuel"]) _ _ _ _ _ _) = "local_gas_station"
      icon (Shop _ _ _ _ _ _ _(Just "fuel") _ _ _ _) = "local_gas_station"
      icon _ = "shopping_cart"


instance ToJSON Shop where
    toJSON :: Shop -> A.Value
    toJSON (Shop rid lat lon name descr typ brand amenity cuisine openingHours addr phone) =
        object [ "id" .= rid
               , "lat" .= lat
               , "lon" .= lon
               , "name" .= name
               , "descr" .= descr
               , "type" .= typ
               , "brand" .= brand
               , "amenity" .= amenity
               , "cuisine" .= cuisine
               , "openingHours" .= openingHours
               , "addr" .= addr
               , "phone" .= phone
               ]


instance FromJSON Shop where
    parseJSON :: A.Value -> Parser Shop
    parseJSON = withObject "element" $ \e -> do
        t <- e .: "tags"
        Shop <$> e .: "id"
            <*> e .: "lat"
            <*> e .: "lon"
            <*> t .: "name"
            <*> t .:? "description"
            <*> ( (splitOn ";" <$>) <$> (t .:? "shop") )
            <*> t .:? "brand"
            <*> t .:? "amenity"
            <*> ( (splitOn ";" <$>) <$> (t .:? "cuisine") )
            <*> t .:? "opening_hours"
            <*> ( (\a b c -> joinMaybeText (joinMaybeText a b) c)
                  <$> t .:? "addr:street"
                  <*> t .:? "addr:housenumber"
                  <*> t .:? "addr:city"
                )
            <*> (t .:? "phone" <|> t .:? "contact:phone") 


data Shop = Shop
    { shopId :: Int
    , shopLat :: Double
    , shopLon :: Double
    , shopName :: Text
    , shopDescr :: Maybe Text
    , shopType :: Maybe [Text]
    , shopBrand :: Maybe Text
    , shopAmenity :: Maybe Text
    , shopCuisine :: Maybe [Text]
    , shopOpeningHours :: Maybe Text
    , shopAddr :: Maybe Text
    , shopPhone :: Maybe Text
    } deriving Show


joinMaybeText :: Maybe Text -> Maybe Text -> Maybe Text
joinMaybeText (Just x) (Just y) = Just (x <> ", " <> y)
joinMaybeText (Just x) Nothing = Just x
joinMaybeText Nothing (Just y) = Just y
joinMaybeText Nothing Nothing = Nothing
