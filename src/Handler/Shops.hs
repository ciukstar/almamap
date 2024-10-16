{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Handler.Shops
  ( getShopsR
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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S (Set, fromList, size)
import Data.Text (Text, unpack)
import Data.Text as T (splitOn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar
    , Route(ShopsR, HomeR)
    , AppMessage
      ( MsgClose, MsgSearchForShops, MsgCuisine, MsgDescription, MsgAddress
      , MsgOpeningHours, MsgPhone, MsgShowOnMap, MsgBrand, MsgShops, MsgType
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


getShopsR :: Handler TypedContent
getShopsR = do

    q <- runInputGet $ iopt textField "q"
    typ <- runInputGet $ iopt textField "type"
    brand <- runInputGet $ iopt textField "brand"
    
    let query = renderMarkup
            [shamlet|
                [out:json];

                rel["ISO3166-2"="KZ-75"] -> .rel;
                .rel map_to_area -> .city;

                node["shop"]["name"](area.city) -> ._;
                $maybe x <- q
                  node._["name"~"#{x}",i] -> ._;
                $maybe x <- typ
                  node._["shop"="#{x}"] -> ._;
                $maybe x <- brand
                  node._["brand"="#{x}"] -> ._;

                ._ out body;
            |]

    liftIO $ print query
            
    msgr <- getMessageRender
    msgs <- getMessages

    r <- liftIO $ post (unpack overpass) ["data" := query]
            
    let shops :: [Shop]
        shops = sortBy (\a b -> compare (shopName a) (shopName b)) . fromMaybe []
            $ parseMaybe parseJSON =<< (r L.^? WL.responseBody . key "elements")

    let types :: S.Set Text
        types = S.fromList $ mapMaybe shopType shops

    let brands :: S.Set Text
        brands = S.fromList $ mapMaybe shopBrand shops

    selectRep $ do
            
        provideRep $ defaultLayout $ do
            setTitleI MsgShops

            idFormSearch <- newIdent
            idFieldSearch <- newIdent
            idInputSearch <- newIdent
            idListSearchResults <- newIdent

            idMainSection <- newIdent

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
            <*> t .:? "shop"
            <*> t .:? "brand"
            <*> t .:? "amenity"
            <*> ( (splitOn ";" <$>) <$> (t .:? "cuisine"))
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
    , shopType :: Maybe Text
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
