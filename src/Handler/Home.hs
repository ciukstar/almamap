{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  , getFetchR
  , getFetchP18PhotoR
  ) where

import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (decode, encode, toJSON, object, (.=), Value (Object, String))
import qualified Data.Aeson as A (Value)
import qualified Data.Aeson.KeyMap as AKM (lookup)
import Data.Aeson.Lens (key, AsValue (_String), nth)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, widgetMainMenu
    , Route
      ( ShopsR, RestaurantsR, StaticR, FetchR, FetchP18PhotoR
      )
    , AppMessage
      ( MsgClose, MsgCouldNotGetPosition, MsgAppName, MsgStyleStreets
      , MsgStyleOutdoors, MsgStyleLight, MsgStyleDark, MsgStyleSatellite
      , MsgStyleSatelliteStreets, MsgStyleNavigationDay, MsgStyleNavigationNight
      , MsgRestaurants, MsgShops, MsgNoLocationsWereFound, MsgExploreNearby
      , MsgSearchByNameOrAddress, MsgZoomIn, MsgZoomOut, MsgMyLocation, MsgParks
      , MsgMainMenu, MsgCompass, MsgMapStyleOptions, MsgRestaurantsShopsAndMore
      , MsgAttractions, MsgRadius, MsgInKilometers, MsgNearby, MsgFind
      , MsgPublicInstitutions
      )
    )

import Model (keyThemeLight, keyThemeDark, overpass)

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_attractions_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_park_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_restaurant_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_account_balance_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_assistant_navigation_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_shopping_cart_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_compass_needle_svg
    )

import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Hamlet (Html, shamlet)
import Text.Julius (rawJS)
import Text.Shakespeare.Text (st)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, addStylesheetRemote
    , addScriptRemote, getYesod, selectRep, provideJson, getMessageRender
    , newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)


center :: (Double, Double)
center = (76.9406462, 43.2239423)

style :: Text
style = "mapbox://styles/mapbox/dark-v11"


getHomeR :: Handler Html
getHomeR = do

    mapboxPk <- appMapboxPk . appSettings <$> getYesod

    let styles :: [(Int,(AppMessage, (Text,Text)))]
        styles = zip [1::Int ..] . (second (first ("mapbox://styles/mapbox/" <>)) <$>) $
            [ (MsgStyleStreets, ("streets-v12",keyThemeLight))
            , (MsgStyleOutdoors, ("outdoors-v12",keyThemeLight))
            , (MsgStyleLight, ("light-v11",keyThemeLight))
            , (MsgStyleDark, ("dark-v11",keyThemeDark))
            , (MsgStyleSatellite, ("satellite-v9",keyThemeLight))
            , (MsgStyleSatelliteStreets, ("satellite-streets-v12",keyThemeLight))
            , (MsgStyleNavigationDay, ("navigation-day-v1",keyThemeLight))
            , (MsgStyleNavigationNight, ("navigation-night-v1",keyThemeDark))
            ]

    msgr <- getMessageRender

    let nearbyItems :: [A.Value]
        nearbyItems = [ object [ "marker" .= String "marker-tourism"
                               , "label" .= msgr MsgAttractions
                               , "query" .= queryAround "[tourism]"
                               ]
                      , object [ "marker" .= String "marker-parks"
                               , "label" .= msgr MsgParks
                               , "query" .= queryAround "[leisure=park]"
                               ]
                      , object [ "marker" .= String "marker-restaurant"
                               , "label" .= msgr MsgRestaurants
                               , "query" .= queryAround "[amenity=restaurant]"
                               ]
                      , object [ "marker" .= String "marker-government"
                               , "label" .= msgr MsgPublicInstitutions
                               , "query" .= queryAround "[government]"
                               ]
                      , object [ "marker" .= String "marker-shop"
                               , "label" .= msgr MsgShops
                               , "query" .= queryAround "[shop]"
                               ]
                      ]

    msgs <- getMessages

    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        idMap <- newIdent
        idControlsTopLeft <- newIdent
        idButtonLayers <- newIdent
        idMenuLayers <- newIdent
        idControlButtons <- newIdent
        idButtonExploreNearby <- newIdent
        idButtonZoomIn <- newIdent
        idButtonZoomOut <- newIdent
        idButtonMyLocation <- newIdent
        idButtonSwitch <- newIdent

        idButtonSearchTrigger <- newIdent
        idDialogSearch <- newIdent
        idButtonCloseSearchDialog <- newIdent
        idListSearchResults <- newIdent
        idInputSearch <- newIdent

        idButtonSearchByCategoryTrigger <- newIdent
        idDialogSearchByCategory <- newIdent
        idButtonCloseSearchByCategoryDialog <- newIdent
        idLabelRestaurants <- newIdent
        idLabelShops <- newIdent

        idButtonMainMenu <- newIdent
        idDialogMainMenu <- newIdent
        idDialogOverview <- newIdent
        idDialogOverviewTitle <- newIdent
        idDialogOverviewContent <- newIdent
        idButttonCloseDialogOverview <- newIdent

        idDialogExploreNearby <- newIdent
        idButttonCloseDialogExploreNearby <- newIdent
        idFormExploreNearby <- newIdent
        idFieldRadius <- newIdent
        idInputRadius <- newIdent
        idButttonCancelDialogExploreNearby <- newIdent
        idButttonExploreNearby <- newIdent

        addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.css"
        addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v3.7.0/mapbox-gl.js"

        $(widgetFile "homepage")


getFetchR :: Handler TypedContent
getFetchR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)

    selectRep $ do
        provideJson (decode =<< (r L.^? WL.responseBody) :: Maybe A.Value)



getFetchP18PhotoR :: Handler TypedContent
getFetchP18PhotoR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)

    selectRep $ do
        provideJson $
            r L.^? WL.responseBody . key "claims" . key "P18"
            . nth 0 . key "mainsnak" . key "datavalue" . key "value"
            . _String


queryTagCount :: Text -> Text
queryTagCount tag = [st|
    [out:json];

    rel["ISO3166-2"="KZ-75"] -> .rel;
    .rel map_to_area -> .city;

    node["#{tag}"](area.city);

    out count;
|]


queryAmenityCount :: Text -> Text
queryAmenityCount typ = [st|
    [out:json];

    rel["ISO3166-2"="KZ-75"] -> .rel;
    .rel map_to_area -> .city;

    node["amenity"="#{typ}"](area.city);

    out count;
|]


queryAround :: Text -> Text
queryAround args =
            [st|
                [out:json];

                node#{args}(around:${this.myloc}) -> .tags;

                node.tags[~"^(name|description)$"~".*"];

                out body;
            |]


query :: Text -> Maybe Text -> Text
query tag val = toStrict $ renderMarkup [shamlet|
    [out:json];

    rel["ISO3166-2"="KZ-75"] -> .rel;
    .rel map_to_area -> .city;

    $maybe v <- val
      node["#{tag}"="#{v}"](area.city) -> .tags;
    $nothing
      node["#{tag}"](area.city) -> .tags;

    node.tags[~"^(name|description)$"~".*"];

    out body;
|]


countrycodes :: Text
countrycodes = "kz"


bbox :: Text
bbox = "76.738277,43.032844,77.166754,43.403766"


nominatim :: Text
nominatim = "https://nominatim.openstreetmap.org/search"
