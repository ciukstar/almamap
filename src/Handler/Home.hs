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

import Control.Applicative ((<|>))
import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (Value (Object, String), decode, encode, toJSON, object, (.=))
import qualified Data.Aeson as A (Value)
import qualified Data.Aeson.KeyMap as AKM (lookup)
import Data.Aeson.Lens (key, AsValue (_String), nth)
import Data.Bifunctor (Bifunctor(second, first))
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.), Value (unValue)
    )
import Database.Persist (entityVal)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, widgetMainMenu, mapboxStyles
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
      , MsgGeolocationDisabled, MsgAddress, MsgLongitude, MsgLatitude
      , MsgAboutYourLocation, MsgGeolocationStatusDisabledExploreNearby
      , MsgGeolocationStatusUserMessage, MsgGeolocationAlternativesMessagePrefix
      , MsgGeolocationAlternativesMessageSuffix, MsgGeolocationAlternativesMessageAddress
      , MsgGeolocationAlternativesMessageOrThe, MsgGeolocationAlternativesMessageCoordinates
      , MsgGeolocationNotSupportedUserMessage, MsgGeolocationStatusTimeoutUserMessage
      , MsgGeolocationStatusDisabledUserMessage
      )
    )

import Model
    ( keyThemeLight, keyThemeDark, keyEndpointOverpass, keyEndpointNominatim
    , Bbox (bboxMinLon, bboxMinLat, bboxMaxLon, bboxMaxLat)
    , DefaultMapStyle (DefaultMapStyle)
    , MapboxParam (MapboxParam)
    , Endpoint, EntityField (EndpointKey, EndpointVal)
    )
import qualified Model (overpass, nominatim)

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile, AppSettings (appMapboxPk))
import Settings.StaticFiles
    ( img_shopping_cart_pin_720dp_png
    , img_account_balance_pin_720dp_png
    , img_restaurant_pin_720dp_png
    , img_park_pin_720dp_png
    , img_attractions_pin_720dp_png
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
    , newIdent, languages
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)
import Yesod.Persist.Core (YesodPersist(runDB))


styles :: [(Int,(AppMessage, (Text,(Text,Text))))]
styles = zip [1::Int ..] . (second (first ("mapbox://styles/mapbox/" <>)) <$>) $
        [ (MsgStyleDark, ("dark-v11",(keyThemeDark,"white")))
        , (MsgStyleLight, ("light-v11",(keyThemeLight,"black")))
        , (MsgStyleStreets, ("streets-v12",(keyThemeLight,"black")))
        , (MsgStyleOutdoors, ("outdoors-v12",(keyThemeLight,"black")))
        , (MsgStyleSatellite, ("satellite-v9",(keyThemeLight,"black")))
        , (MsgStyleSatelliteStreets, ("satellite-streets-v12",(keyThemeLight,"black")))
        , (MsgStyleNavigationDay, ("navigation-day-v1",(keyThemeLight,"black")))
        , (MsgStyleNavigationNight, ("navigation-night-v1",(keyThemeDark,"white")))
        ]


defaultChip :: Text
defaultChip = "attractions"


getHomeR :: Handler Html
getHomeR = do
    
    (style,textColor) <- fromMaybe ("","#000000")
        . ((<|> (\((_,c),s) -> (s,c)) <$> LS.head  mapboxStyles)
           . ((\(DefaultMapStyle s c) -> (s,c)) . entityVal <$>))
                     <$> runDB ( selectOne $ from $ table @DefaultMapStyle )

    langs <- languages
    let lang = fromMaybe "" (LS.head langs)

    overpass <- maybe Model.overpass unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointOverpass
        return $ x ^. EndpointVal )

    nominatim <- maybe Model.nominatim unValue <$> runDB ( selectOne $ do
        x <- from $ table @Endpoint
        where_ $ x ^. EndpointKey ==. val keyEndpointNominatim
        return $ x ^. EndpointVal )
        
    area <- (entityVal <$>) <$> runDB ( selectOne $ from $ table @MapboxParam )
    
    bbox <- (entityVal <$>) <$> runDB ( selectOne $ from $ table @Bbox)
    params <- runDB $ selectOne $ from $ table @MapboxParam
    let center = (\(MapboxParam _ _ lon lat _) -> (lon,lat)) . entityVal <$> params
    let zoom = (\(MapboxParam _ _ _ _ z) -> z) . entityVal <$> params
    
    mapboxPk <- appMapboxPk . appSettings <$> getYesod

    msgr <- getMessageRender

    let chips :: [(Text,((Text,Text),(Text,Text)))]
        chips = [ ( "attractions"
                  , ( ("attractions","icon_attractions")
                    , (msgr MsgAttractions, query area bbox "tourism" Nothing)
                    )
                  )
                , ( "park"
                  , (("park","icon_park"), (msgr MsgParks, query area bbox "leisure" (Just "park")))
                  )
                , ( "restaurant"
                  , ( ("restaurant","icon_restaurant")
                    , (msgr MsgRestaurants, query area bbox "amenity" (Just "restaurant"))
                    )
                  )
                , ( "government"
                  , ( ("account_balance","icon_government")
                    , (msgr MsgPublicInstitutions, query area bbox "government" Nothing)
                    )
                  )
                ]

    let nearbyItems :: [(Int, A.Value)]
        nearbyItems = zip [1..]
                      [ object [ "icon" .= String "icon_attractions"
                               , "label" .= msgr MsgAttractions
                               , "query" .= queryAround "[tourism]"
                               ]
                      , object [ "icon" .= String "icon_park"
                               , "label" .= msgr MsgParks
                               , "query" .= queryAround "[leisure=park]"
                               ]
                      , object [ "icon" .= String "icon_restaurant"
                               , "label" .= msgr MsgRestaurants
                               , "query" .= queryAround "[amenity=restaurant]"
                               ]
                      , object [ "icon" .= String "icon_government"
                               , "label" .= msgr MsgPublicInstitutions
                               , "query" .= queryAround "[government]"
                               ]
                      , object [ "icon" .= String "icon_shopping_cart"
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
        idButtonPois <- newIdent

        idDialogPois <- newIdent
        idButtonCloseDialogPois <- newIdent

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
        idDialogExploreNearbyContent <- newIdent
        idPromptPermissions <- newIdent
        idGeolocationStatusIcon <- newIdent
        idGeolocationStatusLabel <- newIdent
        idGeolocationStatusUserMessage <- newIdent
        idGeolocationAlternativesMessage <- newIdent
        idFormExploreNearby <- newIdent
        idSpanAddress <- newIdent
        idSpanCoordinates <- newIdent

        idCoordinates <- newIdent
        idFieldLongitude <- newIdent
        idInputLongitude <- newIdent
        idFieldLatitude <- newIdent
        idInputLatitude <- newIdent

        idFieldAddress <- newIdent
        idInputAddress <- newIdent
        idDatalistAddress <- newIdent

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


queryCount :: Maybe MapboxParam -> Maybe Bbox -> Text -> Text
queryCount area bbox attr = toStrict $ renderMarkup [shamlet|
    $maybe bbox <- bbox
      [bbox:#{bboxMinLat bbox},#{bboxMinLon bbox},#{bboxMaxLat bbox},#{bboxMaxLon bbox}]
    [out:json];

    $maybe MapboxParam country city _ _ _ <- area
      area["name"="#{country}"];
      area(area)[place="city"]["name"="#{city}"];
      node(area)#{attr}[~"^(name|description)$"~".*"];

    $nothing
      node#{attr}[~"^(name|description)$"~".*"];

    out count;
|]


queryAround :: Text -> Text
queryAround args =
            [st|
                [out:json];

                node#{args}(around:${this.myloc}) -> .tags;

                node.tags[~"^(name|description)$"~".*"];

                convert item ::=::,::geom=geom(),_osm_type=type();
    
                out geom;
            |]


query :: Maybe MapboxParam -> Maybe Bbox -> Text -> Maybe Text -> Text
query params bbox tag value = toStrict $ renderMarkup [shamlet|
    $maybe bbox <- bbox
      [bbox:#{bboxMinLat bbox},#{bboxMinLon bbox},#{bboxMaxLat bbox},#{bboxMaxLon bbox}]
    [out:json];
    
    $maybe MapboxParam country city _ _ _ <- params
      area["name"="#{country}"] -> .country;
      area["name"="#{city}"] -> .city;
      $maybe v <- value
        node["#{tag}"="#{v}"](area.country)(area.city) -> .tags;
      $nothing
        node["#{tag}"](area.country)(area.city) -> .tags;
        
    $nothing
      $maybe v <- value
        node["#{tag}"="#{v}"] -> .tags;
      $nothing
        node["#{tag}"] -> .tags;

    node.tags[~"^(name|description)$"~".*"];

    convert item ::=::,::geom=geom(),_osm_type=type();
    
    out geom;
|]
