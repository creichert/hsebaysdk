{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Ebay
        ( defaultEbayConfig
        , withSearchRequest
        , Search (..)
        , SearchRequest (..)
        , SearchResponse (..)
        , SearchResult (..)
        , Item (..)
        , SortOrder (..)
        , FindVerb (..)
        , EbayConfig (..)
        , SellingState (..)
        , ItemFilter(..)
        ) where

import           Control.Applicative          (pure, (<$>), (<*>))
import           Control.Monad                (mzero)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson                   as A
import           Data.Aeson.Types             (Parser)
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Conduit                 (($$+-))
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.HashMap.Strict          as HM
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           GHC.Generics                 (Generic)
import           Network.HTTP.Conduit         (Manager, Request (..),
                                               RequestBody (..), http, parseUrl,
                                               responseBody)
import           Network.HTTP.Types           as HTTP (Header)


-- Ebay api configuration.
--  * TODO: qualify names
data EbayConfig = EbayConfig
                { domain           :: Text
                , uri              :: Text
                , https            :: Bool
             -- , warnings :: Bool
             -- , errors  :: Bool
                , siteid           :: Text
                , responseEncoding :: Encoding
                , requestEncoding  :: Encoding
             -- , proxy_host :: Text
             -- , proxy_port :: Text
             -- , token :: Text
             -- , iaf_token :: Text
                , appid            :: Text
                , version          :: Text
                , service          :: Text
                , docUrl           :: Text
                , debug            :: Bool
                } deriving Show

-- | Supported response encoding
data Encoding = XmlEncoding
              | JsonEncoding
              deriving Show

-- | Request for ebay finding api
data SearchRequest = SearchRequest
                 { verb    :: FindVerb
                 , payload :: Search
                 } deriving Show

instance ToJSON SearchRequest where
    toJSON SearchRequest{..} = object [ "jsonns.xsi" .= xsi
                                      , "jsonns.xs"  .= xs
                                      , "jsonns.tns" .= nstns
                                      , tns .= payload
                                      ]
      where tns   = "tns." <> findVerbToTns verb
            xsi   = "http://www.w3.org/2001/XMLSchema-instance" :: Text
            xs    = "http://www.w3.org/2001/XMLSchema" :: Text
            nstns = "http://www.ebay.com/marketplace/search/v1/services" :: Text

newtype ItemFilter = ItemFilter (Text, Text) deriving Show

instance ToJSON ItemFilter where
    toJSON (ItemFilter (name, val)) = object [ "name"  .= name
                                             , "value" .= val
                                             ]

data Condition = New | Used
               deriving (Generic, Show)

instance ToJSON Condition
instance FromJSON Condition

-- | Generic search query for ebay api.
data Search = Search
            { searchKeywords   :: Text
            -- , searchAffiliateInfo :: AffiliateInfo
            , searchSortOrder  :: Maybe SortOrder
            , searchItemFilter :: [ItemFilter]
            } deriving Show

instance ToJSON Search where
    toJSON Search{..} = object $ [ "keywords"  .= searchKeywords
                                 -- , "affiliate" .=
                                 -- , "paginationInput"
                                 ] ++ order searchSortOrder
                                   ++ ifilter searchItemFilter
      where order (Just so) = ["sortOrder" .= so]
            order Nothing = []
            -- item filter field should not be
            -- added if the list is empty.
            ifilter [] = []
            ifilter sif = [ "itemFilter" .=  sif ]

data SortOrder = EndTimeSoonest
               | BestMatch
               | BidCountFewest
               | BidCountMost
               | CountryAscending
               | CountryDescending
               | CurrentPriceHighest
               | DistanceNearest
               | PricePlusShipingHighest
               | PricePlusShippingLowest
               | StartTimeNewest
               deriving (Show,  Generic)

instance ToJSON SortOrder

-- parse a json field and take the head of the list.
--
-- The eBay api wraps nearly every field in Array,
-- even if it's only a single element.
--
-- TODO: Parser (Maybe a) return type
infixl 4 .:>
(.:>) :: FromJSON a => A.Object -> Text -> Parser a
(.:>) obj key = head <$> obj .: key

-- | Represents a Response from the eBay finding api
--
-- TODO: Search responses technically have a list of SearchResult
data SearchResponse = SearchResponse FindVerb SearchResult
                    deriving Show

-- use GADTs to get rid of this.
instance FromJSON SearchResponse where
    parseJSON (Object o) = case HM.keys o of
        ("findCompletedItemsResponse":_) -> do
            obj <- o .:> "findCompletedItemsResponse"
            SearchResponse <$> pure FindCompletedItems
                           <*> parseJSON obj
        ("findItemsByKeywordsResponse":_) -> do
            obj <- o .:> "findItemsByKeywordsResponse"
            SearchResponse <$> pure FindItemsByKeywords
                           <*> parseJSON obj
        ("findItemsAdvancedResponse":_) -> do
            obj <- o .:> "findItemsAdvancedResponse"
            SearchResponse <$> pure FindItemsAdvanced
                           <*> parseJSON obj
        ("errorMessage":_) -> error $ "An error occurred: " ++ show o
        _ -> error $ "Not implemented: " ++ take 1000 (show o)
    parseJSON _ = mzero

data SearchResult = SearchResult
                  { searchResultCount :: Text
                  , searchResultItems :: [Item]
                  } deriving Show

instance FromJSON SearchResult where
    parseJSON (Object o) = do
        sr <-  o .:> "searchResult"
        SearchResult <$> sr .: "@count"
                     <*> sr .: "item"
    parseJSON _ = mzero

instance ToJSON SearchResult where
    toJSON SearchResult{..} = object [ "count" .= searchResultCount
                                     , "items" .= searchResultItems
                                     ]

-- | A single product item
--
-- This items is not a direct translation of the
-- eBay type yet.
data Item = Item
          { itemId            :: Text
          , itemTitle         :: Text
          , itemLink          :: Text
          , itemImageUrl      :: Text
       -- , itemSold :: Bool
       -- , itemHasBids :: Bool
          , itemCurrentPrice  :: Double
          , itemBuyItNowPrice :: Maybe Text
       -- , itemCondition :: Text
          , itemSellingState  :: SellingState
          } deriving Show

instance ToJSON Item where
    toJSON Item{..} = object [ "id"        .= itemId
                             , "itemTitle" .= itemTitle
                             , "itemLink"  .= itemLink
                             ]

-- TODO: Cleanup parsing
instance FromJSON Item where
    parseJSON (Object o) =
        Item <$>  (o .:> "itemId")
             <*>  (o .:> "title")
             <*>  (o .:> "viewItemURL")
             <*>  (o .:> "galleryURL")
             <*> ((o .:> "sellingStatus")
                     >>= (.:> "convertedCurrentPrice")
                     >>= (.: "__value__")
                     -- a hacky way to convert to double.
                     >>= return . read . T.unpack
                     )
             <*> (head <$> (o .: "sellingStatus")
                     >>= (.:? "buyItNowPrice")
                     >>= (\no -> case no of
                              Nothing -> return Nothing
                              Just no' -> Just <$> (no' .: "__value__")))
             <*> ((o .:> "sellingStatus")
                     >>= (.:> "sellingState"))
    parseJSON _ = mzero

data SellingState = Active
                  | Canceled
                  | Ended
                  | EndedWithSales
                  | EndedWithoutSales
                  deriving (Eq, Generic, Show)

instance FromJSON SellingState

-- | Support verbs in finding api
--
-- 'verb' jargon taken from eBay docs.
data FindVerb = FindCompletedItems -- * Retrieves items whose listings
                                   --   are completed and are no longer
                                   --   available for sale on eBay.
              | FindItemsAdvanced -- * Finds items by a keyword query
                                  --   and/or category and allows
                                  --    searching within item descriptions.
              | FindItemsByImage
              | FindItemsByKeywords
              | FindItemsByProduct
              | FindItemsIneBayStores
              | GetHistograms
              | GetSearchKeywordsRecommendation
              | GetVersion
              deriving Show

-----------------------------------------------------------------------

withSearchRequest :: (MonadResource m, MonadIO m)
                  => SearchRequest
                  -> EbayConfig
                  -> Manager
                  -- TODO: Use Either Text SearchResponse
                  -> (Maybe SearchResponse -> m b) -> m b
withSearchRequest (SearchRequest cmd search) cfg manager f =
    runCommand cmd search cfg manager >>= f

runCommand :: (MonadIO m, MonadResource m)
           => FindVerb
           -> Search
           -> EbayConfig
           -> Manager
           -> m (Maybe SearchResponse)
runCommand cmd search cfg@EbayConfig{..} manager = do

    let proto = if https then "https://" else "http://"
    initreq <- parseUrl $ T.unpack $ proto <> domain <> uri

    let verb    = cmd
        payload = search
        er = SearchRequest { .. }

    let req = initreq
            { method = "POST"
            , redirectCount = 0
            , checkStatus = \_ _ _ -> Nothing
            , requestHeaders = requestHeaders initreq
                            ++ requestHeadersFromConfig cmd cfg
            , requestBody = RequestBodyBS $ L.toStrict $ A.encode er
            }

    res2 <- http req manager
    res <- responseBody res2 $$+- sinkLbs
    return (A.decode res :: Maybe SearchResponse)

defaultEbayConfig :: EbayConfig
defaultEbayConfig = EbayConfig
    { domain = "svcs.ebay.com"
    , uri = "/services/search/FindingService/v1"
    , siteid = "EBAY-US"
    , responseEncoding = JsonEncoding
    , requestEncoding = JsonEncoding
    , appid = ""
    , version = "1.12.0"
    , service = "FindingService"
    , docUrl = "http://developer.ebay.com/DevZone/finding/CallRef/index.html"
    , debug = False
    , https = False
    }

requestHeadersFromConfig :: FindVerb -> EbayConfig -> [HTTP.Header]
requestHeadersFromConfig fb EbayConfig{..} =
    [ ("Content-Type", "application/json")
    , ("X-EBAY-SOA-SERVICE-NAME", utf8 service)
    , ("X-EBAY-SOA-SERVICE-VERSION", utf8 version)
    , ("X-EBAY-SOA-SECURITY-APPNAME", utf8 appid)
    , ("X-EBAY-SOA-GLOBAL-ID", utf8 siteid)
    , ("X-EBAY-SOA-OPERATION-NAME", T.encodeUtf8 (findVerbToOperation fb))
    , ("X-EBAY-SOA-REQUEST-DATA-FORMAT",  dataFormatEncode requestEncoding)
    , ("X-EBAY-SOA-RESPONSE-DATA-FORMAT", dataFormatEncode responseEncoding)
    ]
  where utf8 = T.encodeUtf8
        dataFormatEncode JsonEncoding = utf8 "JSON"
        dataFormatEncode XmlEncoding  = utf8 "XML"

findVerbToOperation :: FindVerb -> Text
findVerbToOperation fb = case fb of
    FindItemsByKeywords -> "findItemsByKeywords"
    FindCompletedItems  -> "findCompletedItems"
    FindItemsAdvanced   -> "findItemsAdvanced"
    _ -> error "Error"

findVerbToTns :: FindVerb -> Text
findVerbToTns fb = case fb of
    FindItemsByKeywords -> "findItemsByKeywordsRequest"
    FindCompletedItems  -> "findCompletedItemsRequest"
    FindItemsAdvanced   -> "findItemsAdvancedRequest"
    _ -> error "Error"
