-- | Haskell SDK for Ebay Finding API

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
    , SearchItem (..)
    , SellingStatus (..)
    , SortOrder (..)
    , FindVerb (..)
    , EbayConfig (..)
    , SellingState (..)
    , ItemFilter (..)
    , Condition (..)
    , ListingInfo (..)
    , OutputSelector (..)
    , GalleryInfo (..)
    , AffiliateInfo(..)
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
import           Data.Time                    (UTCTime)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           GHC.Generics                 (Generic)
-- import           Network.HTTP.Conduit         (Manager, Request (..), RequestBody (..))
import           Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types           as HTTP (Header)


-- | Ebay api configuration.
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

data SearchRequest = SearchRequest
    { verb :: FindVerb
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

data OutputSelector = AspectHistogram
                    | CategoryHistogram
                    | ConditionHistogram
                    | GalleryInfoOutput
                    | PictureURLLarge
                    | PictureURLSuperSize
                    | SellerInfo
                    | StoreInfo
                    | UnitPriceInfo
                    deriving (Generic, Read, Show)

instance ToJSON OutputSelector
instance FromJSON OutputSelector

-- | Generic search query for ebay api.
data Search = Search
            { searchKeywords   :: Text
            , searchOutputSelector :: Maybe OutputSelector
            , searchSortOrder  :: Maybe SortOrder
            , searchItemFilter :: [ItemFilter]
            , searchAffiliateInfo :: Maybe AffiliateInfo
            } deriving Show

instance ToJSON Search where
    toJSON Search{..} = object $ [ "keywords"  .= searchKeywords
                                 -- , "paginationInput"
                                 ] ++ order searchSortOrder
                                   ++ ifilter searchItemFilter
                                   ++ oselector searchOutputSelector
                                   ++ affiliate searchAffiliateInfo
      where order (Just so) = ["sortOrder" .= so]
            order Nothing = []
            -- item filter field should not be
            -- added if the list is empty.
            ifilter [] = []
            ifilter sif = [ "itemFilter" .=  sif ]
            oselector Nothing = []
            oselector (Just os) = [ "outputSelector" .= os ]
            affiliate Nothing = []
            affiliate (Just a) = [ "affiliate" .= a ]

data AffiliateInfo = AffiliateInfo
    { networkId :: Int
    -- ^ The networkId specifies the third party who is your tracking
    --   partner. When specifying affiliate details, this field is
    --   required. Not all partners are valid for all sites.

    , trackingId :: Int
    -- ^ The trackingId specifies an ID to identify you to your
    --   tracking partner. The value you specify is obtained from
    --   your tracking partner. For eBay Partner Network, the
    --   trackingId is the Campaign ID ("campid") provided by eBay
    --   Partner Network. A Campaign ID is a 10-digit, unique number
    --   to be used for associating traffic. A Campaign ID is valid
    --   across all programs to which you have been accepted.

    , customId :: Maybe Int
    -- ^ The customId need not be specified. You can define a customId
    --   (up to 256 characters) if you want to leverage it to better
    --   monitor your marketing efforts. If you are using the eBay Partner
    --   Network, and you provide a customId, it will be contained in the
    --   tracking URL returned by eBay Partner Network.

    } deriving Show

instance ToJSON AffiliateInfo where
    toJSON AffiliateInfo{..} =
        object [ "trackingId" .= trackingId
               , "networkId" .= networkId
               , "customId" .= customId
               ]

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
infixl 5 .:>, .:?>
(.:>) :: FromJSON a => A.Object -> Text -> Parser a
(.:>) obj key = head <$> obj .: key

(.:?>) :: FromJSON a => A.Object -> Text -> Parser (Maybe a)
(.:?>) obj key = do
    ma <- obj .:? key
    return $ head <$> ma

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
        _ -> error $ "Response type not implemented: " ++ take 1000 (show o)
    parseJSON _ = mzero

data SearchResult = SearchResult
    { searchResultCount :: Text
    , searchResultItems :: [SearchItem]
    } deriving Show

instance FromJSON SearchResult where
    parseJSON (Object o) = do
        sr <-  o .:> "searchResult"
        SearchResult <$> sr .: "@count"
                     <*> sr .: "item"
    parseJSON _ = mzero

-- | A single product item
--
-- This items is not a direct translation of the
-- eBay type yet.
data SearchItem = SearchItem
    { searchItemId                    :: Text
    , searchItemTitle                 :: Text
    , searchItemSubtitle              :: Maybe Text
    , searchItemTopRatedListing       :: Bool
    , searchItemViewItemUrl           :: Text
    -- , searchItemGalleryInfo           :: Maybe GalleryInfo
    , searchItemGalleryUrl            :: Maybe Text
    , searchItemGalleryPlusPictureUrl :: Maybe Text
    , searchItemPictureLargeUrl       :: Maybe Text
    -- , searchItemPictureSuperSizeUrl :: Text
    -- , searchItemPostalCode :: Text
    -- , searchItemThumbnailUrl  :: Text
    , searchItemCondition     :: Condition
    , searchItemSellingStatus :: SellingStatus
    , searchItemListingInfo   :: Maybe ListingInfo
    -- , searchItemCategory :: Category
    -- , searchItemSellerInfo :: SellerInfo
    } deriving Show

data GalleryInfo = GalleryInfo
    { galleryInfoUrls :: [Text]
    } deriving Show

-- TODO: Cleanup parsing
instance FromJSON SearchItem where
    parseJSON (Object o) =
        SearchItem
             <$> o .:> "itemId"
             <*> o .:> "title"
             <*> o .:?> "subtitle"
             <*> (o .:> "topRatedListing"
                     >>= return . txtIsTrue)
             <*> o .:> "viewItemURL"
             -- <*> (o .:?> "galleryInfo")
             <*> o .:?> "galleryURL"
             <*> o .:?> "galleryPlusPictureURL"
             <*> o .:?> "pictureURLLarge"
             <*> o .:> "condition"
             <*> o .:> "sellingStatus"
             <*> o .:?> "listingInfo"
      where txtIsTrue btxt = btxt == ("True" :: Text) || btxt == "False"
    parseJSON _ = mzero

data ListingType = Auction
                 | AdFormat
                 | AuctionWithBIN
                 | Classified
                 | FixedPrice
                 | StoreInventory
                 deriving (Generic, Read, Show)

instance FromJSON ListingType

data ListingInfo = ListingInfo
    { listingInfoBestOfferEnabled :: Bool
    , listingInfoBuyItNowAvailable :: Bool
    , listingInfoBuyItNowPrice :: Maybe Text -- Double
    , listingInfoConvertedBuyItNowPrice :: Maybe Text -- Double
    , listingInfoEndTime :: UTCTime
    , listingInfoGift :: Bool
    , listingInfoType :: ListingType
    , listingInfoStartTime :: UTCTime
    } deriving Show

instance FromJSON ListingInfo where
    parseJSON (Object o) =
        ListingInfo
             <$> (o .:> "bestOfferEnabled"
                        >>= return . (== ("true" :: Text)))
             <*> (o .:> "buyItNowAvailable"
                        >>= return . (== ("true" :: Text)))
             <*> (o .:?> "buyItNowPrice" >>= \mbin ->
                      case mbin of
                          Nothing -> return Nothing
                          Just bin -> bin .: "__value__")
                                      -- TODO some values not parsing
                                      -- >>= return . read . T.unpack)
             <*> (o .:?> "convertedBuyItNowPrice" >>= \mcbin ->
                      case mcbin of
                        Nothing -> return Nothing
                        Just cbin -> cbin .: "__value__")
                                      -- TODO some values not parsing
                                      --  >>= return . read . T.unpack)
             <*> o .:> "endTime"
             <*> (o .:> "gift"
                        >>= return . (== ("true" :: Text)))
             <*> o .:> "listingType"
             <*> o .:> "startTime"
    parseJSON _ = mzero

data SellingStatus = SellingStatus
    { sellingStatusConvertedCurrentPrice :: Double
    , sellingStatusCurrentPrice :: Double
    , sellingStatusBidCount :: Maybe Text
    , sellingStatusState :: SellingState
    } deriving Show

instance FromJSON SellingStatus where
    parseJSON (Object o) = SellingStatus
                        <$> (o .:> "convertedCurrentPrice"
                                   >>= (.: "__value__")
                                   >>= return . read . T.unpack)
                        <*> (o .:> "currentPrice"
                                   >>= (.: "__value__")
                                   >>= return . read . T.unpack)
                        <*>  o .:?> "bidCount"
                        <*>  o .:> "sellingState"
    parseJSON _ = error "Unable to parse SellingStatus."

data SellingState = Active
                  | Canceled
                  | Ended
                  | EndedWithSales
                  | EndedWithoutSales
                  deriving (Eq, Generic, Show)

instance FromJSON SellingState

-- | Condition is made up of condition id
-- condition display name
data Condition = Condition Text Text deriving Show

instance FromJSON Condition where
    parseJSON (Object o) = Condition
                        <$> o .:> "conditionId"
                        <*> o .:> "conditionDisplayName"
    parseJSON _ = error "Error parsing Condition."

-- | Support verbs in finding api
--
-- 'verb' jargon taken from eBay docs.
data FindVerb = FindCompletedItems -- Retrieves items whose listings
                                   -- are completed and are no longer
                                   -- available for sale on eBay.

              | FindItemsAdvanced -- Finds items by a keyword query
                                  -- and/or category and allows
                                  -- searching within item descriptions.
              | Find
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
    initreq <- HTTP.parseUrl $ T.unpack $ proto <> domain <> uri

    let verb    = cmd
        payload = search
        er = SearchRequest { .. }

    let req = initreq
            { method = "POST"
            , requestHeaders = requestHeaders initreq
                            ++ requestHeadersFromConfig cmd cfg
            , requestBody = RequestBodyBS $ L.toStrict $ A.encode er
            }

    res2 <- HTTP.http req manager
    res <- HTTP.responseBody res2 $$+- sinkLbs
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
  where
    utf8 = T.encodeUtf8
    dataFormatEncode JsonEncoding = utf8 "JSON"
    dataFormatEncode XmlEncoding  = utf8 "XML"

findVerbToOperation :: FindVerb -> Text
findVerbToOperation fb = case fb of
    FindItemsByKeywords -> "findItemsByKeywords"
    FindCompletedItems  -> "findCompletedItems"
    FindItemsAdvanced   -> "findItemsAdvanced"
    _ -> error "Unsupported find verb to text operation."

findVerbToTns :: FindVerb -> Text
findVerbToTns fb = case fb of
    FindItemsByKeywords -> "findItemsByKeywordsRequest"
    FindCompletedItems  -> "findCompletedItemsRequest"
    FindItemsAdvanced   -> "findItemsAdvancedRequest"
    _ -> error "Unsupported find verb to tns operation."
