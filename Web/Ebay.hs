{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Api
-- Copyright   : (c) Christopher Reichert, 2014
-- License     : AllRightsReserved
-- Maintainer  : Christopher Reichert <creichert@reichertbrothers.com>
-- Stability   : experimental
-- Portability : GNU/Linux, FreeBSD
--
-- Haskell SDK for Ebay Finding API

module Web.Ebay
    (
      -- ** Functions
      searchWithVerb
    , simpleSearchWithVerb
    , defaultEbayConfig

      -- ** Ebay API types.
      --
      -- Most of these map directly to the eBay Finding API
      -- Documentation at:
      -- @http://developer.ebay.com/DevZone/finding/Concepts/FindingAPIGuide.html@

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
    , ProductId (..)
    , GalleryInfo (..)
    , AffiliateInfo(..)
    ) where


import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import Control.Applicative     (pure, (<$>), (<*>))
import Control.Monad           (mzero)
import Control.Monad.IO.Class  (MonadIO (liftIO))
import Data.Aeson              (FromJSON (..), ToJSON (..), Value (..), object,
                                (.:), (.:?), (.=))
import Data.Aeson.Types        (Parser)
import Data.Monoid             ((<>))
import Data.Text               (Text)
import Data.Time               (UTCTime)
import GHC.Generics            (Generic)
import Network.HTTP.Client     as HTTP
import Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types      as HTTP (Header)


-- | Ebay api configuration.
data EbayConfig = EbayConfig
    {
      -- | ebay api domain configuration
      -- Sandbox: svcs.sandbox.ebay.com
      -- Production: svcs.ebay.com
      ebDomain           :: !Text

      -- | ebay api domain configuration
    , ebUri              :: !Text

      -- | use https
      -- defaults to False.
    , ebHttps            :: !Bool
      -- , ebWarnings         :: !Bool
      -- , ebErrors           :: !Bool

      -- | EBay API Site Id.
      -- Default is `EBAY-US'.
    , ebSiteId           :: !Text

      -- | Specify the response encoding.
    , ebResponseEncoding :: !Encoding

      -- | Specify the request encoding.
    , ebRequestEncoding  :: !Encoding
      -- , ebProxy_host       :: !Text
      -- , ebProxy_port       :: !Text
      -- , ebToken            :: !Text
      -- , ebIaf_token        :: !Text

      -- api key
    , ebAppId            :: !Text
    , ebVersion          :: !Text

      -- | eBay API service.
      -- Currently, this library only supports the
      -- Finding API and this value is always
      -- `FindingApi'
    , ebService          :: !Text
    , ebDocUrl           :: !Text

      -- | Enable debugging
    , ebDebug            :: !Bool
    } deriving Show


-- | Supported response encoding
data Encoding = XmlEncoding
              | JsonEncoding
              deriving Show


-- | Ebay Finding API search request.
data SearchRequest = SearchRequest
    { verb    :: !FindVerb  -- ^ specify the type of search
    , payload :: Search -- ^ body of the search (xml or json)
    } deriving Show


instance ToJSON SearchRequest where
    toJSON SearchRequest{..} = object [ "jsonns.xsi" .= (xsi :: Text)
                                      , "jsonns.xs"  .= (xs :: Text)
                                      , "jsonns.tns" .= (nstns :: Text)
                                      , tns          .= payload
                                      ]
      where tns   = "tns." <> findVerbToOperation verb True
            xsi   = "http://www.w3.org/2001/XMLSchema-instance"
            xs    = "http://www.w3.org/2001/XMLSchema"
            nstns = "http://www.ebay.com/marketplace/search/v1/services"


newtype ItemFilter = ItemFilter (Text, Text) deriving Show


instance ToJSON ItemFilter where
    toJSON (ItemFilter (name, val)) =
        object [ "name"  .= name
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


instance ToJSON OutputSelector where
    toJSON = A.genericToJSON A.defaultOptions

instance FromJSON OutputSelector


data ProductId = EAN Integer | ISBN Integer | UPC Integer | ReferenceId String deriving Show

productIdToParams :: ProductId -> (String, String)
productIdToParams (EAN ean) = ("EAN", show ean)
productIdToParams (ISBN isbn) = ("ISBN", show isbn)
productIdToParams (UPC upc) = ("UPC", show upc)
productIdToParams (ReferenceId referenceId) = ("ReferenceID", referenceId)

instance ToJSON ProductId where
  toJSON productId = object ["@type" .= idType, "__value__" .= id']
    where (idType, id') = productIdToParams productId


-- | Generic search query for ebay api.
data Search = Search
            { searchKeywords       :: !Text
            , searchOutputSelector :: Maybe OutputSelector
            , searchSortOrder      :: Maybe SortOrder
            , searchItemFilter     :: ![ItemFilter]
            , searchAffiliateInfo  :: Maybe AffiliateInfo
            , searchProductId      :: Maybe ProductId
            } deriving Show


instance ToJSON Search where
    toJSON Search{..} = object $ [ "keywords"  .= searchKeywords
                                 -- , "paginationInput"
                                 ] ++ order searchSortOrder
                                   ++ ifilter searchItemFilter
                                   ++ oselector searchOutputSelector
                                   ++ affiliate searchAffiliateInfo
                                   ++ productId searchProductId
      where order (Just so)     = ["sortOrder" .= so]
            order Nothing       = []
            -- item filter field should not be
            -- added if the list is empty.
            ifilter []          = []
            ifilter sif         = [ "itemFilter" .=  sif ]
            oselector Nothing   = []
            oselector (Just os) = [ "outputSelector" .= os ]
            affiliate Nothing   = []
            affiliate (Just a)  = [ "affiliate" .= a ]
            productId Nothing   = []
            productId (Just a)  = [ "productId" .= a ]


data AffiliateInfo = AffiliateInfo
    { networkId  :: !Int
      -- ^ The networkId specifies the third party who is your tracking
      --   partner. When specifying affiliate details, this field is
      --   required. Not all partners are valid for all sites.

    , trackingId :: !Int
      -- ^ The trackingId specifies an ID to identify you to your
      --   tracking partner. The value you specify is obtained from
      --   your tracking partner. For eBay Partner Network, the
      --   trackingId is the Campaign ID ("campid") provided by eBay
      --   Partner Network. A Campaign ID is a 10-digit, unique number
      --   to be used for associating traffic. A Campaign ID is valid
      --   across all programs to which you have been accepted.

    , customId   :: !(Maybe Int)
      -- ^ The customId need not be specified. You can define a customId
      --   (up to 256 characters) if you want to leverage it to better
      --   monitor your marketing efforts. If you are using the eBay Partner
      --   Network, and you provide a customId, it will be contained in the
      --   tracking URL returned by eBay Partner Network.

    } deriving (Eq, Read, Show)


instance ToJSON AffiliateInfo where
    toJSON AffiliateInfo{..} =
        object [ "trackingId" .= trackingId
               , "networkId"  .= networkId
               , "customId"   .= customId
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

instance ToJSON SortOrder where
    toJSON = A.genericToJSON A.defaultOptions

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


parseHead :: FromJSON a => [a] -> Maybe a
parseHead (x:_) = Just x
parseHead _     = Nothing

-- use GADTs to get rid of this.
instance FromJSON SearchResponse where
    parseJSON (Object o) = case HM.keys o of
        ("findCompletedItemsResponse":_) -> do
            mobj <- o .: "findCompletedItemsResponse"
            case parseHead mobj of
              Nothing -> mzero
              Just obj -> SearchResponse
                            <$> pure FindCompletedItems
                            <*> parseJSON obj
        ("findItemsByKeywordsResponse":_) -> do
            mobj <- o .: "findItemsByKeywordsResponse"
            case parseHead mobj of
              Nothing -> mzero
              Just obj -> SearchResponse
                            <$> pure FindItemsByKeywords
                            <*> parseJSON obj
        ("findItemsAdvancedResponse":_) -> do
            mobj <- o .: "findItemsAdvancedResponse"
            case parseHead mobj of
              Nothing -> mzero
              Just obj -> SearchResponse
                            <$> pure FindItemsAdvanced
                            <*> parseJSON obj
        ("errorMessage":_) -> error $ "An error occurred: " ++ show o
        _ -> mzero
    parseJSON _ = mzero


data SearchResult = SearchResult
    { searchResultCount :: Text
    , searchResultItems :: [SearchItem]
    } deriving Show


instance FromJSON SearchResult where
    parseJSON (Object o) = do
        sr <- o .:> "searchResult"
        SearchResult <$> sr .: "@count"
                     <*> sr .: "item"
    parseJSON _ = mzero


-- | A single ebay listing item
--
-- Note that some fields have not yet been implemented
-- from the ebay api documentation.
data SearchItem = SearchItem
    { searchItemId                    :: !Text
    , searchItemTitle                 :: !Text
    , searchItemSubtitle              :: !(Maybe Text)
    , searchItemTopRatedListing       :: !Bool
    , searchItemViewItemUrl           :: !Text
    -- , searchItemGalleryInfo        :: Maybe GalleryInfo
    , searchItemGalleryUrl            :: !(Maybe Text)
    , searchItemGalleryPlusPictureUrl :: !(Maybe Text)
    , searchItemPictureLargeUrl       :: !(Maybe Text)
    -- , searchItemPictureSuperSizeUrl :: Text
    -- , searchItemPostalCode          :: Text
    -- , searchItemThumbnailUrl        :: Text
    , searchItemCondition             :: Condition
    , searchItemSellingStatus         :: SellingStatus
    , searchItemListingInfo           :: Maybe ListingInfo
    -- , searchItemCategory           :: Category
    -- , searchItemSellerInfo         :: SellerInfo
    } deriving Show


data GalleryInfo = GalleryInfo
    { galleryInfoUrls :: ![Text]
    } deriving Show


-- TODO: Cleanup parsing
instance FromJSON SearchItem where
    parseJSON (Object o) =
        SearchItem
             <$> o .:> "itemId"
             <*> o .:> "title"
             <*> o .:?> "subtitle"
             <*> (o .:> "topRatedListing"
                     >>= return . truetxt)
             <*> o .:> "viewItemURL"
             -- <*> (o .:?> "galleryInfo")
             <*> o .:?> "galleryURL"
             <*> o .:?> "galleryPlusPictureURL"
             <*> o .:?> "pictureURLLarge"
             <*> o .:> "condition"
             <*> o .:> "sellingStatus"
             <*> o .:?> "listingInfo"
      where truetxt btxt = btxt == ("True" :: Text) || btxt == "False"
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
    { listingInfoBestOfferEnabled       :: !Bool
    , listingInfoBuyItNowAvailable      :: !Bool

    , listingInfoBuyItNowPrice          :: !(Maybe Text)
    , listingInfoConvertedBuyItNowPrice :: !(Maybe Text)
      -- TODO it would be preferrabel to use Double fields for
      --  all pricing. However, in practice I noticed a significant
      --  amount of unpredicatble parse errors.

    , listingInfoEndTime                :: !UTCTime
    , listingInfoGift                   :: !Bool
    , listingInfoType                   :: ListingType
    , listingInfoStartTime              :: !UTCTime
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
    { sellingStatusConvertedCurrentPrice :: !Double
    , sellingStatusCurrentPrice          :: !Double
    , sellingStatusBidCount              :: !(Maybe Text)
    , sellingStatusState                 :: SellingState
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
    parseJSON _ = mzero


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
    parseJSON _ = mzero


-- | Support verbs in finding api
--
-- 'verb' jargon taken from eBay docs.
data FindVerb = FindCompletedItems -- Retrieves items whose listings
                                   -- are completed and are no longer
                                   -- available for sale on eBay.

              | FindItemsAdvanced -- Finds items by a keyword query
                                  -- and/or category and allows
                                  -- searching within item descriptions.
              | FindItemsByImage
              | FindItemsByKeywords
              | FindItemsByProduct
              | FindItemsIneBayStores
              | GetHistograms
              | GetSearchKeywordsRecommendation
              | GetVersion
              deriving Show


------------------------------------------------------------------------------


simpleSearchWithVerb :: EbayConfig
                     -> SearchRequest
                     -> IO (Maybe SearchResponse)
simpleSearchWithVerb cfg (SearchRequest fv s)  = do
    man <- newManager TLS.tlsManagerSettings
    searchWithVerb cfg fv s man


-- | Runs an eBay Finding API search
searchWithVerb :: MonadIO m
               => EbayConfig -- ^ api configuration
               -> FindVerb -- ^ action to run
               -> Search -- ^ search request
               -> Manager -- ^ http connection manager
               -> m (Maybe SearchResponse)
searchWithVerb ecfg cmd search manager = do

    initreq <- liftIO $ HTTP.parseUrl (eburl ecfg)

    let req = initreq
            { method = "POST"
            , requestHeaders = requestHeaders initreq
                            ++ requestHeadersFromConfig cmd ecfg
            , requestBody = encodeRequestBody esr
            }

    res' <- liftIO $ HTTP.httpLbs req manager
    return $ decodeResponseBody res'
  where
    proto = if ebHttps ecfg then "https://" else "http://"
    eburl EbayConfig{..} = T.unpack $ proto <> ebDomain <> ebUri
    esr   = SearchRequest cmd search
    encodeRequestBody = RequestBodyBS . L.toStrict . A.encode
    decodeResponseBody :: Response L.ByteString -> Maybe SearchResponse
    decodeResponseBody = A.decode . responseBody


-- | Default Ebay configuration for working with the finding API in a
-- sandbox.
defaultEbayConfig :: EbayConfig
defaultEbayConfig = EbayConfig
    { ebDomain           = "svcs.ebay.com"
    , ebUri              = "/services/search/FindingService/v1"
    , ebSiteId           = "EBAY-US"
    , ebResponseEncoding = JsonEncoding
    , ebRequestEncoding  = JsonEncoding
    , ebAppId            = ""
    , ebVersion          = "1.12.0"
    , ebService          = "FindingService"
    , ebDocUrl           = docurl
    , ebDebug            = False
    , ebHttps            = False
    }
  where
    docurl = "http://developer.ebay.com/DevZone/finding/CallRef/index.html"


-- | Generate the list of HTTP headers needed by a 'SearchRequest'
requestHeadersFromConfig :: FindVerb -> EbayConfig -> [HTTP.Header]
requestHeadersFromConfig fb EbayConfig{..} =
    [ ("Content-Type", "application/json")
    , ("X-EBAY-SOA-SERVICE-NAME", utf8 ebService)
    , ("X-EBAY-SOA-SERVICE-VERSION", utf8 ebVersion)
    , ("X-EBAY-SOA-SECURITY-APPNAME", utf8 ebAppId)
    , ("X-EBAY-SOA-GLOBAL-ID", utf8 ebSiteId)
    , ("X-EBAY-SOA-OPERATION-NAME", encodedFindVerb)
    , ("X-EBAY-SOA-REQUEST-DATA-FORMAT",  dataFormatEncode ebRequestEncoding)
    , ("X-EBAY-SOA-RESPONSE-DATA-FORMAT", dataFormatEncode ebResponseEncoding)
    ]
  where
    utf8 = T.encodeUtf8
    dataFormatEncode JsonEncoding = utf8 "JSON"
    dataFormatEncode XmlEncoding  = utf8 "XML"
    encodedFindVerb = T.encodeUtf8 $ findVerbToOperation fb False


-- | Convert the FindVerb to Text suitable for ebay request headers.
findVerbToOperation :: FindVerb -> Bool -> Text
findVerbToOperation fb tns = op <> req
  where
    op = case fb of
          FindItemsByKeywords             -> "findItemsByKeywords"
          FindCompletedItems              -> "findCompletedItems"
          FindItemsAdvanced               -> "findItemsAdvanced"
          FindItemsByImage                -> "findItemsByImage"
          FindItemsByProduct              -> "findItemsByProduct"
          FindItemsIneBayStores           -> "findItemsIneBayStores"
          GetHistograms                   -> "getHistograms"
          GetSearchKeywordsRecommendation -> "getSearchKeywordsRecommendation"
          GetVersion                      -> "getVersion"
    req = if tns then "Request" else ""
