{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid ((<>))
import Web.Ebay

averageCurrentPrice :: Maybe SearchResponse -> Double
averageCurrentPrice sr = case sr of
    Nothing -> 0.0
    Just (SearchResponse _ SearchResult{..}) ->
        let price SellingStatus{..} = sellingStatusConvertedCurrentPrice
            f SearchItem{..} = (+) (price searchItemSellingStatus)
            total = foldr f 0.0 searchResultItems
        in total / fromIntegral (length searchResultItems)

printAvg :: Maybe SearchResponse -> IO ()
printAvg = print . (<>) "Average price: $" . show . averageCurrentPrice


main :: IO ()
main = do
    results <- simpleSearchWithVerb config searchRequest
    printAvg results
  where
    condition = "Used"
    keywords = "mechanical keyboard"
    config = defaultEbayConfig { ebDomain = "svcs.ebay.com"
                                 -- ^ Use `svcs.sandbox.ebay.com` when
                                 --   connecting to sandbox.
                               , ebAppId = "EBAY_APP_ID"
                               , ebDebug = False
                               }
    -- Find items by keywords.
    search = Search { searchKeywords = keywords
                    , searchOutputSelector = Just PictureURLLarge
                    , searchSortOrder = Nothing
                    , searchItemFilter = [ ItemFilter ("Condition", condition) ]
                    , searchAffiliateInfo = Nothing
                    }

    searchRequest = SearchRequest FindItemsByKeywords search
