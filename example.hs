{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Network.HTTP.Conduit (withManager)

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

    let condition = "Used"
        keywords = "mechanical keyboard"
        handler = liftIO . print . averageCurrentPrice
        config = defaultEbayConfig { ebDomain = "svcs.ebay.com"
                                     -- ^ Use `svcs.sandbox.ebay.com` when
                                     --   connecting to sandbox.
                                   , ebAppId = "EBAY_APP_ID"
                                   , ebDebug = False
                                   }

    withManager $ \manager -> do

        -- Find items by keywords.
        let search = Search { searchKeywords = keywords
                            , searchOutputSelector = Just PictureURLLarge
                            , searchSortOrder = Nothing
                            , searchItemFilter = [ ItemFilter ("Condition", condition) ]
                            , searchAffiliateInfo = Nothing
                            }
            searchRequest = SearchRequest FindItemsByKeywords search

        _ <- withSearchRequest searchRequest config manager handler

        return ()
