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
        keywords = "brooks brothers suit"
        handler = liftIO . print . averageCurrentPrice
        config = defaultEbayConfig { domain = "svcs.ebay.com"
                                     -- ^ Use `svcs.sandbox.ebay.com` when
                                     --   connecting to sandbox.
                                   , appid = "EBAY_APP_ID"
                                   , debug = False
                                   }

    withManager $ \manager -> do

        -- Find items by keywords.
        let search = Search keywords Nothing []
            searchRequest = SearchRequest FindItemsByKeywords search

        _ <- withSearchRequest searchRequest config manager handler

        -- Search for only completed items which have sold.
        let cFilter = [ ItemFilter ("Condition", condition)
                      , ItemFilter ("SoldItemsOnly", "true")
                      ]
            csearch = Search keywords (Just EndTimeSoonest) cFilter
            completedSearch = SearchRequest FindCompletedItems csearch

        _ <- withSearchRequest completedSearch config manager handler

        -- Find items which are still ongoing but closest to their end time.
        let onFilter = [ ItemFilter ("Condition", condition) ]
            search4 = Search keywords (Just EndTimeSoonest) onFilter
            searchRequest4 = SearchRequest FindItemsAdvanced search4

        _ <- withSearchRequest searchRequest4 config manager handler

        return ()
