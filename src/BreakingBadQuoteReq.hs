{-# LANGUAGE OverloadedStrings #-}

module BreakingBadQuoteReq (randomQuote) where

import Network.HTTP.Req

import Types

breakingBadQuotesV1 = https "breaking-bad-quotes.herokuapp.com" /: "v1"

randomQuote :: IO BreakingBadQuote
randomQuote = runReq defaultHttpConfig $ do
    r <- req GET (breakingBadQuotesV1 /: "quotes") NoReqBody jsonResponse mempty
    return . head . responseBody $ r
