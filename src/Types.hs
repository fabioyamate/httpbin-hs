{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data BreakingBadQuote = BreakingBadQuote
  { quote :: Text
  , author :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON BreakingBadQuote
instance ToJSON BreakingBadQuote
