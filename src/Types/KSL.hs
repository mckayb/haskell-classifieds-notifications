{-# LANGUAGE DeriveGeneric #-}

module Types.KSL where

import Prelude
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Text (Text)

{-# ANN KSLListing "HLint: ignore" #-}
data KSLListing = KSLListing
  { id :: Int
  , memberId :: Int
  , createTime :: Text
  , displayTime :: Text
  , modifyTime :: Text
  , expireTime :: Text
  , category :: Text
  , subCategory :: Text
  , price :: Float
  , title :: Text
  , description :: Text
  , marketType :: Text
  , city :: Text
  , city_lower :: Text
  , state :: Text
  , zip :: Text
  , name :: Text
  , homePhone :: Text
  , cellPhone :: Maybe Text
  , email :: Int
  , sellerType :: Text
  , lat :: Float
  , lon :: Float
  , standardFeaturedDates :: Maybe [Text]
  , photo :: Text
  , pageviews :: Int
  , favorited :: Int
  , listingType :: Text
  , source :: Text
  , contentType :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON KSLListing
