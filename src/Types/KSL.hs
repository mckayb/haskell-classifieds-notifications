module Types.Ksl where

import Prelude (Int, Float, Maybe, Show, Eq, (==), (&&))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Text (Text)

data Listing = KslListing
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
  } deriving (Show, Generic)

instance Eq Listing where
  (==) (KslListing _ _ _ _ _ _ _ _ priceA titleA _ _ _ _ _ _ nameA _ _ _ _ _ _ _ _ _ _ _ _ _)
       (KslListing _ _ _ _ _ _ _ _ priceB titleB _ _ _ _ _ _ nameB _ _ _ _ _ _ _ _ _ _ _ _ _)
       = priceA == priceB && titleA == titleB && nameA == nameB

instance FromJSON Listing
