module Types.Ksl where

import Prelude (Int, Float, Maybe, Show, Eq, Ord, Ordering(LT), Bool(True), compare, (==), (&&))
import Control.Applicative ((<|>))
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
  (==) (KslListing _ _ _ _ _ _ _ _ priceA titleA descA marketTypeA cityA _ stateA zipA nameA homePhoneA _ _ _ _ _ _ _ _ _ _ _ _)
       (KslListing _ _ _ _ _ _ _ _ priceB titleB descB marketTypeB cityB _ stateB zipB nameB homePhoneB _ _ _ _ _ _ _ _ _ _ _ _)
       = priceA == priceB && titleA == titleB && descA == descB && marketTypeA == marketTypeB && cityA == cityB
         && stateA == stateB && zipA == zipB && nameA == nameB && homePhoneA == homePhoneB

-- instance Ord Listing where
  -- compare (KslListing _ _ _ _ _ _ _ _ _ titleA _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
          -- (KslListing _ _ _ _ _ _ _ _ _ titleB _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = titleA `compare` titleB

instance FromJSON Listing
