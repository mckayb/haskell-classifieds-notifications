module Types.Listings where

import Prelude (Int, Float, Show, Eq, (<$>), (<*>), ($))
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Data.Foldable (asum)
import Data.Text (Text)

data Listing =
    KslListing
    { id :: Int
    , price :: Float
    , title :: Text
    , description :: Text
    , name :: Text
    , homePhone :: Text
    }
  | CraigsListListing
    { url :: Text
    , title :: Text
    , clPrice :: Text
    }
  deriving (Eq, Show)

instance FromJSON Listing where
  parseJSON = withObject "ksl or craigslist" $ \o -> asum
    [ KslListing <$> o .: "id" <*> o .: "price" <*> o .: "title" <*> o .: "description" <*> o .: "name" <*> o .: "homePhone"
    , CraigsListListing <$> o .: "url" <*> o .: "title" <*> o .: "price"
    ]
