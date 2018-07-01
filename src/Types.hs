module Types where

import Prelude (Int, Maybe, Float, Show, Eq, (<$>), (<*>), ($))
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?))
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.SendGridV3.Api (ApiKey, MailAddress)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq, Generic)
instance Hashable SearchTerm

newtype Subdomain = Subdomain Text
  deriving (Show, Eq, Generic)
instance Hashable Subdomain

data Site = Ksl | CraigsList Subdomain
  deriving (Show, Eq, Generic)
instance Hashable Site

type SiteSearchTuple = (Site, SearchTerm)

type ListingsMap = HashMap SiteSearchTuple [Listing]

type Environment = (ApiKey, MailAddress, [SiteSearchTuple])

data Listing =
    KslListing
    { kslId :: Int
    , kslPrice :: Float
    , kslTitle :: Text
    , kslDescription :: Text
    , kslName :: Text
    , kslHomePhone :: Maybe Text
    }
  | CraigsListListing
    { clUrl :: Text
    , clTitle :: Text
    , clPrice :: Text
    }
  deriving (Eq, Show)

instance FromJSON Listing where
  parseJSON = withObject "ksl or craigslist" $ \o -> asum
    [ KslListing <$> o .: "id" <*> o .: "price" <*> o .: "title" <*> o .: "description" <*> o .: "name" <*> o .:? "homePhone"
    , CraigsListListing <$> o .: "url" <*> o .: "title" <*> o .: "price"
    ]