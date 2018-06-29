module Types where

import Prelude (Int, Float, Show, Eq, (<$>), (<*>), ($))
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
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

type Environment = (ApiKey, MailAddress)

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