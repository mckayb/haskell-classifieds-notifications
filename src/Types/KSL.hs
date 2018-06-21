module Types.Ksl where

import Prelude (Int, Float, Maybe, Bool, Show, Eq, (==), (&&), (<*>), (<$>), ($))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Data.Text (Text)

data Listing =
  KslListing
    { id :: Int
    , price :: Float
    , title :: Text
    , description :: Text
    , name :: Text
    , homePhone :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON Listing