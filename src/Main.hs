module Main where

import Prelude hiding (writeFile, dropWhile)
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Semigroup ((<>))
import Data.List (find)
import Data.Text (Text, pack)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 (ByteString, takeWhile, dropWhile, isInfixOf, init, pack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.Wreq (getWith, defaults, param, responseBody)
import Text.HTML.TagSoup (Tag(TagText), maybeTagText, parseTags, partitions, isTagText, (~==))
import Types.KSL (KSLListing)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq)

data Site = KSL | FacebookMarketplace | LetGo | OfferUp
  deriving (Show, Eq)

oneSecond :: Int
oneSecond = 1000000

isListingsTag :: Tag Data.ByteString.Char8.ByteString -> Bool
isListingsTag (TagText s) = Data.ByteString.Char8.pack "renderSearchSection" `Data.ByteString.Char8.isInfixOf` s

-- https://www.ebay.com/sch/i.html?_from=R40&_nkw=arcade&_sacat=0&_trksid=p2380057.m570.l1313.TR10.TRC2.A0.H0.Xarcade.TRS2
-- https://www.ksl.com/classifieds/search/?keyword=arcade&zip=&miles=25&priceFrom=&priceTo=&marketType%5B%5D=Sale&city=&state=&sort=0

handleSites :: Site -> SearchTerm -> IO ()
handleSites KSL (SearchTerm s) = do
  putStrLn "KSL Results: \n\n"
  let opts = defaults & param (pack "keyword") .~ [s]
  r <- getWith opts "https://www.ksl.com/classifieds/search/"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> do
      -- writeFile "results/ksl.html" $ toStrict body
      let listingTagText = find (\x -> isTagText x && isListingsTag x) $ parseTags (toStrict body)
      let maybeListingText = maybeTagText =<< listingTagText
      let maybeListingsJson = fmap ( Data.ByteString.Char8.init 
                                   . Data.ByteString.Char8.takeWhile (/= '\n') 
                                   . Data.ByteString.Char8.dropWhile (/= '[')
                                   ) maybeListingText
      let mListings = (decode =<< fmap fromStrict maybeListingsJson) :: Maybe [KSLListing]
      print mListings
    Nothing -> putStrLn "Couldn't find response body!"

handleSites FacebookMarketplace (SearchTerm s) = putStrLn "Checking Facebook Marketplace"
handleSites LetGo (SearchTerm s) = putStrLn "Checking LetGo"
handleSites OfferUp (SearchTerm s) = putStrLn "Checking OfferUp"

main :: IO ()
main = forever $ do
  handleSites KSL $ SearchTerm $ pack "arcade"
  handleSites FacebookMarketplace $ SearchTerm $ pack "arcade"
  handleSites LetGo $ SearchTerm $ pack "arcade"
  handleSites OfferUp $ SearchTerm $ pack "arcade"
  threadDelay $ 10 * oneSecond
