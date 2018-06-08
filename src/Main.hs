module Main where

import Prelude hiding (writeFile)
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.ByteString (writeFile)
import Data.ByteString.Lazy (toStrict)
import Network.Wreq (getWith, defaults, param, responseBody)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq)

data Site = KSL | FacebookMarketplace | LetGo | OfferUp
  deriving (Show, Eq)

oneSecond :: Int
oneSecond = 1000000

-- https://www.ebay.com/sch/i.html?_from=R40&_nkw=arcade&_sacat=0&_trksid=p2380057.m570.l1313.TR10.TRC2.A0.H0.Xarcade.TRS2
-- https://www.ksl.com/classifieds/search/?keyword=arcade&zip=&miles=25&priceFrom=&priceTo=&marketType%5B%5D=Sale&city=&state=&sort=0

handleSites :: Site -> SearchTerm -> IO ()
handleSites KSL (SearchTerm s) = do
  putStrLn "KSL Results: \n\n"
  let opts = defaults & param "keyword" .~ [s]
  r <- getWith opts "https://www.ksl.com/classifieds/search/"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> writeFile "results/ksl.html" $ toStrict body
    Nothing -> putStrLn "Couldn't find response body!"
handleSites FacebookMarketplace (SearchTerm s) = putStrLn "Checking Facebook Marketplace"
handleSites LetGo (SearchTerm s) = putStrLn "Checking LetGo"
handleSites OfferUp (SearchTerm s) = putStrLn "Checking OfferUp"

main :: IO ()
main = forever $ do
  handleSites KSL $ SearchTerm "arcade"
  handleSites FacebookMarketplace $ SearchTerm "arcade"
  handleSites LetGo $ SearchTerm "arcade"
  handleSites OfferUp $ SearchTerm "arcade"
  threadDelay $ 10 * oneSecond
