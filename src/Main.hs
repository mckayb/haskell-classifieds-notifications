module Main where

import Prelude (Show, Eq, Bool(False), Maybe(Just, Nothing), IO, Int, print, putStrLn, pure, sequence, fmap, mapM, foldr, show, (>), (<$>), (<*>), ($), (>>=), (*), (=<<), (/=), (.), (&&), (==))
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Control.Concurrent (threadDelay)
import Data.HashMap.Lazy (HashMap, singleton, unions, (!))
import Data.Semigroup ((<>))
import Data.List (find, (\\), takeWhile)
import Data.List.NonEmpty (fromList)
import Data.Text (Text, length, pack, unpack)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS (ByteString, takeWhile, dropWhile, isInfixOf, init, pack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.Wreq (getWith, defaults, param, responseBody)
import Text.HTML.TagSoup (Tag(TagText), maybeTagText, parseTags, isTagText, partitions, sections, (~==), (~/=))
import Types.Ksl (Listing(KslListing))
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Network.SendGridV3.Api (ApiKey(ApiKey) , MailAddress(MailAddress) , Mail , sendMail , personalization , mail , mailContentText)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq, Generic)
instance Hashable SearchTerm

data Site = Ksl | FacebookMarketplace | LetGo | OfferUp | CraigsList Text
  deriving (Show, Eq, Generic)
instance Hashable Site

type SiteSearchTuple = (Site, SearchTerm)

type ListingsMap = HashMap SiteSearchTuple [Listing]

type Environment = (ApiKey, MailAddress)

-- https://www.ebay.com/sch/i.html?_from=R40&_nkw=arcade&_sacat=0&_trksid=p2380057.m570.l1313.TR10.TRC2.A0.H0.Xarcade.TRS2
-- https://www.ksl.com/classifieds/search/?keyword=arcade&zip=&miles=25&priceFrom=&priceTo=&marketType%5B%5D=Sale&city=&state=&sort=0

handleSites :: Site -> SearchTerm -> IO [Listing]
handleSites Ksl (SearchTerm s) = do
  let opts = defaults & param (pack "keyword") .~ [s]
  r <- getWith opts "https://www.ksl.com/classifieds/search/"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> do
      let listingTagText = find (\x -> isTagText x && isListingsTag x) $ parseTags (toStrict body)
      let maybeListingText = maybeTagText =<< listingTagText
      let maybeListingsJson = fmap ( BS.init 
                                   . BS.takeWhile (/= '\n') 
                                   . BS.dropWhile (/= '[')
                                   ) maybeListingText
      let mListings = (decode =<< fmap fromStrict maybeListingsJson) :: Maybe [Listing]
      pure $ fromMaybe [] mListings
    Nothing -> do
      putStrLn "Couldn't find response body!"
      pure []
  where
    isListingsTag :: Tag BS.ByteString -> Bool
    isListingsTag (TagText s') = BS.pack "renderSearchSection" `BS.isInfixOf` s'
    isListingsTag _ = False

handleSites FacebookMarketplace (SearchTerm _) = pure []
handleSites LetGo (SearchTerm _) = pure []
handleSites OfferUp (SearchTerm _) = pure []
handleSites (CraigsList subdomain) (SearchTerm s) = do
  let opts = defaults & param (pack "query") .~ [s]
  r <- getWith opts $ "https://" <> unpack subdomain <> ".craigslist.org/search/sss"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> do
      let tags = parseTags (toStrict body)
      let parts = partitions (~== unpack "<p class='result-info'>") tags
      let resultArrHtml = fmap (takeWhile (~/= unpack "</li>")) parts

      -- Pull price from <span class='result-price'>$PRICE</span>
      -- Pull title and link from <a class='result-title' href='the-link'>TITLE</a>
      -- Pull location from <span class='result-hood'>LOCATION</span>
      print resultArrHtml
      pure []
    Nothing -> do
      putStrLn "Couldn't find response body!"
      pure []

createMail :: MailAddress -> Text -> Mail () ()
createMail addr content = mail to' from' subject' content'
  where
    from' = MailAddress "arcade.dealz@gmail.com" "Arcade Finder"
    subject' = "Possible Deals"
    content' = fromList [mailContentText content]
    to' = [personalization (fromList [addr])]

diffListings :: [Listing] -> [Listing] -> [Listing]
-- If one of these is empty, then something has gone wrong, or it's the first call, so we can't get a good diff
diffListings [] _ = []
diffListings _ [] = []
diffListings a b = b \\ a

-- "https://www.ksl.com/classifieds/listing/id-of-listing"
getMailContent :: Site -> [Listing] -> Text
getMailContent Ksl listings = foldr (<>) "" $ fmap f listings
  where f (KslListing id price title _ name homePhone) = title <> "\n" <> pack (show price) <> "\n" <> "ksl.com/classifieds/listing/" <> pack (show id) <> "\n" <> name <> "\n" <> homePhone <> "\n"
getMailContent FacebookMarketplace _ = ""
getMailContent LetGo _ = ""
getMailContent OfferUp _ = ""
getMailContent (CraigsList _) _ = ""

getListings :: Environment -> StateT ListingsMap IO ListingsMap
getListings (sendgridApiKey, mailAddr) = forever $ do
  prevListings <- get

  results <- liftIO $ mapM (group prevListings) activeSiteSearchTuple

  let diffContent = foldr (\(s, _, _, diff) prev -> prev <> getMailContent s diff) "" results
  let newListings = unions $ foldr (\(s, s', new, _) prev -> prev <> [singleton (s, s') new]) [] results

  liftIO $ when (length diffContent > 1) $ do
      statusCode <- sendMail sendgridApiKey (createMail mailAddr diffContent)
      print ("Sent email: " <> show statusCode)

  put newListings

  liftIO $ threadDelay $ oneSecond * 30
  where
    group :: ListingsMap -> SiteSearchTuple -> IO (Site, SearchTerm, [Listing], [Listing])
    group p (s, s') =  do
      new <- handleSites s s'
      let old = p ! (s, s')
      pure (s, s', new, diffListings old new)

    oneSecond :: Int
    oneSecond = 1000000

activeSites :: [Site]
-- activeSites = [Ksl, FacebookMarketplace, OfferUp, LetGo]
activeSites = [FacebookMarketplace, OfferUp, LetGo]

activeSearchTerms :: [SearchTerm]
activeSearchTerms = fmap SearchTerm ["arcade", "pinball"]

activeSiteSearchTuple :: [SiteSearchTuple]
activeSiteSearchTuple = (fmap (,) activeSites <*> activeSearchTerms)
  <> [(CraigsList "saltlakecity", SearchTerm "(\"coin op*\"|(upright|standup|coin*|quarter*|taito|bally|midway game|arcade)|coin-op*|coinop|neogeo|\"neo-geo\"|\"neo geo\"|arkade|acade|acrade|arcade|aracade) -wash* -xbox -dry* -tic*")]
  -- <> [(CraigsList "saltlakecity", SearchTerm "((pin ball)|(coin operated)|(upright game)|(standup game)|(coin game)|coin-op|(coin op)|(quarters game)|coinop|pinbal|pinabll|pinnball|arkade|acade|acrade|arcade|pinball|aracade)")]
  -- <> [(CraigsList "saltlakecity", SearchTerm "((takes quarters)|(coin operated)|(upright game)|(standup game)|(coin game)|coin-op|(coin op)|(quarters game)|coinop|neogeo|neo-geo|(neo geo)|arkade|acade|acrade|arcade|aracade)")]

initialState :: ListingsMap
initialState = unions $ fmap (\s -> singleton s []) activeSiteSearchTuple

main :: IO ()
main = do
  let combine key pn = [key, fmap (<> "@txt.att.net") pn]
  vars <- lookupEnv "SENDGRID_API_KEY" >>= (\key -> combine key <$> lookupEnv "PHONE_NUMBER")
  let env = fmap pack <$> sequence vars
  case env of
    Just [apiKey, email] -> do
      let massagedEnv = (ApiKey apiKey, MailAddress email "Deal Finder")
      _ <- runStateT (getListings massagedEnv) initialState
      exitSuccess
    _ -> do
      putStrLn "Environment not set properly."
      exitFailure
