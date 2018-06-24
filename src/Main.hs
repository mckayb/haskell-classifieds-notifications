module Main where

import Prelude (Show, Eq, Bool(False), Maybe(Just, Nothing), IO, Int, print, putStrLn, pure, filter, zipWith, sequence, fmap, mapM, foldr, show, (>), (<$>), (<*>), ($), (>>=), (*), (=<<), (/=), (.), (&&), (==))
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Control.Concurrent (threadDelay)
import Data.HashMap.Lazy (HashMap, singleton, unions, (!))
import Data.Semigroup ((<>))
import Data.List (find, (\\), takeWhile, dropWhile)
import Data.List.NonEmpty (fromList)
import Data.Text (Text, length, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson (decodeStrict)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Char8 as BS (ByteString, takeWhile, dropWhile, isInfixOf, init, pack)
import Data.ByteString.Lazy (toStrict)
import Network.Wreq (getWith, defaults, param, responseBody)
import Text.HTML.TagSoup (Tag(TagText, TagOpen), maybeTagText, parseTags, isTagText, partitions, (~==), (~/=))
import Types.Listings (Listing(KslListing, CraigsListListing))
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Network.SendGridV3.Api (ApiKey(ApiKey) , MailAddress(MailAddress) , Mail , sendMail , personalization , mail , mailContentText)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq, Generic)
instance Hashable SearchTerm

newtype Subdomain = Subdomain Text
  deriving (Show, Eq, Generic)
instance Hashable Subdomain

data Site = Ksl | FacebookMarketplace | LetGo | OfferUp | CraigsList Subdomain
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
      let mListings = (decodeStrict =<< maybeListingsJson) :: Maybe [Listing]
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
handleSites (CraigsList (Subdomain sub)) (SearchTerm s) = do
  let opts = defaults & param (pack "query") .~ [s]
  r <- getWith opts $ "https://" <> unpack sub <> ".craigslist.org/search/sss"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> do
      let tags = parseTags (toStrict body)
      let parts = partitions (~== unpack "<p class='result-info'>") tags
      let resultArrHtml = fmap (takeWhile (~/= unpack "</li>")) parts
      let prices = fmap ( fmap maybeTagText
                        . filter isTagText
                        . takeWhile (~/= unpack "</span>")
                        . dropWhile (~/= unpack "<span class='result-price'>")
                        ) resultArrHtml
      let titlesAndLinks = fmap ( fmap parseLinkAndTitle 
                                . takeWhile (~/= unpack "</a>")
                                . dropWhile (~/= unpack "<a class='result-title hdrlnk'>")
                                ) resultArrHtml
      pure $ catMaybes $ toCraigsListListing . sequence <$> zipWith handleUnknown prices titlesAndLinks
    Nothing -> do
      putStrLn "Couldn't find response body!"
      pure []
  where
    handleUnknown :: [Maybe BS.ByteString] -> [Maybe BS.ByteString] -> [Maybe BS.ByteString]
    handleUnknown [] a = [Just "$Unknown"] <> a
    handleUnknown _ [] = []
    handleUnknown a b = a <> b

    toCraigsListListing :: Maybe [BS.ByteString] -> Maybe Listing
    toCraigsListListing (Just [price, url, title]) = Just $ CraigsListListing (decodeUtf8 url) (decodeUtf8 title) (decodeUtf8 price)
    toCraigsListListing _ = Nothing

    parseLinkAndTitle :: Tag BS.ByteString -> Maybe BS.ByteString
    parseLinkAndTitle (TagOpen "a" (("href", a):_)) = Just a
    parseLinkAndTitle (TagText a) = Just a
    parseLinkAndTitle _ = Nothing

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
getMailContent :: [Listing] -> Text
getMailContent listings = foldr (<>) "" $ fmap f listings
  where
    f (KslListing id price title _ name homePhone) = title <> "\n" <> pack (show price) <> "\n" <> "ksl.com/classifieds/listing/" <> pack (show id) <> "\n" <> name <> "\n" <> homePhone <> "\n"
    f (CraigsListListing url title price) = title <> "\n" <> url <> "\n" <> price <> "\n"

getListings :: Environment -> StateT ListingsMap IO ListingsMap
getListings (sendgridApiKey, mailAddr) = forever $ do
  prevListings <- get

  results <- liftIO $ mapM (group prevListings) activeSiteSearchTuple

  let diffContent = foldr (\(_, _, _, diff) prev -> prev <> getMailContent diff) "" results
  let newListings = unions $ foldr (\(s, s', new, _) prev -> prev <> [singleton (s, s') new]) [] results

  liftIO $ when (length diffContent > 1) $ do
    statusCode <- sendMail sendgridApiKey (createMail mailAddr diffContent)
    print ("Sent email: " <> show statusCode)

  liftIO $ when (length diffContent == 0) $ putStrLn "No Results yet..."

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
activeSites = [Ksl, FacebookMarketplace, OfferUp, LetGo]

activeSearchTerms :: [SearchTerm]
activeSearchTerms = fmap SearchTerm ["arcade", "pinball"]

activeSiteSearchTuple :: [SiteSearchTuple]
activeSiteSearchTuple = (fmap (,) activeSites <*> activeSearchTerms)
  <> [(CraigsList (Subdomain "saltlakecity"), SearchTerm "(\"coin op*\"|(upright|standup|coin*|quarter*|taito|bally|midway game|arcade)|coin-op*|coinop|neogeo|\"neo-geo\"|\"neo geo\"|arkade|acade|acrade|arcade|aracade) -wash* -xbox -dry* -tic*")]

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
