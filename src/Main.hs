module Main where

import qualified Data.ByteString.Char8 as BS (ByteString, takeWhile, dropWhile, isInfixOf, init, pack, drop)

import Prelude (String, Bool(False), Maybe(Just, Nothing), IO, Int, print, putStrLn, pure, filter, zipWith, sequence, fmap, mapM, otherwise, foldr, show, (-), (||), (>), (<=), (>=), (<$>), (<*>), ($), (*), (=<<), (/=), (.), (&&), (==), (+))
import Control.Concurrent (threadDelay)
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Data.Aeson (decodeStrict)
import Data.HashMap.Lazy (singleton, unions, (!))
import Data.List (find, (\\), takeWhile, dropWhile)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text, length, pack, unpack, split)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.ByteString.Lazy (toStrict)
import Network.SendGridV3.Api (ApiKey(ApiKey), MailAddress(MailAddress), Mail, sendMail, personalization, mail, mailContentText)
import Network.Wreq (getWith, defaults, param, responseBody)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Random (getStdRandom, randomR)
import Text.HTML.TagSoup (Tag(TagText, TagOpen), maybeTagText, parseTags, isTagText, partitions, (~==), (~/=))
import Text.Read (readMaybe)
import Types (Site(Ksl, CraigsList), Listing(KslListing, CraigsListListing), SearchTerm(SearchTerm), Subdomain(Subdomain), ListingsMap, SiteSearchTuple, Environment)

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
      case mListings of
        Just listings -> pure listings
        Nothing -> do
          putStrLn "Had trouble parsing KSL Listings!"
          pure []
    Nothing -> do
      putStrLn "Couldn't find response body!"
      pure []
  where
    isListingsTag :: Tag BS.ByteString -> Bool
    isListingsTag (TagText s') = BS.pack "renderSearchSection" `BS.isInfixOf` s'
    isListingsTag _ = False

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
    toCraigsListListing (Just [price, url, title]) = Just $ CraigsListListing (decodeUtf8 (BS.drop 8 url)) (decodeUtf8 title) (decodeUtf8 price)
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
    f (KslListing id price title _ name maybeHomePhone) = "ksl.com/classifieds/listing/" <> pack (show id) <> "\n" <> title <> "\n" <> pack (show price) <> "\n" <> name <> "\n" <> fromMaybe "" maybeHomePhone <> "\n"
    f (CraigsListListing url title price) = url <> "\n" <> title <> "\n" <> price <> "\n"

getListings :: Environment -> StateT ListingsMap IO ListingsMap
getListings (sendgridApiKey, mailAddr, siteSearchTuples) = forever $ do
  -- If it's the middle of the night, I don't want this bothering me...
  time <- liftIO getCurrentTime
  let currentHourUtc = formatTime defaultTimeLocale "%H" time
  let currentMinutesUtc = formatTime defaultTimeLocale "%M" time
  let maybeRemaining = getRemainingTime <$> (readMaybe currentHourUtc :: Maybe Int) <*> (readMaybe currentMinutesUtc :: Maybe Int)

  case maybeRemaining of
    Nothing -> liftIO $ do
      putStrLn "Failed to parse the current time!"
      exitFailure
    Just (0, 0) -> pure ()
    Just (remainingHours, remainingMinutes) -> liftIO $ do
      putStrLn ("Shutting down for the night... Will start back up in " <> show remainingHours <> " hours and " <> show remainingMinutes <> ".")
      threadDelay $ (remainingHours * 60 * 60 * oneSecond) + (remainingMinutes * 60 * oneSecond)

  -- It's not the middle of the night, so let's start checking listings
  -- The process is simple
  -- 1) Grab the old listings from the last run
  -- 2) Go through all of our site search tuples and get all the new listings
  -- 3) Diff the listings. If there's new ones, then email them to me.
  -- 4) Set the new listings in the state to compare to in the next cycle.
  -- 5) Wait 30-60 seconds and then repeat the process
  prevListings <- get

  results <- liftIO $ mapM (group prevListings) siteSearchTuples

  -- TODO: Get a better diff of CraigsList results. There's a big problem with it.
  let diffContent = foldr (\(_, _, _, diff) prev -> prev <> getMailContent diff) "" results
  let newListings = unions $ foldr (\(s, s', new, _) prev -> prev <> [singleton (s, s') new]) [] results

  liftIO $ when (length diffContent > 1) $ do
    statusCode <- sendMail sendgridApiKey (createMail mailAddr diffContent)
    print ("Sent email: " <> show statusCode)

  liftIO $ when (length diffContent == 0) $ putStrLn "No new listings yet..."

  put newListings

  randomDelay <- liftIO $ getStdRandom (randomR (30, 60))
  liftIO $ threadDelay $ oneSecond * randomDelay
  where
    group :: ListingsMap -> SiteSearchTuple -> IO (Site, SearchTerm, [Listing], [Listing])
    group p (s, s') =  do
      randomNum <- getStdRandom $ randomR (1, 5)
      threadDelay $ oneSecond * randomNum
      new <- handleSites s s'
      let old = p ! (s, s')
      pure (s, s', new, diffListings old new)

    oneSecond :: Int
    oneSecond = 1000000

    getRemainingTime :: Int -> Int -> (Int, Int)
    getRemainingTime utcHour utcMinutes
      | utcHour >= 13 || utcHour <= 3 = (0, 0)
      | otherwise = ((utcHour * (-1)) + 12, (utcMinutes - 60) * (-1))

environment :: IO (Maybe [String])
environment = fmap sequence $ sequence $ fmap lookupEnv ["SENDGRID_API_KEY", "PHONE_NUMBER", "CRAIGSLIST_SUBDOMAINS", "CRAIGSLIST_SEARCH_TERMS", "KSL_SEARCH_TERMS"]

main :: IO ()
main = do
  maybeEnv <- environment
  let env = fmap pack <$> maybeEnv
  case env of
    Just [apiKey, phoneNumber, clSubdomains, clSearchTerms, kslSearchTerms] -> do
      let kslTuples = fmap (,) [Ksl] <*> fmap SearchTerm (split (== ',') kslSearchTerms)
      let clTuples = fmap ((,) . CraigsList . Subdomain) (split (== ',') clSubdomains) <*> fmap SearchTerm (split (== ',') clSearchTerms)
      let siteSearchTuples = kslTuples <> clTuples
      let initialState = unions $ fmap (\s -> singleton s []) siteSearchTuples
      let massagedEnv = (ApiKey apiKey, MailAddress (phoneNumber <> "@txt.att.net") "Deal Finder", siteSearchTuples)
      _ <- runStateT (getListings massagedEnv) initialState
      exitSuccess
    _ -> do
      putStrLn "Environment not set properly."
      exitFailure
