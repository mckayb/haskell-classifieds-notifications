module Main where

import Prelude (Show, Eq, Bool(False), Maybe(Just, Nothing), IO, print, putStrLn, pure, sequence, fmap, foldr, show, (<$>), ($), (>>=), (*), (=<<), (/=), (.), (&&))
import Control.Lens ((.~), (&), (^?))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get, put)
import Control.Concurrent (threadDelay)
import Data.HashMap.Lazy (HashMap, singleton, unions, (!))
import Data.Semigroup ((<>))
import Data.List (find, (\\))
import Data.List.NonEmpty (fromList)
import Data.Text (Text, pack)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 (ByteString, takeWhile, dropWhile, isInfixOf, init, pack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.Wreq (getWith, defaults, param, responseBody)
import Text.HTML.TagSoup (Tag(TagText), maybeTagText, parseTags, isTagText)
import Types.Ksl (Listing(KslListing))
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Network.SendGridV3.Api (ApiKey(ApiKey) , MailAddress(MailAddress) , Mail , sendMail , personalization , mail , mailContentText)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype SearchTerm = SearchTerm Text
  deriving (Show, Eq)

data Site = Ksl | FacebookMarketplace | LetGo | OfferUp
  deriving (Show, Eq, Generic)

instance Hashable Site

type ListingsMap = HashMap Site [Listing]
type Environment = (ApiKey, MailAddress)

-- https://www.ebay.com/sch/i.html?_from=R40&_nkw=arcade&_sacat=0&_trksid=p2380057.m570.l1313.TR10.TRC2.A0.H0.Xarcade.TRS2
-- https://www.ksl.com/classifieds/search/?keyword=arcade&zip=&miles=25&priceFrom=&priceTo=&marketType%5B%5D=Sale&city=&state=&sort=0

isListingsTag :: Tag Data.ByteString.Char8.ByteString -> Bool
isListingsTag (TagText s) = Data.ByteString.Char8.pack "renderSearchSection" `Data.ByteString.Char8.isInfixOf` s
isListingsTag _ = False

handleSites :: Site -> SearchTerm -> IO [Listing]
handleSites Ksl (SearchTerm s) = do
  let opts = defaults & param (pack "keyword") .~ [s]
  r <- getWith opts "https://www.ksl.com/classifieds/search/"
  let maybeBody = r ^? responseBody
  case maybeBody of
    Just body -> do
      let listingTagText = find (\x -> isTagText x && isListingsTag x) $ parseTags (toStrict body)
      let maybeListingText = maybeTagText =<< listingTagText
      let maybeListingsJson = fmap ( Data.ByteString.Char8.init 
                                   . Data.ByteString.Char8.takeWhile (/= '\n') 
                                   . Data.ByteString.Char8.dropWhile (/= '[')
                                   ) maybeListingText
      let mListings = (decode =<< fmap fromStrict maybeListingsJson) :: Maybe [Listing]
      pure $ fromMaybe [] mListings
    Nothing -> do
      putStrLn "Couldn't find response body!"
      pure []

handleSites FacebookMarketplace (SearchTerm s) = do
  -- print $ "Checking Facebook Marketplace: " <> s
  pure []

handleSites LetGo (SearchTerm s) = do
  -- print $ "Checking LetGo: " <> s
  pure []

handleSites OfferUp (SearchTerm s) = do
  -- print $ "Checking OfferUp: " <> s
  pure []

createMail :: MailAddress -> Text -> Mail () ()
createMail addr content =
  mail [personalization (fromList [addr])] addr "Possible Deal" (fromList [mailContentText content])

diffListings :: Site -> [Listing] -> [Listing] -> [Listing]
-- If one of these is empty, then something has gone wrong, or it's the first call, so we can't get a good diff
diffListings _ [] _ = []
diffListings _ _ [] = []
diffListings _ a b = b \\ a

-- "https://www.ksl.com/classifieds/listing/id-of-listing"
getMailContent :: Site -> [Listing] -> Text
getMailContent Ksl listings = foldr (\a b -> a <> b <> " <br>\n ") "" $ fmap f listings
  where f (KslListing id _ _ _ _ _ _ _ price title _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = "https://ksl.com/classifieds/listing/" <> pack (show id) <> " <br>\n " <> title <> " <br>\n " <> pack (show price)
getMailContent FacebookMarketplace _ = "Facebook Marketplace Content"
getMailContent LetGo _ = "LetGo Content"
getMailContent OfferUp _ = "OfferUp Content"

getListings :: Environment -> StateT ListingsMap IO ListingsMap
getListings (sendgridApiKey, mailAddr) = forever $ do
  prevListings <- get

  let oldKslListings = prevListings ! Ksl
  -- let oldFbmpListings = prevListings ! FacebookMarketplace
  -- let oldOuListings = prevListings ! OfferUp
  -- let oldLgListings = prevListings ! LetGo

  newKslListings <- liftIO $ handleSites Ksl $ SearchTerm "arcade"
  newFbmpListings <- liftIO $ handleSites FacebookMarketplace $ SearchTerm "arcade"
  newOuListings <- liftIO $ handleSites OfferUp $ SearchTerm "arcade"
  newLgListings <- liftIO $ handleSites LetGo $ SearchTerm "arcade"

  -- Collect the diff listings into email content
  -- liftIO $ putStrLn "New Ksl Listings: "
  -- liftIO $ print $ diffListings Ksl oldKslListings newKslListings
  let diffKsl = diffListings Ksl oldKslListings newKslListings
  let kslContent = getMailContent Ksl diffKsl
  case diffKsl of
    [] -> liftIO $ print "No changes."
    x -> liftIO $ do
      print "Old Listings: "
      print oldKslListings
      print "New Listings: "
      print newKslListings
      print "Diff Listings: "
      print diffKsl
      print kslContent

  -- statusCode <- liftIO $ sendMail sendgridApiKey (createMail mailAddr kslContent)

  put $ unions [ singleton Ksl newKslListings
               , singleton FacebookMarketplace newFbmpListings
               , singleton OfferUp newOuListings
               , singleton LetGo newLgListings
               ]

  let oneSecond = 1000000

  -- Check every 30 seconds
  liftIO $ threadDelay $ oneSecond * 30

initialState :: ListingsMap
initialState = unions [ singleton Ksl []
                      , singleton FacebookMarketplace []
                      , singleton OfferUp []
                      , singleton LetGo []
                      ]

-- phone#@txt.att.net

main :: IO ()
main = do
  let combine key pn = [key, fmap (<> "@txt.att.net") pn]
  vars <- lookupEnv "SENDGRID_API_KEY" >>= (\key -> combine key <$> lookupEnv "PHONE_NUMBER")
  let env = fmap pack <$> sequence vars
  case env of
    Nothing -> do
      putStrLn "Environment not set properly."
      exitFailure
    Just [apiKey, email] -> do
      let massagedEnv = (ApiKey apiKey, MailAddress email "Deal Finder")
      _ <- runStateT (getListings massagedEnv) initialState
      exitSuccess
    Just _ -> do
      putStrLn "Environment not set properly."
      exitFailure
