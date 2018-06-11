module Main where

import Prelude (Show, Eq, Bool(False), Maybe(Just, Nothing), IO, print, putStrLn, pure, sequence, fmap, (<$>), ($), (>>=), (*), (=<<), (/=), (.), (&&))
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
import Types.Ksl (Listing)
import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (defaultConfig)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
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
      -- let isListingsTag (TagText t) = Data.ByteString.Char8.pack "renderSearchSection" `Data.ByteString.Char8.isInfixOf` t
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
  print s
  putStrLn "Checking Facebook Marketplace"
  pure []

handleSites LetGo (SearchTerm s) = do
  print s
  putStrLn "Checking LetGo"
  pure []

handleSites OfferUp (SearchTerm s) = do
  print s
  putStrLn "Checking OfferUp"
  pure []

createMail :: MailAddress -> Text -> Mail () ()
createMail addr content =
  mail [personalization (fromList [addr])] addr "Possible Deal" (fromList [mailContentText content])

diffListings :: Site -> [Listing] -> [Listing] -> [Listing]
-- If one of these is empty, then something has gone wrong, or it's the first call, so we can't get a good diff
diffListings _ [] _ = []
diffListings _ _ [] = []

diffListings _ a b = b \\ a
-- diffListings FacebookMarketplace a b = b \\ a
-- diffListings OfferUp a b = b \\ a
-- diffListings LetGo a b = b \\ a

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

  -- Compare the two here
  liftIO $ print $ diffListings Ksl oldKslListings newKslListings
  -- liftIO $ print statusCode
  -- statusCode <- liftIO $ sendMail sendgridApiKey (createMail mailAddr "This is the content")

  put $ unions [ singleton Ksl newKslListings
               , singleton FacebookMarketplace newFbmpListings
               , singleton OfferUp newOuListings
               , singleton LetGo newLgListings
               ]

  let oneSecond = 1000000
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
  _ <- loadFile defaultConfig
  let combine key pn = [key, fmap (<> "@txt.att.net") pn]
  vars <- lookupEnv "SENDGRID_API_KEY" >>= (\key -> combine key <$> lookupEnv "PHONE_NUMBER")
  let env = fmap pack <$> sequence vars
  _ <- case env of
      Nothing -> do
        putStrLn "Environment not set properly."
        exitFailure
      Just [apiKey, email] ->
        let massagedEnv = (ApiKey apiKey, MailAddress email "Deal Finder")
        in runStateT (getListings massagedEnv) initialState
      Just _ -> do
        putStrLn "Environment not set properly."
        exitFailure
  pure ()
