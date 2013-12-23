{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language LambdaCase #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveDataTypeable #-}
{-# Language NamedFieldPuns #-}
module Main where

import qualified Config.OAuth as Config

import Control.Exception (Exception)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Failure

import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Data.Vector ((!), (!?), Vector)
import Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Aeson.Types (Parser)
import Data.Aeson (Value(..))
import qualified Data.Aeson as JSON

import System.IO (hFlush, stdout)

import Web.Authenticate.OAuth
import Network.HTTP.Types
import Network.HTTP.Conduit
import Network (withSocketsDo)

data JSONError = JSONError String deriving (Eq, Show, Typeable)
instance Exception JSONError 

oauth :: OAuth
oauth = newOAuth
  { oauthServerName = "twitter"
  , oauthRequestUri = "https://twitter.com/oauth/request_token"
  , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
  , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
  , oauthSignatureMethod = HMACSHA1
  , oauthConsumerKey = Config.consumerKey
  , oauthConsumerSecret = Config.consumerSecret
  , oauthVersion = OAuth10a
  }

requestUrl :: String -> String
requestUrl endpoint = "https://api.twitter.com/1.1/" ++ endpoint ++ ".json"

query :: Method -> Credential -> String -> SimpleQuery -> IO Value
query httpMethod cred api qry = withManager $ \manager -> do
  requestBase <- parseUrl (requestUrl api)
  let requestModified = requestBase
                          { method = httpMethod
                          , queryString = renderSimpleQuery True qry}
  liftIO $ print requestModified >> hFlush stdout
  requestSigned <- signOAuth oauth cred requestModified
  response <- httpLbs requestSigned manager
  either fail return $ JSON.eitherDecode (responseBody response)
  
getQuery, postQuery :: Credential -> String -> SimpleQuery -> IO Value
getQuery = query methodGet
postQuery = query methodPost

postQuery' :: Credential -> String -> SimpleQuery -> IO ()
postQuery' cred api qry = query methodPost cred api qry >> return ()

data Mention = Mention
  { user :: T.Text
  , text :: T.Text
  , tweetId :: Integer
  } deriving (Eq, Show)

lookupE :: Value -> T.Text -> Either String Value
lookupE (Object obj) key = case H.lookup key obj of
        Nothing -> Left $ "key " ++ show key ++ " not present"
        Just v  -> Right v
lookupE _ _             = Left $ "not an object"

(.:*) :: (JSON.FromJSON a) => Value -> [T.Text] -> Parser a
(.:*) value = JSON.parseJSON <=< foldM ((either fail return .) . lookupE) value

instance JSON.FromJSON Mention where
  parseJSON (obj@(Object _)) = Mention <$>
                           obj .:* ["user", "screen_name"] <*>
                           obj .:* ["text"] <*>
                           obj .:* ["id"]
  parseJSON _ = mzero

mentionsFromJSON :: (Failure JSONError m, Monad m) => Value -> m (Vector Mention)
mentionsFromJSON (Array ary) = V.mapM extractMention ary
  where
    extractMention (JSON.fromJSON -> JSON.Success mention) = return mention
    extractMention _ = failure (JSONError "mentionsFromJSON.extractMention")
mentionsFromJSON _ = failure (JSONError "mentionsFromJSON")

pickTopMention :: (Failure JSONError m, Monad m) => Value -> m Mention
pickTopMention = \case
  Array (JSON.fromJSON.(! 0) -> JSON.Success topMention) -> return topMention
  _ -> failure (JSONError "pickTopMention")

getNewMentions :: Credential -> Integer -> IO (Vector Mention)
getNewMentions cred sinceId =
  mentionsFromJSON =<< getQuery cred "statuses/mentions_timeline" [("since_id", BS.pack $ show sinceId)]

parseTimeSetting :: T.Text -> T.Text
parseTimeSetting text = "day: " <> d <> " & hour: " <> h
  where (d, h) = T.span (/=':') text

checkAndRegister :: Credential -> Mention -> IO ()
checkAndRegister cred Mention{user, text} = do
  let ws = T.words text
      date = parseTimeSetting $ ws!!1
      content = T.unwords (tail . tail $ ws)
      post = "@" <> user
                 <> " Accept: " <> content
                 <> " at [" <> date <> "]"
  postQuery' cred "statuses/update" [("status", encodeUtf8 $ post)]

mainLoop :: Credential -> Integer -> IO ()
mainLoop cred sinceId = do
  mentions <- getNewMentions cred sinceId
  case mentions!?0 of
    Nothing -> do threadDelay (70*1000000)
                  mainLoop cred sinceId
    Just (tweetId -> newSinceId) ->
      do V.mapM_ (checkAndRegister cred) mentions
         threadDelay (70*1000000)
         mainLoop cred newSinceId

main :: IO ()
main = withSocketsDo $ do
  let cred = newCredential Config.accessToken Config.accessTokenSecret
  getQuery cred "statuses/mentions_timeline" [] >>= \case
    (pickTopMention -> Just topMention) -> mainLoop cred (tweetId topMention)
    _ -> putStrLn "Error: failed to get mentions."
