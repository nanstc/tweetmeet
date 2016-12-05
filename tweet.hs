{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Maybe
import Data.Char
import GHC.Generics

-- The Twitter API credentials of our app

myoauth :: OAuth
myoauth = newOAuth 
           { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "TEjYdE3Ac3s8UtWr3n6PVojFw"
           , oauthConsumerSecret = "qcc67ceJfSOd9x1aSx02nUIarGmmZDpzishy79gUl8ZxwhYgHq"
           }

mycred :: Credential
mycred = newCredential "727254860375052289-ta0l05o8hwvycLDfdyc0loQPaaHUiQk"
                       "3WW4dDQMzpkHSOIVL3O7jRsZ3GHcZJjWdyf5KtTR3V5fc"

-- For documentation on the availalbe Tweeter API
-- fields used in parser please refer to 
-- https://dev.twitter.com/docs/platform-objects/tweets

-- data type of a tweet that we display on screen
data Tweet =
  Tweet { text :: !Text
        , retweet_count :: !Integer
        , favorite_count :: !Integer
        , created_at :: !String
          } deriving (Show, Generic)
          
instance FromJSON Tweet where
 parseJSON (Object v) =
    Tweet  <$> v .: "text"
           <*> v .: "retweet_count"
           <*> v .: "favorite_count"
           <*> v .: "created_at"

-- Parser to parse a list of tweets in json forms
-- , specfically for hashtag handler
tweets :: Value -> Parser [Tweet]
tweets  = withObject "tweets" (.: "statuses")


-- output tweet information on screen
format :: Tweet -> IO()
format tweet = do 
   putStrLn $ "Tweet: " ++ (show $ text tweet)
   putStrLn $ "Favourite count: " ++ (show $ favorite_count tweet)
   putStrLn $ "Retweet count: " ++ (show $ retweet_count tweet)
   putStrLn $ "Posted at: " ++ (show $ created_at tweet)
   putStrLn $ ""


-- A function that calls Tweeter API, receives 
-- timeline JSON based on user handle, and calls
-- function that parses the JSON to return tweets
-- , or error if parser fails 
timeline :: String -- the handle user is interested in
         -> IO (Either String [Tweet]) -- left String = error message
                                       -- right [Tweet] = proper output
timeline name = do
  -- create a HTTP request and binds it to req
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  -- Use a HTTP manager to authenticate the request, and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  return $ eitherDecode $ responseBody res
  
  
-- A function that calls Tweeter API, receives 
-- hashtag search result JSON based on hashtag, 
-- and calls function that parses the JSON to 
-- return tweets, or error if parser fails 

searchResult :: String -- the hashtag user is interested in
         -> String
         -> IO (Either String [Tweet]) -- left String = error message
                                       -- right [Tweet] = proper output
         
searchResult name counts = do
  -- create a HTTP request and binds it to req
  req <- parseUrl $ "https://api.twitter.com/1.1/search/tweets.json?q=%23" ++ name ++ "&result_type=popular&count="++counts
  -- Use a HTTP manager to authenticate the request, and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  -- Set error message as Left variable
  let err = Left "Cannot parse the result from search." :: Either String [Tweet]
  -- Parse array of twitter in the "statuses" tag in JSON and set as Right variable
  let twts = Right $ fromJust $ parseMaybe tweets  =<< decode (responseBody res) :: Either String [Tweet]
  return twts
  

-- A simple function to get rid of leading '@' in input, if at all 
parsehandle [] = []
parsehandle (h:t)
    | h == '@' = parsehandle t
    | otherwise = h:parsehandle t
    
-- A simple function to get rid of leading '#' in input, if at all 
parsehashtag [] = []
parsehashtag (h:t)
    | h == '#' = parsehashtag t
    | otherwise = h:parsehashtag t
    
-- The function that gives back tweets based on handles 
handle_handling :: IO ()
handle_handling = do 
    putStrLn "Enter a handle you would like to search:"
    h <- getLine
    putStrLn "What is the maximum number of tweets you would like to see?"
    n <- getLine
    -- Calls the timeline function to get formatted tweets
    ets <- timeline $ parsehandle h
    putStrLn $ ""
    putStrLn $ "Searched handle: @" ++ h
    putStrLn $ ""
    case ets of
        -- When the parsing of the JSON data fails, we report it.
        Left err -> putStrLn err
        -- When successful, print in the screen the number of tweets user specified
        Right ts  -> mapM_ format $ take (read n :: Int) ts

-- The function that gives back tweets based on hashtags 
hashtag_handling :: IO ()
hashtag_handling = do 
    putStrLn "Enter the hashtag you would like to look up:"
    h <- getLine
    putStrLn "What is the maximum number of tweets you would like to see?"
    n <- getLine
    -- Calls the searchResult function to get formatted tweets
    ets <- searchResult (parsehashtag h) n
    putStrLn $ ""
    putStrLn $ "Searched hashtag: #" ++ h
    putStrLn $ ""
    case ets of 
        -- When the parsing of the JSON data fails, we report it.
        Left err -> putStrLn err
        -- When successful, print in the screen the number of tweets user specified
        Right ts -> mapM_ format $ take (read n :: Int) ts

-- Function that greets users when entering system
greeting :: IO ()
greeting = do 
  putStrLn "Welcome to Tweetmeet! This mini-app provides you live tweets of your faviourite subject topics."
  putStrLn "Would you like to search based on @userhandle, or #hashtag? (enter @ or #)"
  
-- -- | The main function to kick-start the app
main :: IO ()
main = do
  greeting
  symbol <- getLine
  if (symbol == "@") then handle_handling else hashtag_handling
