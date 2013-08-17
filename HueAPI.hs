 {-# LANGUAGE OverloadedStrings, DeriveGeneric, DoAndIfThenElse #-}
module HueAPI (
  
    HueData(..)
  , Light(..)
  , LightState(..)
  , Group(..)
  , Name
  
  , Hue
  , runHue
  
  , getState
  , getLightState
  , updateLight
  , initLight
  
) where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Conduit
import Network
import Data.Map.Strict (Map, fromList, (!), adjust)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (StateT(..), get, put)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Concurrent


type Name = String

data HueData = Hue
  { lights :: Map Name Light
  , groups :: Map Name Group
  } deriving (Show, Generic)
  
data Light = Light
  { state :: LightState
  , name :: Name
  , modelid :: String
  , swversion :: String
  } deriving (Show, Generic)
  
data LightState = LightState
  { on :: Bool
  , bri :: Int
  , hue :: Int
  , sat :: Int
  } deriving (Show, Generic)

data Group = Group
  { action :: LightState
  , groupName :: Name
  , groupLights :: [Name]
  } deriving (Show)

instance FromJSON HueData
instance FromJSON Light
instance FromJSON LightState
instance FromJSON Group where
  parseJSON (Object v) = Group <$> v .: "action" <*> v .: "name" <*> v .: "lights"

data HueResult = HueResult HueError deriving Show
instance FromJSON HueResult where
  parseJSON (Object v) = HueResult <$> v .: "error"
data HueError = HueError Int String deriving Show
instance FromJSON HueError where
  parseJSON (Object v) = HueError <$> v .: "type" <*> v .: "description"


type Hue = StateT HueData (ReaderT String IO)

runHue :: String -> String -> Hue a -> IO a
runHue host key hm = do
    hueData <- withSocketsDo go
    runReaderT (fst <$> runStateT hm hueData) url
  where
    url = "http://" ++ host ++ "/api/" ++ key ++ "/"
    go = do
      request' <- parseUrl url
      let request = request' { responseTimeout = Nothing }
      resp <- withManager $ httpLbs request
      either doConnect return $ eitherDecode $ responseBody resp
    doConnect _ = do
      putStrLn "Press the link button on the base station"
      connect key host
      go
      

getState :: Hue HueData
getState = get

getLightState :: Name -> Hue LightState
getLightState name = do
  d <- get
  return $ state $ lights d ! name


updateLight :: Name -> LightState -> Hue ()
updateLight name l = do
  l' <- getLightState name
  if on l then do
    when (not $ on l') $ updateLightProps name [("on", True)]
    updateLightProps name $
         [("bri", toJSON $ bri l)|bri l /= bri l']
      ++ [("hue", toJSON $ hue l)|hue l /= hue l']
      ++ [("sat", toJSON $ sat l)|sat l /= sat l']
  else
    when (on l') $ updateLightProps name [("on", False)]
  d <- get
  put $ d { lights = adjust (\light -> light { state = if on l then l else l' { on = False } }) name (lights d) }


initLight :: Name -> LightState -> Hue ()
initLight name l = do
  updateLightProps name [("on", True)]
  updateLightProps name $
    [ ("bri", toJSON $ bri l)
    , ("hue", toJSON $ hue l)
    , ("sat", toJSON $ sat l)
    ]
  updateLightProps name [("on", on l)]
  d <- get
  put $ d { lights = adjust (\light -> light { state = l }) name (lights d) }
  
map2json :: ToJSON a => [(String, a)] -> RequestBody m
map2json = RequestBodyLBS . encode . fromList
  
updateLightProps :: ToJSON a => Name -> [(String, a)] -> Hue ()
updateLightProps _ [] = return ()
updateLightProps name m = do
  url <- ask
  resp <- liftIO $ withSocketsDo $ do
    initReq <- parseUrl $ url ++ "lights/" ++ name ++ "/state"
    let request = initReq {
        requestBody = map2json m
      , method = "PUT"
      , responseTimeout = Nothing
      }
    withManager (httpLbs request)
  case eitherDecode $ responseBody resp of
    Right [HueResult (HueError i msg)] -> do
      when (i /= 901) $ -- Don't print internal server errors
        liftIO $ putStrLn $ "Error " ++ show i ++ ": " ++ msg
      liftIO $ threadDelay 100000
      updateLightProps name m
    _ -> return ()

connect :: String -> String -> IO ()
connect key host = do
  initReq <- parseUrl $ "http://" ++ host ++ "/api/"
  let request = initReq {
      requestBody = map2json
        [ ("username", key)
        , ("devicetype", "Unknown")
        ]
    , method = "POST"
    , responseTimeout = Nothing
    }
  resp <- withManager (httpLbs request)
  case eitherDecode $ responseBody resp of
    Right [HueResult (HueError i msg)] -> do
      when (i /= 101) $ -- Don't print "press the button" errors
        putStrLn $ "Error " ++ show i ++ ": " ++ msg
      threadDelay 100000
      connect key host
    _ -> return ()