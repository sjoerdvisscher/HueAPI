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
import Data.Aeson.Lens (_JSON)
import Data.Map.Strict (Map, fromList, (!), adjust)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (StateT(..), runStateT, get, put)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask)
import Control.Concurrent
import Control.Lens
import qualified Network.Wreq as Wreq

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
  } deriving (Show, Generic)

instance FromJSON HueData
instance FromJSON Light
instance FromJSON LightState
instance FromJSON Group where
  parseJSON (Object v) = Group <$> v .: "action" <*> v .: "name" <*> v .: "lights"

instance ToJSON HueData
instance ToJSON Light
instance ToJSON LightState
instance ToJSON Group

data HueResult = HueResult HueError deriving (Show, Generic)
instance FromJSON HueResult where
  parseJSON (Object v) = HueResult <$> v .: "error"
instance ToJSON HueResult
data HueError = HueError Int String deriving (Show, Generic)
instance FromJSON HueError where
  parseJSON (Object v) = HueError <$> v .: "type" <*> v .: "description"
instance ToJSON HueError


type Hue = StateT HueData (ReaderT String IO)

runHue :: String -> String -> Hue a -> IO a
runHue host key hm = do
    hueData <- go
    runReaderT (fst <$> runStateT hm hueData) url
  where
    url = "http://" ++ host ++ "/api/" ++ key ++ "/"
    go = do
      r <- Wreq.get url
      maybe doConnect return $ r ^? Wreq.responseBody . _JSON
    doConnect = do
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

updateLightProps :: ToJSON a => Name -> [(String, a)] -> Hue ()
updateLightProps _ [] = return ()
updateLightProps name m = do
  url <- ask
  resp <- liftIO $ Wreq.put (url ++ "lights/" ++ name ++ "/state") $ toJSON (fromList m)
  case resp ^? Wreq.responseBody . _JSON of
    Just [HueResult (HueError i msg)] -> do
      when (i /= 901) $ -- Don't print internal server errors
        liftIO $ putStrLn $ "Error " ++ show i ++ ": " ++ msg
      liftIO $ threadDelay 100000
      updateLightProps name m
    _ -> return ()

connect :: String -> String -> IO ()
connect key host = do
  resp <- Wreq.post ("http://" ++ host ++ "/api/") $ toJSON $ fromList
    [ ("username", key)
    , ("devicetype" :: String, "Unknown" :: String)
    ]
  case resp ^? Wreq.responseBody . _JSON of
    Just [HueResult (HueError i msg)] -> do
      when (i /= 101) $ -- Don't print "press the button" errors
        putStrLn $ "Error " ++ show i ++ ": " ++ msg
      threadDelay 100000
      connect key host
    _ -> return ()
