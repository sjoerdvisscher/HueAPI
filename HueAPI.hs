 {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module HueAPI (
  
    HueData(..)
  , Light(..)
  , LightState(..)
  , Group(..)
  , Name
  
  , HueMonad
  , runHueMonad
  
  , getLightState
  , updateLight
  , initLight
  
) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as B (breakSubstring, null)
import qualified Data.ByteString.Lazy as B hiding (null)
import Network.HTTP.Conduit
import Network
import Data.Map.Strict (Map, toList, (!), adjust)
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


type HueMonad = StateT HueData (ReaderT String IO)

runHueMonad :: String -> String -> HueMonad a -> IO a
runHueMonad host key api =
  let url = "http://" ++ host ++ "/api/" ++ key ++ "/" in
  withSocketsDo $ do
    connect key host
    request' <- parseUrl url
    let request = request' { responseTimeout = Nothing }
    resp <- withManager $ httpLbs request
    d <- either fail return (eitherDecode (responseBody resp))
    runReaderT (fst <$> runStateT api d) url


getLightState :: Name -> HueMonad LightState
getLightState name = do
  d <- get
  return $ state $ lights d ! name


updateLight :: Name -> LightState -> HueMonad ()
updateLight name l = do
  l' <- getLightState name
  when (on l /= on l') $
    updateLightProp name "on" (if on l then "true" else "false")
  when (on l) $ do
    when (bri l /= bri l') $
      updateLightProp name "bri" (show $ bri l)
    when (hue l /= hue l') $
      updateLightProp name "hue" (show $ hue l)
    when (sat l /= sat l') $
      updateLightProp name "sat" (show $ sat l)
  d <- get
  put $ d { lights = adjust (\light -> light { state = if on l then l else l' { on = False } }) name (lights d) }


initLight :: Name -> LightState -> HueMonad ()
initLight name l = do
  updateLightProp name "on" "true"
  updateLightProp name "bri" (show $ bri l)
  updateLightProp name "hue" (show $ hue l)
  updateLightProp name "sat" (show $ sat l)
  updateLightProp name "on" (if on l then "true" else "false")
  d <- get
  put $ d { lights = adjust (\light -> light { state = l }) name (lights d) }
  
  
updateLightProp :: Name -> String -> String -> HueMonad ()
updateLightProp name prop value = do
  url <- ask
  resp <- liftIO $ withSocketsDo $ do
    initReq <- parseUrl $ url ++ "lights/" ++ name ++ "/state"
    let request = initReq {
        requestBody = RequestBodyLBS (B.pack $ map(toEnum.fromEnum) $ "{\"" ++ prop ++ "\":" ++ value ++ "}")
      , method = "PUT"
      , responseTimeout = Nothing
      }
    withManager (httpLbs request)
  if B.null . snd . B.breakSubstring "Internal error, 503" . B.toStrict $ responseBody resp then return () else do
    liftIO $ print resp
    liftIO $ threadDelay 100000
    updateLightProp name prop value

connect :: String -> String -> IO ()
connect key host = do
  initReq <- parseUrl $ "http://" ++ host ++ "/api/"
  let request = initReq {
      requestBody = RequestBodyLBS (B.pack $ map(toEnum.fromEnum) $
        "{\"username\":\"" ++ key ++ "\",\"devicetype\":\"Unknown\"}")
    , method = "POST"
    , responseTimeout = Nothing
    }
  resp <- withManager (httpLbs request)
  if B.null . snd . B.breakSubstring "error" . B.toStrict $ responseBody resp then return () else do
    liftIO $ print resp
    liftIO $ threadDelay 100000
    connect key host