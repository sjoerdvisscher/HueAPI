import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Time.Clock
import Data.Map.Strict (toList)
import System.Random

import HueAPI

lightSeq :: [String]
lightSeq = map show [22,18,21,15,26,19,23,20,16,17,13,12,10,11,4,7,1,3,9,2,5,6,8,14,24,25,27,28,29 :: Int]

kitt :: Hue ()
kitt = forever $ forM (zip [0..] $ drop 14 lightSeq) $ \(i, nm) -> do
  time <- liftIO getCurrentTime
  let t = fromRational . toRational . utctDayTime $ time
  let m = abs (15 * (sin t + 1) / 2 - fromInteger i) < (1.7 :: Double)
  updateLight nm LightState { on = True, bri = if m then 255 else 0, hue = 0, sat = 255 }

candleLight :: Hue ()
candleLight = do
  ls <- toList . lights <$> getState
  forever $ do
    i <- liftIO $ randomRIO (0, length ls - 1)
    b <- liftIO $ randomRIO (0, 20)
    h <- liftIO $ randomRIO (10000, 15000)
    s <- liftIO $ randomRIO (150, 255)
    updateLight (fst $ ls !! i) LightState { on = True, bri = b, hue = h, sat = s }

main :: IO ()
main = do
  void $ forkIO $ runHue "10.42.9.38" "haskellforhue" $ do
    forM_ lightSeq $ \nm -> initLight nm LightState { on = False, bri = 0, hue = 0, sat = 255 }
    kitt
  void getLine
  void $ forkIO $ runHue "10.42.9.38" "haskellforhue" candleLight
  void getLine
