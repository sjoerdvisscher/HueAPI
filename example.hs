import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Time.Clock
import Data.Map.Strict (toList)
import System.Random

import HueAPI


lightSeq = map show [22,18,21,15,26,19,23,20,16,17,13,12,10,11,4,7,1,3,9,2,5,6,8,14,24,25,27,28,29]

kitt :: HueMonad ()
kitt = do
  forM (zip [0..] lightSeq) $ \(i, name) -> do
    time <- liftIO getCurrentTime
    let t = fromRational . toRational . utctDayTime $ time
    let m = sin(t * 2 + fromInteger i / 3) > 0.5
    let h = floor $ 32000 * (1 + sin (t / 10))
    updateLight name LightState { on = True, bri = if m then 255 else 0, hue = 10000, sat = 255 }
  kitt

candleLight :: HueMonad ()
candleLight = do
  ls <- lights <$> getState
  forM (toList ls) $ \(name, _) -> do
    b <- liftIO $ randomRIO (0, 20)
    h <- liftIO $ randomRIO (10000, 15000)
    s <- liftIO $ randomRIO (150, 255)
    updateLight name LightState { on = True, bri = b, hue = h, sat = s }
  candleLight

main :: IO ()
main = do
  forkIO $ runHueMonad "10.42.9.38" "haskellforhue" $ do
    forM lightSeq $ \name -> initLight name LightState { on = True, bri = 0, hue = 10000, sat = 255 }
    kitt
  void getLine
  forkIO $ runHueMonad "10.42.9.38" "haskellforhue" candleLight
  void getLine