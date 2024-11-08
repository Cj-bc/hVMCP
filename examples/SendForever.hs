{- |
Module      :  Main
Description :  Example program that sends motion forever
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

It will send message that rotate the VRM forever.
-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Monad (forM)
import Data.Bool (bool)
import Data.List (iterate')
import Data.VMCP.Marionette (MarionetteMsg(RootTransform))
import Pipes.VMCP.Marionette
import Pipes
import Sound.Osc (sleepThread)
import Linear.V3 (V3(..))
import Linear.Quaternion (axisAngle)

-- | Create 'RootTransform' massage from rotation angle (in theta)
createMsg :: Float -> MarionetteMsg
createMsg theta = RootTransform (V3 0 0 0) q
  where
    q = axisAngle (V3 0 1 0) theta

-- | Sleep given @sec@ seconds between each yielding
insertSleep :: MonadIO m => Float -> Pipe a a m r
insertSleep sec = do
  a <- await
  yield a
  liftIO $ sleepThread sec
  insertSleep sec

main = runEffect $ (each $ createMsg <$> iterate' (\v -> bool (v + (pi/180)) 0 (v > (2*pi))) 0)
       >-> insertSleep 0.01 >-> sendMarionetteMsg "127.0.0.1" 39542
