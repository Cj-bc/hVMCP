module Pipes.VMCP.Marionette where

import Control.Monad (forM_)
import Pipes
import Sound.OSC
import Sound.OSC.Coding.Encode.Builder (encodeBundle)
import Sound.OSC.Transport.FD.UDP (udpServer, openUDP)
import qualified Sound.OSC.Transport.FD as FD
import Data.VMCP.Marionette
import qualified Data.ByteString.Lazy as L

-- | Each OSC Bundle should be smaller than this value in byte.
--
--
-- > パケットは適切な範囲(1500byte以内)でbundle化されており、受信者は適切に扱う必要があります。
-- URL: https://protocol.vmc.info/specification
oneBundleMaxByteSize = 1500

-- | Optimized function to calculate size of 'Bundle'
calcBundleSize :: Bundle -> Int
calcBundleSize = fromIntegral . L.length . encodeBundle -- TODO: No Optimization is done yet

-- | Produce 'MarionetteMsg' continuously 
--
-- TODO: Don't reopen udpServer each time. I think it's better implementation.
recvMarionetteMsg :: String -> Int -> Producer MarionetteMsg IO ()
recvMarionetteMsg addr p = do
  bundles <- lift $ FD.withTransport (udpServer addr p) (fmap fromOSCBundle . FD.recvBundle)
  forM_ bundles each
  recvMarionetteMsg addr p

-- | Send 'MarionetteMsg' to given addr/port.
--
-- It will automatically bundle some 'MarionetteMsg's into 'Budle'
--
-- TODO: This implementation is tedious and could be performance issue.
-- We have to find good way to calculate length of OSC bundle in bytes without
-- actually convert them into ByteString every time.
sendMarionetteMsg :: String -> Int -> Consumer MarionetteMsg IO ()
sendMarionetteMsg addr p = do
  messages <- accum_data []
  lift $ FD.withTransport (openUDP addr p) (flip FD.sendBundle (toOSCBundle messages)) 
  sendMarionetteMsg addr p
  where
    -- | Accumulate 'MarionetteMsg's
    accum_data :: [MarionetteMsg] -> Consumer MarionetteMsg IO [MarionetteMsg]
    accum_data ds = do
      message <- await
      let new_ds = message:ds
          bSize = calcBundleSize (Bundle 0 . fmap toOSCMessage $ new_ds)
      if bSize >= 1300 -- 適当な値。1500を越える前に送る必要があるので,最終的には「最も大きいメッセージのバイト数」を引く
        then return ds
        else accum_data new_ds
