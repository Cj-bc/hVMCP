module Pipes.VMCP.Marionette where

import Control.Monad (forM_)
import Pipes
import Sound.OSC
import Sound.OSC.Transport.FD.UDP (udpServer)
import qualified Sound.OSC.Transport.FD as FD
import Data.VMCP.Marionette

-- | Produce 'MarionetteMsg' continuously 
--
-- TODO: Don't reopen udpServer each time. I think it's better implementation.
recvMarionetteMsg :: String -> Int -> Producer MarionetteMsg IO ()
recvMarionetteMsg addr p = do
  bundles <- lift $ FD.withTransport (udpServer addr p) (fmap fromOSCBundle . FD.recvBundle)
  forM_ bundles each
  recvMarionetteMsg addr p
