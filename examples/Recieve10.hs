{-

Assume VMC Performer uses 192.168.10.3:39540 to send data,
and read first 10 data sent from there.

VMC Performerが 192.168.10.3:39540 にデータを送信している前提で,
送られてきたデータの最初の10つを表示します。
-}
module Main where

import Pipes
import qualified Pipes.Prelude as P
import Pipes.VMCP.Marionette

main = runEffect $ recvMarionetteMsg "192.168.10.3" 39540
       >-> P.take 10
       >-> P.print
