{-
Assume VMC Performer uses 192.168.10.3:39540 to send data,
and read them forever

VMC Performerが 192.168.10.3:39540 にデータを送信している前提で,
送られてきたデータを永続的に表示します
-}
module Main where

import Pipes
import qualified Pipes.Prelude as P
import Pipes.VMCP.Marionette
import Sound.Osc.Transport.Fd (withTransport)
import Sound.Osc (udp_server)

main = withTransport (udp_server 39540) $ \udp ->
  runEffect $ recvMarionetteMsgWithUdp udp >-> P.print
