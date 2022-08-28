{- |
Module      :  Main
Description :  Tiny sample of recv&send combination 
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

Recieve marionette message and send it to other.
Use it after changing port numbers and addresses for your needs.
-}
module Main where
import Pipes
import Pipes.VMCP.Marionette

main = let in_port = 39540
           in_addr = "192.168.10.3"
           out_port = 39540
           out_addr = "192.168.10.6"
       in runEffect $ recvMarionetteMsg in_addr in_port >-> sendMarionetteMsg out_addr out_port
