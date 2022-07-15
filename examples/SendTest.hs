{- |
Module      :  Main
Description :  Sample program to send 'MarionetteMsg'
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  non-portable

Send Pre-defined 'MarionetteMsg' to 127.0.0.1:39540.
39540 is default port number for VMCP.
-}
module Main where
import Pipes.VMCP.Marionette
import Pipes
import Data.VMCP.Marionette (MarionetteMsg(..))
import Data.VRM (BlendShapeExpression(..))
import Control.Monad (join)

main = runEffect $ each [Available True
                        , VRMBlendShapeProxyValue BlinkL 1.0
                        , VRMBlendShapeProxyValue BlinkR 0.0
                        , VRMBlendShapeProxyValue A 0.6
                        , VRMBlendShapeProxyApply
                        ]
      >-> sendMarionetteMsg "127.0.0.1" 39540
  
