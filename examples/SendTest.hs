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
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Pipes.VMCP.Marionette
import Pipes
import Data.VMCP.Marionette (MarionetteMsg(..))
import Data.VRM (BlendShapeExpression(..))
import Control.Monad (join)

main = runEffect $ each (mconcat $ replicate 20 [Available True
                        , VRMBlendShapeProxyValue Neutral 0.0
                        , VRMBlendShapeProxyValue A      0.0
                        , VRMBlendShapeProxyValue I      0.0
                        , VRMBlendShapeProxyValue U      0.0
                        , VRMBlendShapeProxyValue E      0.0
                        , VRMBlendShapeProxyValue O      0.0
                        , VRMBlendShapeProxyValue Blink  0.0
                        , VRMBlendShapeProxyValue Joy 0.0
                        , VRMBlendShapeProxyValue Angry  0.0
                        , VRMBlendShapeProxyValue Sorrow 0.0
                        , VRMBlendShapeProxyValue Fun 0.0
                        , VRMBlendShapeProxyValue LookUp 0.0
                        , VRMBlendShapeProxyValue LookDown 0.0
                        , VRMBlendShapeProxyValue LookLeft 0.0
                        , VRMBlendShapeProxyValue LookRight 0.0
                        , VRMBlendShapeProxyValue BlinkL 1.0
                        , VRMBlendShapeProxyValue BlinkR 0.0
                        , VRMBlendShapeProxyValue (Custom "HELLO") 0.6
                        , VRMBlendShapeProxyApply
                        ])
      >-> sendMarionetteMsg "127.0.0.1" 39540
  
