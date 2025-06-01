{-# LANGUAGE OverloadedStrings #-}
module Data.VMCP.PerformerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success, Failure) -- Avoids clash with RSpec
import Test.QuickCheck.Instances.Text () -- For Arbitrary Text
import Test.QuickCheck.Instances.ByteString () -- For Arbitrary ByteString if needed for OSC

import Data.VMCP.Performer
import Data.VMCP.Message (VMCPMessage(..)) -- To use fromOSCMessage, toOSCMessage

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Linear.V3 (V3(..))
import Linear.Quaternion (Quaternion(..))
import qualified Sound.Osc as OSC
import Sound.Osc.Datum (Datum(..)) -- Explicit import for Datum constructors
import Control.Lens ((^.))
import Linear.V4 (_x, _y, _z, _w) -- For accessing V3 and Quaternion components

-- Reusable Arbitrary instances for Linear types (similar to MarionetteSpec)
instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (V3 x y z) = [V3 x' y' z' | (x', y', z') <- shrink (x,y,z)]

instance Arbitrary a => Arbitrary (Quaternion a) where
  arbitrary = Quaternion <$> arbitrary <*> arbitrary
  shrink (Quaternion w v) = [Quaternion w' v' | (w',v') <- shrink (w,v)]

-- Arbitrary instances for Performer data types

instance Arbitrary VirtualDeviceTransform where
  arbitrary = VirtualDeviceTransform
    <$> arbitrary -- _vdtSerial :: Text
    <*> arbitrary -- _vdtPosition :: V3 Float
    <*> arbitrary -- _vdtQuaternion :: Quaternion Float
  shrink = genericShrink

instance Arbitrary FramePeriod where
  arbitrary = FramePeriod
    <$> arbitrary -- _fpStatus :: Int
    <*> arbitrary -- _fpRoot :: Int
    <*> arbitrary -- _fpBone :: Int
    <*> arbitrary -- _fpBlendShape :: Int
    <*> arbitrary -- _fpCamera :: Int
    <*> arbitrary -- _fpDevices :: Int
  shrink = genericShrink

instance Arbitrary VirtualMIDICCValueInput where
  arbitrary = VirtualMIDICCValueInput
    <$> arbitrary -- _vmcvKnob :: Int
    <*> arbitrary -- _vmcvValue :: Float
  shrink = genericShrink

instance Arbitrary VirtualCameraTransformFOV where
  arbitrary = VirtualCameraTransformFOV
    <$> arbitrary -- _vctfCameraName :: Text
    <*> arbitrary -- _vctfPosition :: V3 Float
    <*> arbitrary -- _vctfQuaternion :: Quaternion Float
    <*> arbitrary -- _vctfFov :: Float
  shrink = genericShrink

instance Arbitrary VRMBlendShapeProxyValue where
  arbitrary = VRMBlendShapeProxyValue
    <$> arbitrary -- _bspvName :: Text
    <*> arbitrary -- _bspvValue :: Float
  shrink = genericShrink

instance Arbitrary EyeTrackingTargetPosition where
  arbitrary = EyeTrackingTargetPosition
    <$> arbitrary -- _ettpEnable :: Int
    <*> arbitrary -- _ettpPosition :: V3 Float
  shrink = genericShrink

instance Arbitrary PerformerMsg where
  arbitrary = oneof
    [ HmdPos <$> arbitrary
    , ConPos <$> arbitrary
    , TraPos <$> arbitrary
    , SetPeriod <$> arbitrary
    , MidiCCValue <$> arbitrary
    , CameraTransformFOV <$> arbitrary
    , BlendShapeValue <$> arbitrary
    , pure BlendShapeApply -- No fields
    , SetEyeTarget <$> arbitrary
    ]
  shrink = genericShrink

spec :: Spec
spec = do
  describe "Data.VMCP.Performer" $ do
    describe "VMCPMessage instance for PerformerMsg" $ do
      prop "fromOSCMessage . toOSCMessage is an identity" $ \msg ->
        (fromOSCMessage . toOSCMessage) msg `shouldBe` Just (msg :: PerformerMsg)

      -- Specific unit tests for fromOSCMessage and toOSCMessage for each type
      -- HmdPos
      context "/VMC/Ext/Hmd/Pos" $ do
        let exampleSerial = "TestHMD"
            examplePos = V3 1.0 2.0 3.0
            exampleQuat = Quaternion 0.707 (V3 0.0 0.707 0.0)
            exampleMsgData = HmdPos (VirtualDeviceTransform exampleSerial examplePos exampleQuat)
            exampleOsc = OSC.Message "/VMC/Ext/Hmd/Pos"
              [ OSC.AsciiString (encodeUtf8 exampleSerial)
              , OSC.Float (examplePos ^. _x), OSC.Float (examplePos ^. _y), OSC.Float (examplePos ^. _z)
              , OSC.Float (exampleQuat ^. _x), OSC.Float (exampleQuat ^. _y), OSC.Float (exampleQuat ^. _z), OSC.Float (exampleQuat ^. _w)
              ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- ConPos (similar structure to HmdPos)
      context "/VMC/Ext/Con/Pos" $ do
        let exampleSerial = "TestCon"
            examplePos = V3 0.1 0.2 0.3
            exampleQuat = Quaternion 0.5 (V3 0.5 0.5 0.5)
            exampleMsgData = ConPos (VirtualDeviceTransform exampleSerial examplePos exampleQuat)
            exampleOsc = OSC.Message "/VMC/Ext/Con/Pos"
              [ OSC.AsciiString (encodeUtf8 exampleSerial)
              , OSC.Float (examplePos ^. _x), OSC.Float (examplePos ^. _y), OSC.Float (examplePos ^. _z)
              , OSC.Float (exampleQuat ^. _x), OSC.Float (exampleQuat ^. _y), OSC.Float (exampleQuat ^. _z), OSC.Float (exampleQuat ^. _w)
              ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- TraPos (similar structure to HmdPos)
      context "/VMC/Ext/Tra/Pos" $ do
        let exampleSerial = "TestTra"
            examplePos = V3 (-1.0) (-2.0) (-3.0)
            exampleQuat = Quaternion 1.0 (V3 0.0 0.0 0.0)
            exampleMsgData = TraPos (VirtualDeviceTransform exampleSerial examplePos exampleQuat)
            exampleOsc = OSC.Message "/VMC/Ext/Tra/Pos"
              [ OSC.AsciiString (encodeUtf8 exampleSerial)
              , OSC.Float (examplePos ^. _x), OSC.Float (examplePos ^. _y), OSC.Float (examplePos ^. _z)
              , OSC.Float (exampleQuat ^. _x), OSC.Float (exampleQuat ^. _y), OSC.Float (exampleQuat ^. _z), OSC.Float (exampleQuat ^. _w)
              ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- SetPeriod
      context "/VMC/Ext/Set/Period" $ do
        let exampleData = FramePeriod 1 16 33 50 66 83 -- status, root, bone, blendShape, camera, devices
            exampleMsgData = SetPeriod exampleData
            exampleOsc = OSC.Message "/VMC/Ext/Set/Period"
              [ OSC.Int32 1, OSC.Int32 16, OSC.Int32 33, OSC.Int32 50, OSC.Int32 66, OSC.Int32 83 ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- MidiCCValue
      context "/VMC/Ext/Midi/CC/Val" $ do
        let exampleData = VirtualMIDICCValueInput 10 0.75 -- knob, value
            exampleMsgData = MidiCCValue exampleData
            exampleOsc = OSC.Message "/VMC/Ext/Midi/CC/Val" [ OSC.Int32 10, OSC.Float 0.75 ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- CameraTransformFOV
      context "/VMC/Ext/Cam" $ do
        let exampleName = "MainCamera"
            examplePos = V3 10.0 1.0 5.0
            exampleQuat = Quaternion 0.0 (V3 1.0 0.0 0.0)
            exampleFov = 60.0
            exampleMsgData = CameraTransformFOV (VirtualCameraTransformFOV exampleName examplePos exampleQuat exampleFov)
            exampleOsc = OSC.Message "/VMC/Ext/Cam"
              [ OSC.AsciiString (encodeUtf8 exampleName)
              , OSC.Float (examplePos ^. _x), OSC.Float (examplePos ^. _y), OSC.Float (examplePos ^. _z)
              , OSC.Float (exampleQuat ^. _x), OSC.Float (exampleQuat ^. _y), OSC.Float (exampleQuat ^. _z), OSC.Float (exampleQuat ^. _w)
              , OSC.Float exampleFov
              ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- BlendShapeValue
      context "/VMC/Ext/Blend/Val" $ do
        let exampleName = "Smile" :: Text
            exampleVal = 0.8
            exampleMsgData = BlendShapeValue (VRMBlendShapeProxyValue exampleName exampleVal)
            exampleOsc = OSC.Message "/VMC/Ext/Blend/Val" [ OSC.AsciiString (encodeUtf8 exampleName), OSC.Float exampleVal ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- BlendShapeApply
      context "/VMC/Ext/Blend/Apply" $ do
        let exampleMsgData = BlendShapeApply
            exampleOsc = OSC.Message "/VMC/Ext/Blend/Apply" []
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc

      -- SetEyeTarget
      context "/VMC/Ext/Set/Eye" $ do
        let exampleEnable = 1
            examplePos = V3 0.0 0.1 1.0 -- x, y, z (absolute)
            exampleMsgData = SetEyeTarget (EyeTrackingTargetPosition exampleEnable examplePos)
            exampleOsc = OSC.Message "/VMC/Ext/Set/Eye"
              [ OSC.Int32 exampleEnable
              , OSC.Float (examplePos ^. _x), OSC.Float (examplePos ^. _y), OSC.Float (examplePos ^. _z)
              ]
        it "parses correctly (fromOSCMessage)" $
          fromOSCMessage exampleOsc `shouldBe` Just exampleMsgData
        it "encodes correctly (toOSCMessage)" $
          toOSCMessage exampleMsgData `shouldBe` exampleOsc
