{-# LANGUAGE OverloadedStrings #-}
module Data.VMCP.MarionetteSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fun)
import Test.QuickCheck.Instances.Text
import Data.VMCP.Marionette
import Data.VRM
import Data.UnityEditor
import Data.Maybe (isNothing, isJust)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Sound.OSC as OSC
import Control.Monad (guard, unless)
import Control.Monad.State (execStateT, runStateT)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))


instance Arbitrary HumanBodyBones where
  arbitrary = elements [Hips
                       , LeftUpperLeg , RightUpperLeg, LeftLowerLeg , RightLowerLeg
                       , LeftFoot , RightFoot
                       , Spine, Chest, UpperChest, Neck, Head
                       , LeftShoulder , RightShoulder
                       , LeftUpperArm , RightUpperArm, LeftLowerArm , RightLowerArm
                       , LeftHand , RightHand, LeftToes , RightToes
                       , LeftEye , RightEye, Jaw
                       , LeftThumbProximal, LeftThumbIntermediate
                       , LeftThumbDistal, LeftIndexProximal
                       , LeftIndexIntermediate, LeftIndexDistal
                       , LeftMiddleProximal, LeftMiddleIntermediate
                       , LeftMiddleDistal, LeftRingProximal
                       , LeftRingIntermediate, LeftRingDistal
                       , LeftLittleProximal, LeftLittleIntermediate
                       , LeftLittleDistal, RightThumbProximal
                       , RightThumbIntermediate
                       , RightThumbDistal
                       , RightIndexProximal
                       , RightIndexIntermediate
                       , RightIndexDistal
                       , RightMiddleProximal
                       , RightMiddleIntermediate
                       , RightMiddleDistal
                       , RightRingProximal
                       , RightRingIntermediate
                       , RightRingDistal
                       , RightLittleProximal
                       , RightLittleIntermediate
                       , RightLittleDistal
                       , LastBone
                       ]

instance Arbitrary BlendShapeExpression where
  arbitrary = do
    let defaultExpressions = [ Neutral, A , I , U , E , O
             , Blink, Joy , Angry , Sorrow , Fun
             , LookUp , LookDown, LookLeft , LookRight
             , BlinkL , BlinkR]
    customName <- (arbitrary :: Gen T.Text)  `suchThat` (\s -> (fromString.T.unpack) s `notElem` defaultExpressions)
    elements $ Custom customName:defaultExpressions

toDatumStr :: Show s => s -> OSC.Datum
toDatumStr = OSC.ASCII_String . fromString . show

-- | Special case for 'BlendShapeExpression', as one of its constructor
-- should be treated differently.
toDatumStr' :: BlendShapeExpression -> OSC.Datum
toDatumStr' (Custom s) = OSC.ASCII_String . encodeUtf8 $ s
toDatumStr' other = toDatumStr other

spec = do
  describe "fromOSCMessage" $ do
    context "when provided with unknown Message" $ do
        it "returns Nothing" .
          isNothing $ fromOSCMessage (OSC.Message "invalidAddress" [])
    context "test for each endpoints" $ do
      it "Ext/Ok  -- Before model is loaded" $
        fromOSCMessage (OSC.Message "/VMC/Ext/OK" [OSC.Int32 0]) `shouldBe` Just (Available False)

      it "Ext/Ok  -- After  model is loaded" $
        fromOSCMessage (OSC.Message "/VMC/Ext/OK" [OSC.Int32 1]) `shouldBe` Just (Available True)

      prop "Ext/T -- Just get timestamp" $ \t ->
        fromOSCMessage (OSC.Message "/VMC/Ext/T" [OSC.Float t]) `shouldBe` Just (Time t)

      prop "Ext/Bone/Pos" $ \name x y z qx qy qz qw ->
                              fromOSCMessage (OSC.Message "/VMC/Ext/Bone/Pos"
                                              [toDatumStr name, OSC.Float x, OSC.Float y, OSC.Float z
                                              , OSC.Float qx, OSC.Float qy, OSC.Float qz, OSC.Float qw
                                              ])
                              `shouldBe` Just (BoneTransform name (V3 x y z) (Quaternion qw $ V3 qx qy qz))

      prop "Ext/Blend/Val" $ \name val -> 
                               fromOSCMessage (OSC.Message "/VMC/Ext/Blend/Val" [toDatumStr' name, OSC.Float val])
                               `shouldBe` Just (VRMBlendShapeProxyValue name val)
      it "Ext/Blend/Apply" $
        fromOSCMessage (OSC.Message "/VMC/Ext/Blend/Apply" []) `shouldBe` Just VRMBlendShapeProxyApply
      
  describe "pop" $ do
    context "when State is empty list" $ do
      it "fails" $
        runStateT pop ([] :: [Int]) `shouldBe` Nothing
