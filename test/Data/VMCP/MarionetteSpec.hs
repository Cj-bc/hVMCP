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
import qualified Sound.Osc as OSC
import Control.Monad (guard, unless)
import Control.Monad.State (execStateT, runStateT, evalStateT)
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))

instance Arbitrary MarionetteMsg where
  arbitrary =
    oneof [Available <$> arbitrary
             , Time <$> arbitrary
             , RootTransform <$> arbitrary <*> arbitrary
             , BoneTransform <$> arbitrary <*> arbitrary <*> arbitrary
             , VRMBlendShapeProxyValue <$> arbitrary <*> arbitrary
             , pure VRMBlendShapeProxyApply
             ]

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Quaternion a) where
  arbitrary = Quaternion <$> arbitrary <*> arbitrary
  

toDatumStr :: Show s => s -> OSC.Datum
toDatumStr = OSC.AsciiString . fromString . show

-- | Special case for 'BlendShapeExpression', as one of its constructor
-- should be treated differently.
toDatumStr' :: BlendShapeExpression -> OSC.Datum
toDatumStr' (Custom s) = OSC.AsciiString . encodeUtf8 $ s
toDatumStr' other = toDatumStr other

spec = do
  describe "fromOSCMessage" $ do
    context "when provided with unknown Message" $ do
        it "returns Nothing" .
          isNothing $ fromOSCMessage' (OSC.Message "invalidAddress" [])
    context "test for each endpoints" $ do
      it "Ext/Ok  -- Before model is loaded" $
        fromOSCMessage' (OSC.Message "/VMC/Ext/OK" [OSC.Int32 0]) `shouldBe` Just (Available False)

      it "Ext/Ok  -- After  model is loaded" $
        fromOSCMessage' (OSC.Message "/VMC/Ext/OK" [OSC.Int32 1]) `shouldBe` Just (Available True)

      prop "Ext/T -- Just get timestamp" $ \t ->
        fromOSCMessage' (OSC.Message "/VMC/Ext/T" [OSC.Float t]) `shouldBe` Just (Time t)

      prop "Ext/Root/Pos" $ \x y z qx qy qz qw ->
                              fromOSCMessage' (OSC.Message "/VMC/Ext/Root/Pos"
                                             [ toDatumStr "root", OSC.Float x, OSC.Float y, OSC.Float z
                                             , OSC.Float qx, OSC.Float qy, OSC.Float qz, OSC.Float qw
                                             ])
                              `shouldBe` Just (RootTransform (V3 x y z) (Quaternion qw $ V3 qx qy qz))

      it "Ext/Root/Pos should fail when bone name is wrong" $
        forAll ((arbitrary :: Gen T.Text) `suchThat` (/=) "root")
        $ \name -> do
            fromOSCMessage' (OSC.Message "/VMC/Ext/Root/Pos"
                             [ toDatumStr name, OSC.Float 0, OSC.Float 0, OSC.Float 0
                             , OSC.Float 0, OSC.Float 0, OSC.Float 0
                             ])
              `shouldBe` Nothing
        
      prop "Ext/Bone/Pos" $ \name x y z qx qy qz qw ->
                              fromOSCMessage' (OSC.Message "/VMC/Ext/Bone/Pos"
                                              [toDatumStr name, OSC.Float x, OSC.Float y, OSC.Float z
                                              , OSC.Float qx, OSC.Float qy, OSC.Float qz, OSC.Float qw
                                              ])
                              `shouldBe` Just (BoneTransform name (V3 x y z) (Quaternion qw $ V3 qx qy qz))

      prop "Ext/Blend/Val" $ \name val -> 
                               fromOSCMessage' (OSC.Message "/VMC/Ext/Blend/Val" [toDatumStr' name, OSC.Float val])
                               `shouldBe` Just (VRMBlendShapeProxyValue name val)
      it "Ext/Blend/Apply" $
        fromOSCMessage' (OSC.Message "/VMC/Ext/Blend/Apply" []) `shouldBe` Just VRMBlendShapeProxyApply
      
  describe "toOSCMessage" $ do
    prop "Should be opposite to fromOSCMessage" $ \mario ->
      (fromOSCMessage' . toOSCMessage') mario `shouldBe` (Just mario)
    
  describe "pop" $ do
    context "when State is empty list" $ do
      it "fails" $
        runStateT pop ([] :: [Int]) `shouldBe` Nothing
    prop "it shuld pop first one" $
        \x y -> evalStateT pop ([x, y] :: [Int]) `shouldReturn` x
    it "After running 'pop', state should have -1 length" .
      forAll ( listOf1 (arbitrary :: Gen Int)) $ \s -> (length <$> execStateT pop s) `shouldReturn` (length s - 1)


