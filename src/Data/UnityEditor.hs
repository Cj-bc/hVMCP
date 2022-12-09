{-# LANGUAGE DeriveGeneric #-}
module Data.UnityEditor where
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Test.QuickCheck (Arbitrary(arbitrary), elements)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

-- | Unity's 'HumanBodyBones' enum
data HumanBodyBones = Hips
                    | LeftUpperLeg | RightUpperLeg
                    | LeftLowerLeg | RightLowerLeg
                    | LeftFoot | RightFoot
                    | Spine
                    | Chest
                    | UpperChest
                    | Neck
                    | Head
                    | LeftShoulder | RightShoulder
                    | LeftUpperArm | RightUpperArm
                    | LeftLowerArm | RightLowerArm
                    | LeftHand | RightHand
                    | LeftToes | RightToes
                    | LeftEye | RightEye
                    | Jaw
                    | LeftThumbProximal
                    | LeftThumbIntermediate
                    | LeftThumbDistal
                    | LeftIndexProximal
                    | LeftIndexIntermediate
                    | LeftIndexDistal
                    | LeftMiddleProximal
                    | LeftMiddleIntermediate
                    | LeftMiddleDistal
                    | LeftRingProximal
                    | LeftRingIntermediate
                    | LeftRingDistal
                    | LeftLittleProximal
                    | LeftLittleIntermediate
                    | LeftLittleDistal
                    | RightThumbProximal
                    | RightThumbIntermediate
                    | RightThumbDistal
                    | RightIndexProximal
                    | RightIndexIntermediate
                    | RightIndexDistal
                    | RightMiddleProximal
                    | RightMiddleIntermediate
                    | RightMiddleDistal
                    | RightRingProximal
                    | RightRingIntermediate
                    | RightRingDistal
                    | RightLittleProximal
                    | RightLittleIntermediate
                    | RightLittleDistal
                    | LastBone
                    deriving (Read, Show, Eq, Generic)

instance Hashable HumanBodyBones

instance Arbitrary HumanBodyBones where
  arbitrary = genericArbitrary
