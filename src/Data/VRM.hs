{- |
Module      :  Data.VRM
Description :  Data types for subset of VRM
Copyright   :  (c) Cj.bc-sd a.k.a. Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

Subset of data types for VRM
-}
{-# LANGUAGE DeriveGeneric #-}
module Data.VRM where
import Data.Text (Text, pack, unpack)
import Data.String (IsString(..))
import Data.Hashable (Hashable(..))
import Test.QuickCheck (Arbitrary(arbitrary), elements)
import GHC.Generics (Generic)

-- | Predefined Blendshape names.
--
-- ref: https://github.com/vrm-c/vrm-specification/blob/master/specification/0.0/README.ja.md#ブレンドシェイプグループjsonextensionsvrmblendshapemasterblendshapegroups 
data BlendShapeExpression = Neutral
                          | A | I | U | E | O
                          | Blink
                          | Joy | Angry | Sorrow | Fun
                          | LookUp | LookDown
                          | LookLeft | LookRight
                          | BlinkL | BlinkR
                          | Custom Text
                          deriving (Read, Eq, Generic)

instance Show BlendShapeExpression where
  show Neutral     = "Neutral"     
  show A           = "A"
  show I           = "I"
  show U           = "U"
  show E           = "E"
  show O           = "O"
  show Blink       = "Blink"
  show Joy         = "Joy"
  show Angry       = "Angry"
  show Sorrow      = "Sorrow"
  show Fun         = "Fun"
  show LookUp      = "LookUp"
  show LookDown    = "LookDown"
  show LookLeft    = "LookLeft"
  show LookRight   = "LookRight"
  show BlinkL      = "Blink_L"
  show BlinkR      = "Blink_R"
  show (Custom other) = unpack other
-- 'IsString' にするのが正しいのかは少し悩んでいる...
--
-- I'm not sure whether it is correct decision to implement
-- 'IsString' instance for 'BlendShapeExpression'
instance IsString BlendShapeExpression where
  fromString "Neutral"   = Neutral
  fromString "A"         = A
  fromString "I"         = I
  fromString "U"         = U
  fromString "E"         = E
  fromString "O"         = O
  fromString "Blink"     = Blink
  fromString "Joy"       = Joy
  fromString "Angry"     = Angry
  fromString "Sorrow"    = Sorrow
  fromString "Fun"       = Fun
  fromString "LookUp"    = LookUp
  fromString "LookDown"  = LookDown
  fromString "LookLeft"  = LookLeft
  fromString "LookRight" = LookRight
  fromString "Blink_L"    = BlinkL
  fromString "Blink_R"    = BlinkR
  fromString other       = Custom $ pack other


instance Hashable BlendShapeExpression where
  hashWithSalt s Neutral        = s `hashWithSalt` (0 :: Int) 
  hashWithSalt s A              = s `hashWithSalt` (1 :: Int)
  hashWithSalt s I              = s `hashWithSalt` (2 :: Int)
  hashWithSalt s U              = s `hashWithSalt` (3 :: Int)
  hashWithSalt s E              = s `hashWithSalt` (4 :: Int)
  hashWithSalt s O              = s `hashWithSalt` (5 :: Int)
  hashWithSalt s Blink          = s `hashWithSalt` (6 :: Int)
  hashWithSalt s Joy            = s `hashWithSalt` (7 :: Int)
  hashWithSalt s Angry          = s `hashWithSalt` (8 :: Int)
  hashWithSalt s Sorrow         = s `hashWithSalt` (9 :: Int)
  hashWithSalt s Fun            = s `hashWithSalt` (10 :: Int)
  hashWithSalt s LookUp         = s `hashWithSalt` (11 :: Int)
  hashWithSalt s LookDown       = s `hashWithSalt` (12 :: Int)
  hashWithSalt s LookLeft       = s `hashWithSalt` (13 :: Int)
  hashWithSalt s LookRight      = s `hashWithSalt` (14 :: Int)
  hashWithSalt s BlinkL         = s `hashWithSalt` (15 :: Int)
  hashWithSalt s BlinkR         = s `hashWithSalt` (16 :: Int)
  hashWithSalt s (Custom other) = s `hashWithSalt` (17 :: Int) `hashWithSalt` other


instance Arbitrary BlendShapeExpression where
  arbitrary = elements [Neutral
                       , A , I , U , E , O
                       , Blink
                       , Joy , Angry , Sorrow , Fun
                       , LookUp , LookDown
                       , LookLeft , LookRight
                       , BlinkL , BlinkR
                       , Custom (pack "") -- TODO: Use Text arbitrary value
                       ]
