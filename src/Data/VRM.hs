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
module Data.VRM where
import Data.Text (Text, pack)
import Data.String (IsString(..))

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
                          deriving (Show, Read, Eq)

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
