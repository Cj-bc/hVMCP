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
import Data.Text (Text)

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
