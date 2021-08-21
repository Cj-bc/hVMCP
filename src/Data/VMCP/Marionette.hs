{- |
Module      :  Data.VMCP.Marionette
Description :  Data types for VMCP's Marionette specification
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

Data types for
https://protocol.vmc.info/marionette-spec
-}

module Data.VMCP.Marionette where
import Data.Text (Text)
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3)
import Control.Lens.TH (makeLenses)

-- | Addresses for Marionette protocol
--
-- For now it follows 'basic specification' and doesn't have 2.x
--
-- refer to: https://protocol.vmc.info/marionette-spec
data Address =
  -- | 利用可否
  Available { _loaded :: Bool
            -- ^ True if model is already loaded
            --
            -- モデルが既に読み込まれていた場合 'True'
            }
  -- | 送信側相対時刻
  --
  -- 送信側の現在の相対時刻 通信できているかを確認するのに主に使用する
  | Time { _time :: Float }
  -- | 
  --
  -- モデルのrootとなるオブジェクトの絶対姿勢
  -- 受信側ではLoal姿勢として扱うことを推奨する
  | RootTransform { _position :: V3 Float
                  , _quaternion :: Quaternion Float
                  }
  -- | モデルのrootとなるオブジェクトのLocal姿勢
  --
  -- ※HumanBodyBonesすべてが送信される。LastBoneも含む。
  -- これにより指の動きやEyeボーンなども送信される。
  | BoneTransform { _bone :: HumanBodyBones
                  , _position :: V3 Float
                  , _quaternion :: Quaternion Float
                  }
  -- | BlendShapeProxyの値。
  -- 送信側のVRMモデルに含まれるものすべてが送信され、
  -- 一連の内容が送信された後、Applyが送信される。
  --
  -- UniVRMの仕様上、AccumulateValueで蓄えた後、Applyを行うためこのようになっている。
  -- これにより、表情やリップシンクなども送信される。
  | VRMBlendShapeProxyValue { _name :: BlendShapeExpression
                            , _value :: Float 
                            }
  -- | 一連の内容が送信された後送信される
  | VRMBlendShapeProxyApply

makeLenses ''Address
