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
{-# LANGUAGE TemplateHaskell #-}
module Data.VMCP.Marionette where
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.String (fromString)
import Data.VRM
import Data.UnityEditor
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Control.Lens ((^?), preview)
import Control.Lens.TH (makeLenses, makePrisms)
import Sound.OSC (Bundle(..), Message(..), ascii_to_string, Datum)
import Sound.OSC.Lens
import Control.Monad.State (StateT(..), evalStateT, state, lift, get)
import Control.Monad (when)

-- | VMCPMessagees for Marionette protocol
--
-- For now it follows 'basic specification' and doesn't have 2.x
--
-- refer to: https://protocol.vmc.info/marionette-spec
data MarionetteMsg =
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
  deriving (Show, Eq)

makeLenses ''MarionetteMsg
makePrisms ''MarionetteMsg


pop :: MonadFail m => StateT [a] m a
pop = do
  s <- get
  when (null s) . lift $ fail "No more state to pop"
  state $ \s -> (head s, tail s)
  
pop' l = do
  x <- pop
  lift (preview l x)


fromOSCMessage :: Message -> Maybe MarionetteMsg
fromOSCMessage (Message addr datums)
  | addr == "/VMC/Ext/OK"            = Available . (== 1) <$> head datums^?_Int32
  | addr == "/VMC/Ext/T"             = Time <$> head datums^?_Float
  | addr == "/VMC/Ext/Bone/Pos"      = flip evalStateT datums $ do -- Monad of 'StateT [Datum] Maybe
      name <- read . ascii_to_string <$> pop' _ASCII_String
      pos <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q'  <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q <- Quaternion <$> pop' _Float <*> pure q'
      return $ BoneTransform name pos q
  | addr == "/VMC/Ext/Blend/Val"     = flip evalStateT datums $ do
      name <- decodeUtf8' <$> pop' _ASCII_String
      case name of
        (Left e) -> fail "Ext/Blend/Val: invalid UTF-8 character"
        (Right n') -> do
          val <- pop' _Float
          return $ VRMBlendShapeProxyValue (fromText n') val
  | addr == "/VMC/Ext/Blend/Apply"   = Just VRMBlendShapeProxyApply
  | otherwise                                = Nothing
fromOSCMessage _ = Nothing
