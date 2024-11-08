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
import Data.Bool (bool)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T
import Data.String (fromString)
import Data.VRM
import Data.VMCP.Message (VMCPMessage(..), byteSizeWithPadding)
import Data.UnityEditor
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Linear.V4 (_x, _y, _z, _w)
import Control.Lens ((^?), preview, review, (^.))
import Control.Lens.TH (makeLenses, makePrisms)
import Sound.Osc (Bundle(..), Message(..), ascii_to_string, ascii, Datum, bundle)
import Sound.Osc.Lens
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


instance VMCPMessage MarionetteMsg where
  -- | Byte size of each 'MarionetteMsg'
  byteSize msg = case msg of
                   VRMBlendShapeProxyApply -> 28 -- ^ "/VMC/Ext/Blend/Apply0000,000"
                   (VRMBlendShapeProxyValue name _) -> byteSizeWithPadding name + 32
                   -- ^ "/VMC/Ext/Blend/Value0000,sf0<Name><Float 4bytes>"
                   (BoneTransform bone _ _)         -> byteSizeWithPadding bone + 60
                   -- ^ "/VMC/Ext/Bone/Pos000,sfffffff000<String >[<Float 4bytes>*7]"
                   _ -> 0
                   -- TODO: Should I implement this function for other value constructors?
  fromOSCMessage = fromOSCMessage'
  toOSCMessage = toOSCMessage'

-- | 'pop' one item from 'State' state
-- 
-- Helper function for state monad
pop :: MonadFail m => StateT [a] m a
pop = do
  s <- get
  when (null s) . lift $ fail "No more state to pop"
  state $ \s -> (head s, tail s)
  
-- | 'pop' one item from 'State' state, and extract with 'Getting'
pop' l = do
  x <- pop
  lift (preview l x)


-- | Convert OSC's 'Message' into 'MarionetteMsg'
--
-- All invalid 'Message's will be ignored, as it's written
-- in VMCP specification.
fromOSCMessage' :: Message -> Maybe MarionetteMsg
fromOSCMessage' (Message addr datums)
  | addr == "/VMC/Ext/OK"            = Available . (== 1) <$> head datums^?_Int32
  | addr == "/VMC/Ext/T"             = Time <$> head datums^?_Float
  | addr == "/VMC/Ext/Root/Pos"      = flip evalStateT datums $ do
      name <- ascii_to_string <$> pop' _AsciiString
      -- when (name /= "root") $ fail "Wrong root bone name"
      pos <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q'  <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q <- Quaternion <$> pop' _Float <*> pure q'
      return $ RootTransform pos q
  | addr == "/VMC/Ext/Bone/Pos"      = flip evalStateT datums $ do -- Monad of 'StateT [Datum] Maybe
      name <- read . ascii_to_string <$> pop' _AsciiString
      pos <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q'  <- V3 <$> pop' _Float  <*> pop' _Float  <*> pop' _Float
      q <- Quaternion <$> pop' _Float <*> pure q'
      return $ BoneTransform name pos q
  | addr == "/VMC/Ext/Blend/Val"     = flip evalStateT datums $ do
      name <- decodeUtf8' <$> pop' _AsciiString
      case name of
        (Left e) -> fail "Ext/Blend/Val: invalid UTF-8 character"
        (Right n') -> do
          val <- pop' _Float
          return $ VRMBlendShapeProxyValue (fromString . T.unpack $ n') val
  | addr == "/VMC/Ext/Blend/Apply"   = Just VRMBlendShapeProxyApply
  | otherwise                                = Nothing
fromOSCMessage' _ = Nothing



-- | Convert 'MarionetteMsg' into 'Message'
toOSCMessage' :: MarionetteMsg -> Message
toOSCMessage' (Available loaded) = Message "/VMC/Ext/OK" $ [review _Int32 (bool 0 1 loaded)]
toOSCMessage' (Time time)        = Message "/VMC/Ext/T"  $ [review _Float time]
toOSCMessage' (RootTransform pos q) =
  let nameDatum = review _AsciiString (ascii "Root")
      valueDatums = review _Float <$> [pos^._x, pos^._y, pos^._z
                                     , q^._x, q^._y, q^._z, q^._w]
  in Message "/VMC/Ext/Root/Pos" (nameDatum:valueDatums)
toOSCMessage' (BoneTransform name pos q) =
  let nameDatum = (review _AsciiString . ascii . show $ name)
      valueDatums = review _Float <$> [pos^._x, pos^._y, pos^._z
                                      , q^._x, q^._y, q^._z, q^._w]
  in Message "/VMC/Ext/Bone/Pos" (nameDatum:valueDatums)
toOSCMessage' (VRMBlendShapeProxyValue name val)
  = Message "/VMC/Ext/Blend/Val" [(review _AsciiString . ascii . show $ name)
                                 , review _Float val ]
toOSCMessage' VRMBlendShapeProxyApply = Message "/VMC/Ext/Blend/Apply" []



