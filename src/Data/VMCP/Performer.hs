{- |
Module      :  Data.VMCP.Performer
Description :  Data types for VMCP's Performer specification V2.3
Copyright   :  (c) Your Name Here
License     :  GPL-3
Maintainer  :  your.email@example.com
Stability   :  experimental
Portability :  portable

Data types for Performer V2.3
https://protocol.vmc.info/performer-spec
-}
{-# LANGUAGE TemplateHaskell #-}
module Data.VMCP.Performer where

import Control.Lens.TH (makeLenses, makePrisms)
import Data.Text (Text)
import Linear.V3 (V3(..))
import Linear.Quaternion (Quaternion(..))
import Data.VMCP.Message (VMCPMessage(..), byteSizeWithPadding)
import Sound.Osc (Message(..), Datum(..), OscType(..), ascii, ascii_to_string, int32, float)
import Sound.Osc.Lens (MessageLens(..), message, HasDatumLens(..), AsOscType(..)) -- For pop' / review
import Control.Lens ((^?), preview, review, (^.))
import Control.Monad.State (StateT(..), evalStateT, state, lift, get)
import Control.Monad (when)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Linear.V4 (_x, _y, _z, _w) -- For accessing V3 and Quaternion components

-- Data records

data VirtualDeviceTransform = VirtualDeviceTransform
  { _vdtSerial     :: Text
  , _vdtPosition   :: V3 Float
  , _vdtQuaternion :: Quaternion Float
  } deriving (Show, Eq)

data FramePeriod = FramePeriod
  { _fpStatus     :: Int
  , _fpRoot       :: Int
  , _fpBone       :: Int
  , _fpBlendShape :: Int
  , _fpCamera     :: Int
  , _fpDevices    :: Int
  } deriving (Show, Eq)

data VirtualMIDICCValueInput = VirtualMIDICCValueInput
  { _vmcvKnob :: Int
  , _vmcvValue :: Float
  } deriving (Show, Eq)

data VirtualCameraTransformFOV = VirtualCameraTransformFOV
  { _vctfCameraName :: Text
  , _vctfPosition   :: V3 Float
  , _vctfQuaternion :: Quaternion Float
  , _vctfFov        :: Float
  } deriving (Show, Eq)

data VRMBlendShapeProxyValue = VRMBlendShapeProxyValue
  { _bspvName  :: Text -- Using Text as decided, not BlendShapeExpression
  , _bspvValue :: Float
  } deriving (Show, Eq)

data EyeTrackingTargetPosition = EyeTrackingTargetPosition
  { _ettpEnable   :: Int -- 0 for disable, 1 for enable
  , _ettpPosition :: V3 Float -- Absolute position for V2.3
  } deriving (Show, Eq)

-- Main PerformerMsg sum type
data PerformerMsg
  = HmdPos VirtualDeviceTransform
  | ConPos VirtualDeviceTransform
  | TraPos VirtualDeviceTransform
  | SetPeriod FramePeriod
  | MidiCCValue VirtualMIDICCValueInput
  | CameraTransformFOV VirtualCameraTransformFOV
  | BlendShapeValue VRMBlendShapeProxyValue
  | BlendShapeApply -- No fields for Apply
  | SetEyeTarget EyeTrackingTargetPosition
  deriving (Show, Eq)

-- Lenses and Prisms
makeLenses ''VirtualDeviceTransform
makeLenses ''FramePeriod
makeLenses ''VirtualMIDICCValueInput
makeLenses ''VirtualCameraTransformFOV
makeLenses ''VRMBlendShapeProxyValue
makeLenses ''EyeTrackingTargetPosition
makePrisms ''PerformerMsg
-- For PerformerMsg itself, we might want specific lenses for each constructor's payload
-- e.g., makeLenses ''PerformerMsg will create lenses like 'hmdPos', 'conPos', etc.
-- which might conflict if we want to access the VirtualDeviceTransform fields directly.
-- The prisms are more important for the sum type.
-- Let's add lenses for PerformerMsg as well, it's standard.
makeLenses ''PerformerMsg

-- Helper function for state monad, similar to Marionette's
pop :: MonadFail m => StateT [Datum] m a
pop = do
  s <- get
  when (null s) . lift $ fail "No more state to pop"
  state $ \st -> (head st, tail st)

pop' :: (MonadFail m, AsOscType a) => Prism' Datum a -> StateT [Datum] m a
pop' l = do
  x <- pop
  lift (preview l x)

instance VMCPMessage PerformerMsg where
  fromOSCMessage = fromOSCMessage' -- Placeholder
  toOSCMessage = toOSCMessage'   -- Placeholder
  byteSize = byteSize'

fromOSCMessage' :: Message -> Maybe PerformerMsg
fromOSCMessage' (Message addr datums)
  | addr == "/VMC/Ext/Hmd/Pos" = flip evalStateT datums $ do
      serial <- decodeUtf8' <$> pop' _AsciiString >>= either (fail . show) return
      posX <- pop' _Float; posY <- pop' _Float; posZ <- pop' _Float
      quatX <- pop' _Float; quatY <- pop' _Float; quatZ <- pop' _Float; quatW <- pop' _Float
      return $ HmdPos $ VirtualDeviceTransform serial (V3 posX posY posZ) (Quaternion quatW (V3 quatX quatY quatZ))
  | addr == "/VMC/Ext/Con/Pos" = flip evalStateT datums $ do
      serial <- decodeUtf8' <$> pop' _AsciiString >>= either (fail . show) return
      posX <- pop' _Float; posY <- pop' _Float; posZ <- pop' _Float
      quatX <- pop' _Float; quatY <- pop' _Float; quatZ <- pop' _Float; quatW <- pop' _Float
      return $ ConPos $ VirtualDeviceTransform serial (V3 posX posY posZ) (Quaternion quatW (V3 quatX quatY quatZ))
  | addr == "/VMC/Ext/Tra/Pos" = flip evalStateT datums $ do
      serial <- decodeUtf8' <$> pop' _AsciiString >>= either (fail . show) return
      posX <- pop' _Float; posY <- pop' _Float; posZ <- pop' _Float
      quatX <- pop' _Float; quatY <- pop' _Float; quatZ <- pop' _Float; quatW <- pop' _Float
      return $ TraPos $ VirtualDeviceTransform serial (V3 posX posY posZ) (Quaternion quatW (V3 quatX quatY quatZ))
  | addr == "/VMC/Ext/Set/Period" = flip evalStateT datums $ do
      status <- pop' _Int32
      root <- pop' _Int32
      bone <- pop' _Int32
      blendShape <- pop' _Int32
      camera <- pop' _Int32
      devices <- pop' _Int32
      return $ SetPeriod $ FramePeriod (fromIntegral status) (fromIntegral root) (fromIntegral bone) (fromIntegral blendShape) (fromIntegral camera) (fromIntegral devices)
  | addr == "/VMC/Ext/Midi/CC/Val" = flip evalStateT datums $ do
      knob <- pop' _Int32
      value <- pop' _Float
      return $ MidiCCValue $ VirtualMIDICCValueInput (fromIntegral knob) value
  | addr == "/VMC/Ext/Cam" = flip evalStateT datums $ do
      name <- decodeUtf8' <$> pop' _AsciiString >>= either (fail . show) return
      posX <- pop' _Float; posY <- pop' _Float; posZ <- pop' _Float
      quatX <- pop' _Float; quatY <- pop' _Float; quatZ <- pop' _Float; quatW <- pop' _Float
      fov <- pop' _Float
      return $ CameraTransformFOV $ VirtualCameraTransformFOV name (V3 posX posY posZ) (Quaternion quatW (V3 quatX quatY quatZ)) fov
  | addr == "/VMC/Ext/Blend/Val" = flip evalStateT datums $ do
      name <- decodeUtf8' <$> pop' _AsciiString >>= either (fail . show) return
      value <- pop' _Float
      return $ BlendShapeValue $ VRMBlendShapeProxyValue name value
  | addr == "/VMC/Ext/Blend/Apply" = Just BlendShapeApply -- No data to parse
  | addr == "/VMC/Ext/Set/Eye" = flip evalStateT datums $ do
      enable <- pop' _Int32
      posX <- pop' _Float; posY <- pop' _Float; posZ <- pop' _Float
      return $ SetEyeTarget $ EyeTrackingTargetPosition (fromIntegral enable) (V3 posX posY posZ)
  | otherwise = Nothing
fromOSCMessage' _ = Nothing -- Catch-all for non-Message OSC packets (e.g. Bundles, though VMCP is usually single messages)


toOSCMessage' :: PerformerMsg -> Message
toOSCMessage' msg = case msg of
  HmdPos (VirtualDeviceTransform serial pos quat) ->
    Message "/VMC/Ext/Hmd/Pos" $
      [review _AsciiString (encodeUtf8 serial)] ++ map (review _Float)
      [pos^._x, pos^._y, pos^._z, quat^._x, quat^._y, quat^._z, quat^._w]
  ConPos (VirtualDeviceTransform serial pos quat) ->
    Message "/VMC/Ext/Con/Pos" $
      [review _AsciiString (encodeUtf8 serial)] ++ map (review _Float)
      [pos^._x, pos^._y, pos^._z, quat^._x, quat^._y, quat^._z, quat^._w]
  TraPos (VirtualDeviceTransform serial pos quat) ->
    Message "/VMC/Ext/Tra/Pos" $
      [review _AsciiString (encodeUtf8 serial)] ++ map (review _Float)
      [pos^._x, pos^._y, pos^._z, quat^._x, quat^._y, quat^._z, quat^._w]
  SetPeriod (FramePeriod status root bone blendShape camera devices) ->
    Message "/VMC/Ext/Set/Period" $ map (review _Int32 . fromIntegral)
      [status, root, bone, blendShape, camera, devices]
  MidiCCValue (VirtualMIDICCValueInput knob val) ->
    Message "/VMC/Ext/Midi/CC/Val"
      [review _Int32 (fromIntegral knob), review _Float val]
  CameraTransformFOV (VirtualCameraTransformFOV name pos quat fov) ->
    Message "/VMC/Ext/Cam" $
      [review _AsciiString (encodeUtf8 name)] ++ map (review _Float)
      [pos^._x, pos^._y, pos^._z, quat^._x, quat^._y, quat^._z, quat^._w] ++
      [review _Float fov]
  BlendShapeValue (VRMBlendShapeProxyValue name val) ->
    Message "/VMC/Ext/Blend/Val"
      [review _AsciiString (encodeUtf8 name), review _Float val]
  BlendShapeApply -> Message "/VMC/Ext/Blend/Apply" []
  SetEyeTarget (EyeTrackingTargetPosition enable pos) ->
    Message "/VMC/Ext/Set/Eye"
      [review _Int32 (fromIntegral enable), review _Float (pos^._x), review _Float (pos^._y), review _Float (pos^._z)]

byteSize' :: PerformerMsg -> Int
byteSize' msg =
  let pathSize path = byteSizeWithPadding path
      stringSize str = byteSizeWithPadding (encodeUtf8 str) -- Text to ByteString for size
  in case msg of
    HmdPos (VirtualDeviceTransform s _ _)       -> pathSize "/VMC/Ext/Hmd/Pos" + stringSize s + 7 * 4
    ConPos (VirtualDeviceTransform s _ _)       -> pathSize "/VMC/Ext/Con/Pos" + stringSize s + 7 * 4
    TraPos (VirtualDeviceTransform s _ _)       -> pathSize "/VMC/Ext/Tra/Pos" + stringSize s + 7 * 4
    SetPeriod _                                 -> pathSize "/VMC/Ext/Set/Period" + 6 * 4
    MidiCCValue _                               -> pathSize "/VMC/Ext/Midi/CC/Val" + 1 * 4 + 1 * 4
    CameraTransformFOV (VirtualCameraTransformFOV n _ _ _) -> pathSize "/VMC/Ext/Cam" + stringSize n + 7 * 4 + 1 * 4
    BlendShapeValue (VRMBlendShapeProxyValue n _) -> pathSize "/VMC/Ext/Blend/Val" + stringSize n + 1 * 4
    BlendShapeApply                             -> pathSize "/VMC/Ext/Blend/Apply"
    SetEyeTarget _                              -> pathSize "/VMC/Ext/Set/Eye" + 1 * 4 + 3 * 4
