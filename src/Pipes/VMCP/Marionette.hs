{-# LANGUAGE RankNTypes #-}
module Pipes.VMCP.Marionette where

import Control.Monad (forM_)
import Pipes
import Sound.OSC
import Sound.OSC.Coding.Encode.Builder (encodeBundle)
import Sound.OSC.Transport.FD.UDP (udpServer, openUDP, upd_send_packet)
import qualified Sound.OSC.Transport.FD as FD
import Data.VMCP.Marionette
import qualified Data.ByteString.Lazy as L

-- | Each OSC Bundle should be smaller than this value in byte.
--
--
-- > パケットは適切な範囲(1500byte以内)でbundle化されており、受信者は適切に扱う必要があります。
-- URL: https://protocol.vmc.info/specification
oneBundleMaxByteSize = 1500

-- | Optimized version
calcBundleSize :: [MarionetteMsg] -> Int
calcBundleSize msgs = headerSize + bundleElementSize + (sum $ fmap bytesize msgs)
  where
    -- | Size of OSC-string "#bundle" + OSC-timetag
    -- 
    -- each of them have 8 bytes, so 16 bytes in total
    -- Reference: https://cnmat.org/OpenSoundControl/OSC-spec.html
    headerSize = 16
    -- | Size of int32 value that express how much byte will next content have.
    bundleElementSize = 4 * (length msgs)

byteSizeWithPadding :: Show a => a -> Int
byteSizeWithPadding = let align' n = (n + 3) .&. complement 3
                      in align' . length . show

bytesize :: MarionetteMsg -> Int
bytesize msg = case msg of
                 VRMBlendShapeProxyApply -> 28 -- ^ "/VMC/Ext/Blend/Apply0000,000"
                 (VRMBlendShapeProxyValue name _) -> byteSizeWithPadding name + 32
                 -- ^ "/VMC/Ext/Blend/Value0000,sf0<Name><Float 4bytes>"
                 (BoneTransform bone _ _)         -> byteSizeWithPadding bone + 60
                 -- ^ "/VMC/Ext/Bone/Pos000,sfffffff000<String >[<Float 4bytes>*7]"
                 _ -> 0
                 -- TODO: Should I implement this function for other value constructors?


-- | Produce 'MarionetteMsg' continuously 
--
-- TODO: Don't reopen udpServer each time. I think it's better implementation.
recvMarionetteMsg :: String -> Int -> Producer MarionetteMsg IO ()
recvMarionetteMsg addr p = do
  bundles <- lift $ FD.withTransport (udpServer addr p) (fmap fromOSCBundle . FD.recvBundle)
  forM_ bundles each
  recvMarionetteMsg addr p

-- | Send 'MarionetteMsg' to given addr/port.
--
-- It will automatically bundle some 'MarionetteMsg's into 'Budle'
--
-- TODO: This implementation is tedious and could be performance issue.
-- We have to find good way to calculate length of OSC bundle in bytes without
-- actually convert them into ByteString every time.
sendMarionetteMsg :: String -> Int -> Consumer MarionetteMsg IO ()
sendMarionetteMsg addr p = mkPacket >-> sendOne >> sendMarionetteMsg addr p
  where
    sendOne = do
      packet <- await
      lift $ FD.withTransport (openUDP addr p) (flip upd_send_packet packet) 

-- | Make OSC 'Packet' from 'MarionetteMsg'.
--
-- It'll make 'Bundle' packet if:
--
-- + Given 'MarionetteMsg' is 'VRMBlendShapeProxyValue'
-- + Given 'MarionetteMsg' is 'BoneTransform'
mkPacket :: Pipe MarionetteMsg Packet IO ()
mkPacket = await >>= mkPacket'

-- | Internal function for support recieving 'MarionetteMsg' as argunment
mkPacket' :: MarionetteMsg -> Pipe MarionetteMsg Packet IO ()
mkPacket' msg =
  case msg of
    (VRMBlendShapeProxyValue _ _) -> do
      (nextMsg, bs) <- mkBlendShapeProxyBundle [msg]
      each $ Packet_Bundle <$> bs
      mkPacket' msg
      
    (BoneTransform _ _ _) -> do
      (nextMsg, bs) <- mkBoneTransformBundle [msg]
      each $ Packet_Bundle <$> bs
      mkPacket' msg
    _ -> yield $ Packet_Message (toOSCMessage msg)


-- | Make 'Bundle' for 'BoneTransform'
--
-- It will return defined 'Bundle' and 'MarionetteMsg' that will be
-- used in next step.
--
-- When entire 'Bundle' gets bigger than 'oneBundleMaxByteSize',
-- this will split into two automatically.
--
-- 'prevMsgs' are stored in reversed order.
-- However, this will reverse it when finally create bundle.
mkBoneTransformBundle :: [MarionetteMsg] -> Consumer' MarionetteMsg IO (MarionetteMsg, [Bundle])
mkBoneTransformBundle prevMsgs = do
  msg <- await
  case msg of
    (BoneTransform _ _ _) ->
      mkBoneTransformBundle (msg:prevMsgs)
    _ ->
      -- As we put 'prevMsgs' in reversed order, it should ver 'reverse'd
      return (msg, mkBundles (reverse prevMsgs))



-- | Make 'Bundle' for 'VRMBlendShapeProxyValue'
--
-- It will return defined 'Bundle' and 'MarionetteMsg' that will be
-- used in next step if exists.
--
-- When entire 'Bundle' gets bigger than 'oneBundleMaxByteSize',
-- this will split into two automatically.
--
-- 'prevMsgs' are stored in reversed order.
-- However, this will reverse it when finally create bundle.
mkBlendShapeProxyBundle :: [MarionetteMsg] -> Consumer' MarionetteMsg IO (Maybe MarionetteMsg, [Bundle])
mkBlendShapeProxyBundle prevMsgs = do
  msg <- await
  case msg of
    (VRMBlendShapeProxyValue _ _) ->
      mkBlendShapeProxyBundle (msg:prevMsgs)
    VRMBlendShapeProxyApply ->
      -- As we put 'prevMsgs' in reversed order, it should ver 'reverse'd
      return (Nothing, mkBundles (reverse $ msg:prevMsgs))
    -- Oops! Actually this should not happen.
    -- 'VRMBlendShapeProxyValue's should be followed by 'VRMBlendShapeProxyApply',
    -- but other type of message is coming.
    -- For now, add 'VRMBlendShapeProxyApply' and return.
    -- 
    -- TODO: are there better way?
    _ ->
      return (Just msg, mkBundles (reverse $ VRMBlendShapeProxyApply:prevMsgs))


-- | Create 'Bundle's from list of 'MarionetteMsg', taking into account of 'oneBundleMaxByteSize'
--
-- TODO: Improve bundle splitting efficiency.
-- It's best if we can determine size before actually construct 'Bundle'
-- Maybe we can pre-calculate them because all 'HumanBodyBones' are supplied
-- in code.
mkBundles :: [MarionetteMsg] -> [Bundle]
mkBundles msgs =
  let halfLen = length msgs `div` 2
  in if calcBundleSize msgs >= oneBundleMaxByteSize
     then toOSCBundle <$> [take halfLen msgs, drop halfLen msgs]
     else [toOSCBundle msgs]
