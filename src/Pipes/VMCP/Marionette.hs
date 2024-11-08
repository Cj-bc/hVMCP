{-# LANGUAGE RankNTypes #-}
module Pipes.VMCP.Marionette where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_, forever)
import Pipes
import Sound.Osc
import Sound.Osc.Coding.Encode.Builder (encodeBundle)
import Sound.Osc.Coding.Byte (bundleHeader_strict)
import Sound.Osc.Transport.Fd.Udp (udpServer, udp_server, openUdp, udp_send_packet)
import qualified Sound.Osc.Transport.Fd as FD
import Data.VMCP.Marionette
import Data.VMCP.Message (VMCPMessage(..), calcBundleSize, oneBundleMaxByteSize, toOSCBundle, fromOSCBundle)
import qualified Data.ByteString.Lazy as L

recvMarionetteMsgAsBundle :: MonadIO m => String -> Int -> Producer [MarionetteMsg] m ()
recvMarionetteMsgAsBundle addr p = do
  bundles <- liftIO $ FD.withTransport (udp_server p) (fmap fromOSCBundle . FD.recvBundle)
  maybe (pure ()) yield bundles
  recvMarionetteMsgAsBundle addr p

-- | Produce 'MarionetteMsg' continuously 
recvMarionetteMsg :: MonadIO m => String -> Int -> Producer MarionetteMsg m ()
recvMarionetteMsg addr p = do
  udp <- (liftIO $ udpServer addr p)
  recvMarionetteMsgWithUdp udp
  liftIO $ FD.close udp

-- | Produce 'MarionetteMsg' by using given 'UDP'.
--
-- It is prefered to use this with bracket functions than 'recvMarionetteMsg',
--
--    withTransport (udp_server YOURPORT) $ \socket -> recvMarionetteMsgWithUdp socket
recvMarionetteMsgWithUdp :: MonadIO m => Udp -> Producer MarionetteMsg m ()
recvMarionetteMsgWithUdp udp = do
  bundles <- liftIO . fmap fromOSCBundle $ FD.recvBundle udp
  forM_ bundles each
  recvMarionetteMsgWithUdp udp

-- | Send 'MarionetteMsg' to given addr/port.
--
-- It will automatically bundle some 'MarionetteMsg's into 'Budle'
--
-- TODO: This implementation is tedious and could be performance issue.
-- We have to find good way to calculate length of OSC bundle in bytes without
-- actually convert them into ByteString every time.
sendMarionetteMsg :: MonadIO m => String -> Int -> Consumer MarionetteMsg m ()
sendMarionetteMsg addr p = mkPacket >-> sendOne
  where
    sendOne = for cat $ liftIO . FD.withTransport (openUdp addr p) . flip udp_send_packet

-- | Make OSC 'Packet' from 'MarionetteMsg'.
--
-- It'll make 'Bundle' packet if:
--
-- + Given 'MarionetteMsg' is 'VRMBlendShapeProxyValue'
-- + Given 'MarionetteMsg' is 'BoneTransform'
mkPacket :: MonadIO m => Pipe MarionetteMsg Packet m ()
mkPacket = for cat mkPacket'

-- | Internal function for support recieving 'MarionetteMsg' as argunment
mkPacket' :: MonadIO m => MarionetteMsg -> Pipe MarionetteMsg Packet m ()
mkPacket' msg =
  case msg of
    (VRMBlendShapeProxyValue _ _) -> do
      (nextMsg, bs) <- mkBlendShapeProxyBundle [msg]
      each $ Packet_Bundle <$> bs
      maybe (pure ()) mkPacket' nextMsg
      
    (BoneTransform _ _ _) -> do
      (nextMsg, bs) <- mkBoneTransformBundle [msg]
      each $ Packet_Bundle <$> bs
      mkPacket' nextMsg
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
mkBoneTransformBundle :: MonadIO m => [MarionetteMsg] -> Consumer' MarionetteMsg m (MarionetteMsg, [Bundle])
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
mkBlendShapeProxyBundle :: MonadIO m => [MarionetteMsg] -> Consumer' MarionetteMsg m (Maybe MarionetteMsg, [Bundle])
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
