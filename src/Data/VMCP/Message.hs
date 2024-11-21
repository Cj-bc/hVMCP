{- |
Module      :  Data.VMCP.Message
Description :  General functions for VMCP messages
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable


-}
module Data.VMCP.Message where
import Data.Bits ((.&.), complement)
import Sound.Osc (Message, Bundle(..), bundle)

-- | VMCP's message types.
--
-- This class contains some utility functions
-- for VMCP message manupulation
class VMCPMessage msg where
  -- | Byte size of given message
  byteSize :: msg -> Int

  -- | Convert 'Message' to 'msg'
  fromOSCMessage :: Message -> Maybe msg

  -- | Convert 'msg' to 'Message'
  toOSCMessage :: msg -> Message


-- | Each OSC Bundle should be smaller than this value in byte.
--
--
-- > パケットは適切な範囲(1500byte以内)でbundle化されており、受信者は適切に扱う必要があります。
-- URL: https://protocol.vmc.info/specification
oneBundleMaxByteSize = 1500

-- | Optimized version
calcBundleSize :: VMCPMessage msg => [msg] -> Int
calcBundleSize msgs = headerSize + bundleElementSize + (sum $ fmap byteSize msgs)
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

-- | Convert 'Bundle' into list of 'MarionetteMsg'
--
-- TODO: Currently it ommit 'OSC-timetag'.
-- I better to use them someway.
fromOSCBundle :: VMCPMessage msg => Bundle -> Maybe [msg]
fromOSCBundle (Bundle t msgs) = mapM fromOSCMessage msgs


-- | Convert List of 'MarionetteMsg' into one 'Bundle'
--
-- TODO: 'OSC-timetag' isn't used properly.
-- I must find good way to set it.
toOSCBundle :: VMCPMessage msg => [msg] -> Bundle
toOSCBundle = bundle 0 . fmap toOSCMessage


-- | Create 'Bundle's from list of 'MarionetteMsg', taking into account of 'oneBundleMaxByteSize'
--
-- TODO: Improve bundle splitting efficiency.
-- It's best if we can determine size before actually construct 'Bundle'
-- Maybe we can pre-calculate them because all 'HumanBodyBones' are supplied
-- in code.
mkBundles :: VMCPMessage msg => [msg] -> [Bundle]
mkBundles msgs =
  let halfLen = length msgs `div` 2
  in if calcBundleSize msgs >= oneBundleMaxByteSize
     then toOSCBundle <$> [take halfLen msgs, drop halfLen msgs]
     else [toOSCBundle msgs]
