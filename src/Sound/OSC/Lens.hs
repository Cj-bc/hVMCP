{- |
Module      :  Sound.OSC.Lens
Description :  Some hand-made lens for Sound.OSC module
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Sound.OSC.Lens where
import Control.Lens
import Sound.OSC

makePrisms ''Datum
