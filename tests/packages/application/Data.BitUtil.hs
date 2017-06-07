{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/BitUtil.hs" #-}























































{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitUtil
-- Copyright   :  (c) Clark Gaebel 2012
--                (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.BitUtil
    ( highestBitMask
    ) where

-- On GHC, include MachDeps.h to get WORD_SIZE_IN_BITS macro.


































































































































































































































































































































































import Data.Bits ((.|.), xor)

import GHC.Exts (Word(..), Int(..))
import GHC.Prim (uncheckedShiftRL#)

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask x1 = let x2 = x1 .|. x1 `shiftRL` 1
                        x3 = x2 .|. x2 `shiftRL` 2
                        x4 = x3 .|. x3 `shiftRL` 4
                        x5 = x4 .|. x4 `shiftRL` 8
                        x6 = x5 .|. x5 `shiftRL` 16
                        x7 = x6 .|. x6 `shiftRL` 32
                     in x7 `xor` (x7 `shiftRL` 1)
{-# INLINE highestBitMask #-}

-- Right and left logical shifts.
shiftRL :: Word -> Int -> Word
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
{-# INLINE shiftRL #-}