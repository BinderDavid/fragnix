{-# LINE 1 "GHC.Natural.hs" #-}










































{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (C) 2014 Herbert Valerio Riedel,
--                (C) 2011 Edward Kmett
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The arbitrary-precision 'Natural' number type.
--
-- __Note__: This is an internal GHC module with an API subject to
-- change.  It's recommended use the "Numeric.Natural" module to import
-- the 'Natural' type.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------
module GHC.Natural
    ( -- * The 'Natural' number type
      --
      -- | __Warning__: The internal implementation of 'Natural'
      -- (i.e. which constructors are available) depends on the
      -- 'Integer' backend used!
      Natural(..)
    , isValidNatural
      -- * Conversions
    , wordToNatural
    , naturalToWordMaybe
      -- * Checked subtraction
    , minusNaturalMaybe
      -- * Modular arithmetic
    , powModNatural
    ) where




































































































































































































































































































































































import GHC.Arr
import GHC.Base
import GHC.Exception
import GHC.Integer.GMP.Internals
import Data.Word
import Data.Int
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Show
import GHC.Enum
import GHC.List

import Data.Bits
import Data.Data

default ()

-- TODO: if saturated arithmetic is to used, replace 'throw Underflow' by '0'

-- | Type representing arbitrary-precision non-negative integers.
--
-- Operations whose result would be negative
-- @'throw' ('Underflow' :: 'ArithException')@.
--
-- @since 4.8.0.0
data Natural = NatS#                 GmpLimb# -- ^ in @[0, maxBound::Word]@
             | NatJ# {-# UNPACK #-} !BigNat   -- ^ in @]maxBound::Word, +inf[@
                                              --
                                              -- __Invariant__: 'NatJ#' is used
                                              -- /iff/ value doesn't fit in
                                              -- 'NatS#' constructor.
             deriving (Eq,Ord) -- NB: Order of constructors *must*
                               -- coincide with 'Ord' relation

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (NatS# _)  = True
isValidNatural (NatJ# bn) = isTrue# (isValidBigNat# bn)
                            && I# (sizeofBigNat# bn) > 0

{-# RULES
"fromIntegral/Natural->Natural"  fromIntegral = id :: Natural -> Natural
"fromIntegral/Natural->Integer"  fromIntegral = toInteger :: Natural->Integer
"fromIntegral/Natural->Word"     fromIntegral = naturalToWord
"fromIntegral/Natural->Word8"
    fromIntegral = (fromIntegral :: Word -> Word8)  . naturalToWord
"fromIntegral/Natural->Word16"
    fromIntegral = (fromIntegral :: Word -> Word16) . naturalToWord
"fromIntegral/Natural->Word32"
    fromIntegral = (fromIntegral :: Word -> Word32) . naturalToWord
"fromIntegral/Natural->Int8"
    fromIntegral = (fromIntegral :: Int -> Int8)    . naturalToInt
"fromIntegral/Natural->Int16"
    fromIntegral = (fromIntegral :: Int -> Int16)   . naturalToInt
"fromIntegral/Natural->Int32"
    fromIntegral = (fromIntegral :: Int -> Int32)   . naturalToInt
  #-}

{-# RULES
"fromIntegral/Word->Natural"     fromIntegral = wordToNatural
"fromIntegral/Word8->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word8  -> Word)
"fromIntegral/Word16->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word16 -> Word)
"fromIntegral/Word32->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word32 -> Word)
"fromIntegral/Int->Natural"     fromIntegral = intToNatural
"fromIntegral/Int8->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int8  -> Int)
"fromIntegral/Int16->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int16 -> Int)
"fromIntegral/Int32->Natural"
    fromIntegral = intToNatural  . (fromIntegral :: Int32 -> Int)
  #-}

-- these RULES are valid for Word==Word64 & Int==Int64
{-# RULES
"fromIntegral/Natural->Word64"
    fromIntegral = (fromIntegral :: Word -> Word64) . naturalToWord
"fromIntegral/Natural->Int64"
    fromIntegral = (fromIntegral :: Int -> Int64) . naturalToInt
"fromIntegral/Word64->Natural"
    fromIntegral = wordToNatural . (fromIntegral :: Word64 -> Word)
"fromIntegral/Int64->Natural"
    fromIntegral = intToNatural . (fromIntegral :: Int64 -> Int)
  #-}

instance Show Natural where
    showsPrec p (NatS# w#)  = showsPrec p (W# w#)
    showsPrec p (NatJ# bn)  = showsPrec p (Jp# bn)

instance Read Natural where
    readsPrec d = map (\(n, s) -> (fromInteger n, s))
                  . filter ((>= 0) . (\(x,_)->x)) . readsPrec d

instance Num Natural where
    fromInteger (S# i#) | I# i# >= 0  = NatS# (int2Word# i#)
    fromInteger (Jp# bn)              = bigNatToNatural bn
    fromInteger _                     = throw Underflow

    (+) = plusNatural
    (*) = timesNatural
    (-) = minusNatural

    abs                  = id

    signum (NatS# 0##)   = NatS# 0##
    signum _             = NatS# 1##

    negate (NatS# 0##)   = NatS# 0##
    negate _             = throw Underflow

instance Real Natural where
    toRational (NatS# w)  = toRational (W# w)
    toRational (NatJ# bn) = toRational (Jp# bn)


instance Enum Natural where
    succ n = n `plusNatural`  NatS# 1##
    pred n = n `minusNatural` NatS# 1##

    toEnum = intToNatural

    fromEnum (NatS# w) | i >= 0 = i
      where
        i = fromIntegral (W# w)
    fromEnum _ = errorWithoutStackTrace "fromEnum: out of Int range"

    enumFrom x        = enumDeltaNatural      x (NatS# 1##)
    enumFromThen x y
      | x <= y        = enumDeltaNatural      x (y-x)
      | otherwise     = enumNegDeltaToNatural x (x-y) (NatS# 0##)

    enumFromTo x lim  = enumDeltaToNatural    x (NatS# 1##) lim
    enumFromThenTo x y lim
      | x <= y        = enumDeltaToNatural    x (y-x) lim
      | otherwise     = enumNegDeltaToNatural x (x-y) lim

----------------------------------------------------------------------------
-- Helpers for 'Enum Natural'; TODO: optimise & make fusion work

enumDeltaNatural :: Natural -> Natural -> [Natural]
enumDeltaNatural !x d = x : enumDeltaNatural (x+d) d

enumDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumDeltaToNatural x0 delta lim = go x0
  where
    go x | x > lim   = []
         | otherwise = x : go (x+delta)

enumNegDeltaToNatural :: Natural -> Natural -> Natural -> [Natural]
enumNegDeltaToNatural x0 ndelta lim = go x0
  where
    go x | x < lim     = []
         | x >= ndelta = x : go (x-ndelta)
         | otherwise   = [x]

----------------------------------------------------------------------------

instance Integral Natural where
    toInteger (NatS# w)  = wordToInteger w
    toInteger (NatJ# bn) = Jp# bn

    divMod = quotRem
    div    = quot
    mod    = rem

    quotRem _ (NatS# 0##) = throw DivideByZero
    quotRem n (NatS# 1##) = (n,NatS# 0##)
    quotRem n@(NatS# _) (NatJ# _) = (NatS# 0##, n)
    quotRem (NatS# n) (NatS# d) = case quotRem (W# n) (W# d) of
        (q,r) -> (wordToNatural q, wordToNatural r)
    quotRem (NatJ# n) (NatS# d) = case quotRemBigNatWord n d of
        (# q,r #) -> (bigNatToNatural q, NatS# r)
    quotRem (NatJ# n) (NatJ# d) = case quotRemBigNat n d of
        (# q,r #) -> (bigNatToNatural q, bigNatToNatural r)

    quot _       (NatS# 0##) = throw DivideByZero
    quot n       (NatS# 1##) = n
    quot (NatS# _) (NatJ# _) = NatS# 0##
    quot (NatS# n) (NatS# d) = wordToNatural (quot (W# n) (W# d))
    quot (NatJ# n) (NatS# d) = bigNatToNatural (quotBigNatWord n d)
    quot (NatJ# n) (NatJ# d) = bigNatToNatural (quotBigNat n d)

    rem _         (NatS# 0##) = throw DivideByZero
    rem _         (NatS# 1##) = NatS# 0##
    rem n@(NatS# _) (NatJ# _) = n
    rem   (NatS# n) (NatS# d) = wordToNatural (rem (W# n) (W# d))
    rem   (NatJ# n) (NatS# d) = NatS# (remBigNatWord n d)
    rem   (NatJ# n) (NatJ# d) = bigNatToNatural (remBigNat n d)

instance Ix Natural where
    range (m,n) = [m..n]
    inRange (m,n) i = m <= i && i <= n
    unsafeIndex (m,_) i = fromIntegral (i-m)
    index b i | inRange b i = unsafeIndex b i
              | otherwise   = indexError b i "Natural"


instance Bits Natural where
    NatS# n .&. NatS# m = wordToNatural (W# n .&. W# m)
    NatS# n .&. NatJ# m = wordToNatural (W# n .&. W# (bigNatToWord m))
    NatJ# n .&. NatS# m = wordToNatural (W# (bigNatToWord n) .&. W# m)
    NatJ# n .&. NatJ# m = bigNatToNatural (andBigNat n m)

    NatS# n .|. NatS# m = wordToNatural (W# n .|. W# m)
    NatS# n .|. NatJ# m = NatJ# (orBigNat (wordToBigNat n) m)
    NatJ# n .|. NatS# m = NatJ# (orBigNat n (wordToBigNat m))
    NatJ# n .|. NatJ# m = NatJ# (orBigNat n m)

    NatS# n `xor` NatS# m = wordToNatural (W# n `xor` W# m)
    NatS# n `xor` NatJ# m = NatJ# (xorBigNat (wordToBigNat n) m)
    NatJ# n `xor` NatS# m = NatJ# (xorBigNat n (wordToBigNat m))
    NatJ# n `xor` NatJ# m = bigNatToNatural (xorBigNat n m)

    complement _ = errorWithoutStackTrace "Bits.complement: Natural complement undefined"

    bitSizeMaybe _ = Nothing
    bitSize = errorWithoutStackTrace "Natural: bitSize"
    isSigned _ = False

    bit i@(I# i#) | i < finiteBitSize (0::Word) = wordToNatural (bit i)
                  | otherwise                   = NatJ# (bitBigNat i#)

    testBit (NatS# w) i = testBit (W# w) i
    testBit (NatJ# bn) (I# i#) = testBitBigNat bn i#

    -- TODO: setBit, clearBit, complementBit (needs more primitives)

    shiftL n           0 = n
    shiftL (NatS# 0##) _ = NatS# 0##
    shiftL (NatS# 1##) i = bit i
    shiftL (NatS# w) (I# i#)
        = bigNatToNatural $ shiftLBigNat (wordToBigNat w) i#
    shiftL (NatJ# bn) (I# i#)
        = bigNatToNatural $ shiftLBigNat bn i#

    shiftR n          0       = n
    shiftR (NatS# w)  i       = wordToNatural $ shiftR (W# w) i
    shiftR (NatJ# bn) (I# i#) = bigNatToNatural (shiftRBigNat bn i#)

    rotateL = shiftL
    rotateR = shiftR

    popCount (NatS# w)  = popCount (W# w)
    popCount (NatJ# bn) = I# (popCountBigNat bn)

    zeroBits = NatS# 0##

----------------------------------------------------------------------------

-- | 'Natural' Addition
plusNatural :: Natural -> Natural -> Natural
plusNatural (NatS# 0##) y         = y
plusNatural x         (NatS# 0##) = x
plusNatural (NatS# x) (NatS# y)
    = case plusWord2# x y of
       (# 0##, l #) -> NatS# l
       (# h,   l #) -> NatJ# (wordToBigNat2 h l)
plusNatural (NatS# x) (NatJ# y) = NatJ# (plusBigNatWord y x)
plusNatural (NatJ# x) (NatS# y) = NatJ# (plusBigNatWord x y)
plusNatural (NatJ# x) (NatJ# y) = NatJ# (plusBigNat     x y)

-- | 'Natural' multiplication
timesNatural :: Natural -> Natural -> Natural
timesNatural _         (NatS# 0##) = NatS# 0##
timesNatural (NatS# 0##) _         = NatS# 0##
timesNatural x         (NatS# 1##) = x
timesNatural (NatS# 1##) y         = y
timesNatural (NatS# x) (NatS# y) = case timesWord2# x y of
    (# 0##, 0## #) -> NatS# 0##
    (# 0##, xy  #) -> NatS# xy
    (# h  , l   #) -> NatJ# $ wordToBigNat2 h l
timesNatural (NatS# x) (NatJ# y) = NatJ# $ timesBigNatWord y x
timesNatural (NatJ# x) (NatS# y) = NatJ# $ timesBigNatWord x y
timesNatural (NatJ# x) (NatJ# y) = NatJ# $ timesBigNat     x y

-- | 'Natural' subtraction. May @'throw' 'Underflow'@.
minusNatural :: Natural -> Natural -> Natural
minusNatural x         (NatS# 0##) = x
minusNatural (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> NatS# l
    _           -> throw Underflow
minusNatural (NatS# _) (NatJ# _) = throw Underflow
minusNatural (NatJ# x) (NatS# y)
    = bigNatToNatural $ minusBigNatWord x y
minusNatural (NatJ# x) (NatJ# y)
    = bigNatToNatural $ minusBigNat     x y

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x         (NatS# 0##) = Just x
minusNaturalMaybe (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> Just (NatS# l)
    _           -> Nothing
  where
minusNaturalMaybe (NatS# _) (NatJ# _) = Nothing
minusNaturalMaybe (NatJ# x) (NatS# y)
    = Just $ bigNatToNatural $ minusBigNatWord x y
minusNaturalMaybe (NatJ# x) (NatJ# y)
  | isTrue# (isNullBigNat# res) = Nothing
  | otherwise = Just (bigNatToNatural res)
  where
    res = minusBigNat x y

-- | Convert 'BigNat' to 'Natural'.
-- Throws 'Underflow' if passed a 'nullBigNat'.
bigNatToNatural :: BigNat -> Natural
bigNatToNatural bn
  | isTrue# (sizeofBigNat# bn ==# 1#) = NatS# (bigNatToWord bn)
  | isTrue# (isNullBigNat# bn)        = throw Underflow
  | otherwise                         = NatJ# bn

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w#) = wordToBigNat w#
naturalToBigNat (NatJ# bn) = bn

-- | Convert 'Int' to 'Natural'.
-- Throws 'Underflow' when passed a negative 'Int'.
intToNatural :: Int -> Natural
intToNatural i | i<0 = throw Underflow
intToNatural (I# i#) = NatS# (int2Word# i#)

naturalToWord :: Natural -> Word
naturalToWord (NatS# w#) = W# w#
naturalToWord (NatJ# bn) = W# (bigNatToWord bn)

naturalToInt :: Natural -> Int
naturalToInt (NatS# w#) = I# (word2Int# w#)
naturalToInt (NatJ# bn) = I# (bigNatToInt bn)


-- | Construct 'Natural' from 'Word' value.
--
-- @since 4.8.0.0
wordToNatural :: Word -> Natural
wordToNatural (W# w#) = NatS# w#

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns 'Nothing' if value doesn't fit in 'Word'.
--
-- @since 4.8.0.0
naturalToWordMaybe :: Natural -> Maybe Word
naturalToWordMaybe (NatS# w#) = Just (W# w#)
naturalToWordMaybe (NatJ# _)  = Nothing

-- This follows the same style as the other integral 'Data' instances
-- defined in "Data.Data"
naturalType :: DataType
naturalType = mkIntType "Numeric.Natural.Natural"

instance Data Natural where
  toConstr x = mkIntegralConstr naturalType x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> errorWithoutStackTrace $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Natural"
  dataTypeOf _ = naturalType

-- | \"@'powModNatural' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- @since 4.8.0.0
powModNatural :: Natural -> Natural -> Natural -> Natural
powModNatural _           _           (NatS# 0##) = throw DivideByZero
powModNatural _           _           (NatS# 1##) = NatS# 0##
powModNatural _           (NatS# 0##) _           = NatS# 1##
powModNatural (NatS# 0##) _           _           = NatS# 0##
powModNatural (NatS# 1##) _           _           = NatS# 1##
powModNatural (NatS# b)   (NatS# e)   (NatS# m)   = NatS# (powModWord b e m)
powModNatural b           e           (NatS# m)
  = NatS# (powModBigNatWord (naturalToBigNat b) (naturalToBigNat e) m)
powModNatural b           e           (NatJ# m)
  = bigNatToNatural (powModBigNat (naturalToBigNat b) (naturalToBigNat e) m)
