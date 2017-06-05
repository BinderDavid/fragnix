{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder/ByteString.hs" #-}
------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.ByteString
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- 'Write's and 'B.Builder's for strict and lazy bytestrings.
--
-- We assume the following qualified imports in order to differentiate between
-- strict and lazy bytestrings in the code examples.
--
-- > import qualified Data.ByteString      as S
-- > import qualified Data.ByteString.Lazy as L
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.ByteString
    (
    -- * Strict bytestrings
      writeByteString
    , fromByteString
    , fromByteStringWith
    , copyByteString
    , insertByteString

    -- * Lazy bytestrings
    , fromLazyByteString
    , fromLazyByteStringWith
    , copyLazyByteString
    , insertLazyByteString

    ) where


import Blaze.ByteString.Builder.Internal.Write ( Write, exactWrite )
import Foreign
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L


-- | Write a strict 'S.ByteString' to a buffer.
writeByteString :: S.ByteString -> Write
writeByteString bs = exactWrite l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Create a 'B.Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
-- The 'B.Builder' inserts large 'S.ByteString's directly, but copies small ones
-- to ensure that the generated chunks are large on average.
fromByteString :: S.ByteString -> B.Builder
fromByteString = B.byteString
{-# INLINE fromByteString #-}


-- | Construct a 'B.Builder' that copies the strict 'S.ByteString's, if it is
-- smaller than the treshold, and inserts it directly otherwise.
--
-- For example, @fromByteStringWith 1024@ copies strict 'S.ByteString's whose size
-- is less or equal to 1kb, and inserts them directly otherwise. This implies
-- that the average chunk-size of the generated lazy 'L.ByteString' may be as
-- low as 513 bytes, as there could always be just a single byte between the
-- directly inserted 1025 byte, strict 'S.ByteString's.
--
fromByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                   -> S.ByteString -- ^ Strict 'S.ByteString' to serialize.
                   -> B.Builder    -- ^ Resulting 'B.Builder'.
fromByteStringWith = B.byteStringThreshold
{-# INLINE fromByteStringWith #-}

-- | Construct a 'B.Builder' that copies the strict 'S.ByteString'.
--
-- Use this function to create 'B.Builder's from smallish (@<= 4kb@)
-- 'S.ByteString's or if you need to guarantee that the 'S.ByteString' is not
-- shared with the chunks generated by the 'B.Builder'.
--
copyByteString :: S.ByteString -> B.Builder
copyByteString = B.byteStringCopy
{-# INLINE copyByteString #-}

-- | Construct a 'B.Builder' that always inserts the strict 'S.ByteString'
-- directly as a chunk.
--
-- This implies flushing the output buffer, even if it contains just
-- a single byte. You should therefore use 'insertByteString' only for large
-- (@> 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too
-- fragmented to be processed efficiently afterwards.
--
insertByteString :: S.ByteString -> B.Builder
insertByteString = B.byteStringInsert
{-# INLINE insertByteString #-}

-- | Create a 'B.Builder' denoting the same sequence of bytes as a lazy
-- 'S.ByteString'.
-- The 'B.Builder' inserts large chunks of the lazy 'L.ByteString' directly,
-- but copies small ones to ensure that the generated chunks are large on
-- average.
--
fromLazyByteString :: L.ByteString -> B.Builder
fromLazyByteString = B.lazyByteString
{-# INLINE fromLazyByteString #-}

-- | Construct a 'B.Builder' that uses the thresholding strategy of 'fromByteStringWith'
-- for each chunk of the lazy 'L.ByteString'.
--
fromLazyByteStringWith :: Int -> L.ByteString -> B.Builder
fromLazyByteStringWith = B.lazyByteStringThreshold
{-# INLINE fromLazyByteStringWith #-}

-- | Construct a 'B.Builder' that copies the lazy 'L.ByteString'.
--
copyLazyByteString :: L.ByteString -> B.Builder
copyLazyByteString = B.lazyByteStringCopy
{-# INLINE copyLazyByteString #-}

-- | Construct a 'B.Builder' that inserts all chunks of the lazy 'L.ByteString'
-- directly.
--
insertLazyByteString :: L.ByteString -> B.Builder
insertLazyByteString = B.lazyByteStringInsert
{-# INLINE insertLazyByteString #-}
