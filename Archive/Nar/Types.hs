-- |
-- Module      : Archive.Nar.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE CPP #-}
module Archive.Nar.Types
    ( HeaderType(..)
    , Header(..)
    , Flags(..)
    , CompressionType(..)
    , SignatureType(..)
    , PerFileFlags
    , perFileFlags
    , Length(..)
    , Pos(..)
    , sizeOfHeader
    , FileHeader(..)
    , FileFlag(..)
    , isFileFlagSet
    ) where

import Control.Applicative
import Data.Word
import Data.Bits (testBit, setBit)
import Foreign.Storable
import Foreign.Ptr

-- | Defined flags for files
data FileFlag =
      Executable -- ^ file is executable (bit 0)
    | Compressed -- ^ file is compressed (bit 1)
    | Ciphered   -- ^ file is ciphered   (bit 2)
    deriving (Show,Eq,Enum)

-- | Magic value, used to distinguish file format and per file header
newtype HeaderType = HeaderType { unHeaderType :: Word64 }
    deriving (Show,Eq,Ord)

-- | Version of the file
newtype Flags = Flags { unFlags :: Word64 }
    deriving (Show,Eq,Ord)

-- | Compression type
newtype CompressionType = CompressionType { unCompressionType :: Word64 }
    deriving (Show,Eq,Ord)

-- | Signature type
newtype SignatureType = SignatureType { unSignatureType :: Word64 }
    deriving (Show,Eq,Ord)

-- | Length for a field
newtype Length = Length { unLength :: Word64 }
    deriving (Show,Eq,Ord)

newtype Pos = Pos { unPos :: Word64 }
    deriving (Show,Eq,Ord)

-- | Per file flags
newtype PerFileFlags = PerFileFlags { unPerFileFlags :: Word64 }
    deriving (Show,Eq,Ord)

-- | NAR Header
data Header = Header
    { magic   :: HeaderType
    , flags   :: Flags
    , length1 :: Length
    , length2 :: Length
    } deriving (Show,Eq)

data FileHeader = FileHeader Header
    deriving (Show,Eq)

sizeOfHeader :: Int
sizeOfHeader = sizeOf (undefined :: Header)

instance Storable Length where
    sizeOf _     = 8
    alignment _  = 8
    peek ptr     = peekW64 Length ptr 0
    poke ptr (Length v) = pokeW64 ptr 0 v

instance Storable Pos where
    sizeOf _     = 8
    alignment _  = 8
    peek ptr     = peekW64 Pos ptr 0
    poke ptr (Pos v) = pokeW64 ptr 0 v

instance Storable Header where
    sizeOf _     = 4 * 8
    alignment _  = 8
    peek ptr     =
        Header <$> peekW64 HeaderType ptr 0
               <*> peekW64 Flags ptr 8
               <*> peekW64 Length ptr 16
               <*> peekW64 Length ptr 24
    poke ptr hdr = sequence_
        [ pokeW64 ptr 0 $ unHeaderType $ magic hdr
        , pokeW64 ptr 8 $ unFlags $ flags hdr
        , pokeW64 ptr 16 $ unLength $ length1 hdr
        , pokeW64 ptr 24 $ unLength $ length2 hdr
        ]

peekW64 :: (Word64 -> b) -> Ptr a -> Int -> IO b
peekW64 constr ptr off = (constr . toLE) `fmap` peek (castPtr ptr `plusPtr` off)

pokeW64 :: Ptr a -> Int -> Word64 -> IO ()
pokeW64 ptr off val = poke (castPtr ptr `plusPtr` off) (toLE val)

-- | convert a little endian Word64 to a host Word64, and vice-versa.
toLE :: Word64 -> Word64
#if BIG_ENDIAN
toLE = id -- FIXME obviously not byte swapping here.
#else
toLE = id
#endif

isFileFlagSet :: FileFlag -> Header -> Bool
isFileFlagSet fileFlag pfh = v `testBit` bit
  where bit = fromEnum fileFlag
        (Flags v) = flags pfh

perFileFlags :: [FileFlag] -> Flags
perFileFlags = Flags . foldl doAcc 0
  where doAcc v flag = setBit v (fromEnum flag)
