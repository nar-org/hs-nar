-- |
-- Module      : Archive.Nar.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE CPP #-}
module Archive.Nar.Types
    ( Magic(..)
    , Version(..)
    , Header(..)
    , CipherType(..)
    , CompressionType(..)
    , SignatureType(..)
    , PerFileFlags
    , perFileFlags
    , Length(..)
    , Pos(..)
    , sizeOfHeader
    , PerFileHeader(..)
    , sizeOfPerFileHeader
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
newtype Magic = Magic { unMagic :: Word64 }
    deriving (Show,Eq,Ord)

-- | Version of the file
newtype Version = Version { unVersion :: Word64 }
    deriving (Show,Eq,Ord)

-- | Cipher type
newtype CipherType = CipherType { unCipherType :: Word64 }
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
    { magic                :: Magic
    , version              :: Version
    , cipherType           :: CipherType
    , compressionType      :: CompressionType
    , signaturePos         :: Pos
    , indexPos             :: Pos
    } deriving (Show,Eq)

-- | NAR per-file Header
data PerFileHeader = PerFileHeader
    { fmagic      :: Magic
    , fflags      :: PerFileFlags
    , fpathLength :: Length
    , fLength     :: Length
    }

sizeOfHeader, sizeOfPerFileHeader :: Int
sizeOfHeader = sizeOf (undefined :: Header)
sizeOfPerFileHeader = sizeOf (undefined :: PerFileHeader)

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

instance Storable Magic where
    sizeOf _     = 8
    alignment _  = 8
    peek ptr     = peekW64 Magic ptr 0
    poke ptr (Magic v) = pokeW64 ptr 0 v

instance Storable Header where
    sizeOf _     = 64
    alignment _  = 8
    peek ptr     =
        Header <$> peek (castPtr ptr)
               <*> peekW64 Version ptr 8
               <*> peekW64 CipherType ptr 16
               <*> peekW64 CompressionType ptr 24
               <*> peekByteOff ptr 32
               <*> peekByteOff ptr 40
    poke ptr hdr = sequence_
        [ pokeW64 ptr 0 $ unMagic $ magic hdr
        , pokeW64 ptr 8 $ unVersion $ version hdr
        , pokeW64 ptr 16 $ unCipherType $ cipherType hdr
        , pokeW64 ptr 24 $ unCompressionType $ compressionType hdr
        , pokeW64 ptr 32 $ unPos $ signaturePos hdr
        , pokeW64 ptr 40 $ unPos $ indexPos hdr
        , pokeW64 ptr 48 0
        , pokeW64 ptr 56 0
        ]

instance Storable PerFileHeader where
    sizeOf _     = 16
    alignment _  = 8
    peek ptr     =
        PerFileHeader <$> peekW64 Magic ptr 0
                      <*> peekW64 PerFileFlags ptr 8
                      <*> peekW64 Length ptr 16
                      <*> peekW64 Length ptr 24
    poke ptr pfh = sequence_
        [ pokeW64 ptr 0 $ unMagic $ fmagic pfh
        , pokeW64 ptr 8 $ unPerFileFlags $ fflags pfh
        , pokeW64 ptr 16 $ unLength $ fpathLength pfh
        , pokeW64 ptr 24 $ unLength $ fLength pfh
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

isFileFlagSet :: FileFlag -> PerFileHeader -> Bool
isFileFlagSet fileFlag pfh = v `testBit` bit
  where bit = fromEnum fileFlag
        (PerFileFlags v) = fflags pfh

perFileFlags :: [FileFlag] -> PerFileFlags
perFileFlags = PerFileFlags . foldl doAcc 0
  where doAcc v flag = setBit v (fromEnum flag)
