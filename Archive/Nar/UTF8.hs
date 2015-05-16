-- |
-- Module      : Archive.Nar.UTF8
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- a tiny UTF8 decoding/encoding
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module Archive.Nar.UTF8
    ( utf8Encode
    , utf8Decode
    ) where

import qualified Data.ByteString as B
import           Data.Bits
import           GHC.Prim
import           GHC.Word
import           GHC.Types

data Table = Table Addr#

indexTableI :: Table -> Word8 -> Int
indexTableI (Table addr) (W8# i) = I# (word2Int# (indexWord8OffAddr# addr (word2Int# i)))

indexTableB :: Table -> Word8 -> Bool
indexTableB (Table addr) (W8# i) = W# (indexWord8OffAddr# addr (word2Int# i)) /= W# 0##

utf8Decode :: B.ByteString -> String
utf8Decode = decode . B.unpack
  where decode :: [Word8] -> [Char]
        decode []     = []
        decode (x:xs) =
            case getNbBytes x of
                0 -> toEnum (fromIntegral x) : decode xs
                1 -> case xs of
                        b1:xs2
                            | isCont b1 -> toChar (x .&. 0x1f) [b1] : decode xs2
                            | otherwise -> error "continuation bytes invalid"
                        _               -> error "not enough bytes (1) "
                2 -> case xs of
                        b1:b2:xs2
                            | and $ map isCont [b1,b2] -> toChar (x .&. 0xf) [b1,b2] : decode xs2
                            | otherwise                -> error "continuation bytes invalid"
                        _                              -> error "not enough bytes (2)"
                3 -> case xs of
                        b1:b2:b3:xs2
                            | and $ map isCont [b1,b2,b3] -> toChar (x .&. 0x7) [b1,b2,b3] : decode xs2
                            | otherwise                   -> error "continuation bytes invalid"
                        _                                 -> error "not enough bytes (3)"
                _ -> error "invalid heading byte"
        toChar :: Word8 -> [Word8] -> Char
        toChar h l = toEnum $ foldl (\acc v -> (acc `shiftL` 6) + clearCont v) (fromIntegral h) l
          where clearCont w = fromIntegral (w `clearBit` 7)

        getNbBytes = indexTableI headTable
        headTable = Table
            "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\
            \\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\
            \\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\x02\
            \\x03\x03\x03\x03\x03\x03\x03\x03\xff\xff\xff\xff\xff\xff\xff\xff"#

        isCont :: Word8 -> Bool
        isCont = indexTableB contTable

        contTable = Table
            "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
            \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

utf8Encode :: String -> B.ByteString
utf8Encode = B.pack . concatMap unf . map fromEnum
  where unf x
            | x < 0x80     = [fromIntegral x]
            | x < 0x07ff   = [0xc0 .|. (sh 6 .&. 0x1f), cont 0]
            | x < 0xffff   = [0xe0 .|. (sh 12 .&. 0xf), cont 6, cont 0]
            | otherwise    = [0xf0 .|. (sh 18 .&. 0x7), cont 12, cont 6, cont 0]
          where sh w   = fromIntegral (x `shiftR` w)
                cont w = (sh w `setBit` 7) `clearBit` 6
