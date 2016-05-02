-- |
-- Module      : Archive.Nar
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- haskell implementation of the Nar archiving format.
--
module Archive.Nar
    ( CompressionAlg
    , NarRegister
    , defaultNarRegister
    -- * default values
    , noCompression
    , gzipCompression
    -- * appender helpers
    , AppendFile(..)
    , appendFileSimple
    -- * methods
    , createNar
    , iterateNar
    ) where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
--import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

import Archive.Nar.Types
import Archive.Nar.Serialization
import Archive.Nar.UTF8

import System.IO

import qualified Codec.Compression.GZip as GZip

data AppendFile = AppendFile
    { appendFilePath       :: B.ByteString
    , appendFileCiphered   :: Bool
    , appendFileCompressed :: Bool
    , appendFileMarkExecutable :: Bool
    , appendFileReader     :: IO L.ByteString
    }

newtype Key = Key B.ByteString

data CompressionAlg = CompressionAlg
    { compressF   :: L.ByteString -> L.ByteString
    , decompressF :: L.ByteString -> L.ByteString
    }

appendFileSimple :: FilePath -> Bool -> Bool -> AppendFile
appendFileSimple file compress isExecutable =
    AppendFile (utf8Encode file) False compress isExecutable (L.readFile file)

-- | Compression algorithms defined
noCompression, gzipCompression :: CompressionType
noCompression = CompressionType 0
gzipCompression = CompressionType 1

defaultHeader :: Header
defaultHeader = Header headerMagic (Flags 0) len0 len0
  where len0 = Length 0

type AppendF = AppendFile -> IO ()

data NarRegister = NarRegister
    { narVerbose     :: Int
    , narCompressors :: [(CompressionType, CompressionAlg)]
    }

defaultNarRegister :: NarRegister
defaultNarRegister = NarRegister
    { narVerbose     = 0
    , narCompressors = [(noCompression, noCompressAlg)
                       ,(gzipCompression, gzipAlg)
                       ]
    }
  where noCompressAlg = CompressionAlg id id
        gzipAlg = CompressionAlg
                    { compressF   = GZip.compressWith GZip.defaultCompressParams { GZip.compressLevel = GZip.bestCompression }
                    , decompressF = GZip.decompressWith GZip.defaultDecompressParams
                    }

addCompressors :: [(CompressionType, CompressionAlg)] -> NarRegister -> NarRegister
addCompressors l reg = reg { narCompressors = l ++ narCompressors reg }

-- | create a new archive @archiveFile. the archive is initially empty
-- and the user need to repeatly call the append callback to add file
-- to the archive.
createNar :: NarRegister        -- ^ a global register/config for NAR.
          -> CompressionType    -- ^ Cipher and compression algorithms to use
          -> FilePath           -- ^ The archive filename
          -> (AppendF -> IO ()) -- ^ the append callback
          -> IO ()
createNar narReg compressionTy archiveFile f = do
    let compressAlg = maybe (error "cannot find compressor") id $ lookup compressionTy (narCompressors narReg)
    withFile archiveFile WriteMode $ \handle -> do
        let hdr = defaultHeader
        writeStorable handle hdr
        f (doAppend compressAlg handle)
  where doAppend compressAlg handle af = do
            let fp = appendFilePath af

            -- align to next file and record the position
            hAlignWrite handle 8
            pos <- hTell handle
            -- write a blank per file header
            writeStorable handle $ Header perFileHeaderMagic
                                          (perFileFlags [])
                                          (Length $ fromIntegral $ B.length fp)
                                          (Length 0)
            -- write the filepath. TODO figure out filepath encryption.
            B.hPut handle fp
            hAlignWrite handle 8
            -- write the content
            posContent <- hTell handle
            appendContent
            posEnd <- hTell handle

            -- now write the per file header back to the position we registered
            hSeek handle AbsoluteSeek pos
            let pfh = Header perFileHeaderMagic
                             (perFileFlags [])
                             (Length $ fromIntegral $ B.length fp)
                             (Length $ fromIntegral $ posEnd - posContent)
            writeStorable handle pfh
            hSeek handle AbsoluteSeek posEnd
          where appendContent = do
                    -- TODO: use cipher
                    stream <- appendFileReader af
                    let stream' = (compressF compressAlg) stream
                    mapM_ (B.hPut handle) $ L.toChunks stream'

-- | Iterate over all contents of a narfile.
iterateNar :: NarRegister -> FilePath -> ((B.ByteString, IO L.ByteString) -> IO ()) -> IO ()
iterateNar narReg archiveFile f =
    withFile archiveFile ReadMode $ \handle -> do
        hdr <- readStorable handle
        unless (magic hdr == headerMagic) $ error "not a nar file"

        doRead handle
  where doRead handle = do
            eof <- hAlignRead handle 8
            if eof
                then return ()
                else do
                    pfh   <- readStorable handle
                    let fsz = fromIntegral $ unLength $ length2 pfh
                    fpath <- B.hGet handle (fromIntegral $ unLength $ length1 pfh)
                    hAlignRead handle 8
                    pos <- hTell handle
                    -- LAZY issue ?
                    f (fpath, readContent fsz)
                    pos2 <- hTell handle
                    -- if pos2 hasn't moved, nobody consumed the file, skip it.
                    _ <- if pos2 > pos
                             then hAlignRead handle fsz >> hAlignRead handle 8
                             else hAlignRead handle 8
                    doRead handle
          where readContent fsz = do
                   -- TODO decipher
                    {-(if compressed then decompressF compressAlg else id) <$>-} L.hGet handle fsz

hAlignWrite :: Handle -> Int -> IO ()
hAlignWrite h n =
    hTell h >>= \i -> unless ((i `mod` fromIntegral n) == 0) (realign i)
  where realign i = B.hPut h $ B.replicate (n - fromIntegral (i `mod` fromIntegral n)) 0

-- | `align` a handle, by reading from it.
--
-- if we reach end of file, True is returned.
--
-- TODO: use seek instead of reading bytes from handle.
-- RelativeSeek doesn't error out when going out of bounds.
hAlignRead :: Handle -> Int -> IO Bool
hAlignRead h n =
    hTell h >>= \i -> if ((i `mod` fromIntegral n) == 0) then realign i else return False
  where realign i = do
            let toRead = n - fromIntegral (i `mod` fromIntegral n)
            afterRead <- B.length <$> B.hGet h toRead
            if afterRead < toRead
                then return True
                else return False

{-
roundUpTo :: Integral a => a -> a -> a
roundUpTo n m
    | n `mod` m == 0 = n
    | otherwise      = n + m - (n `mod` m)

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = let (d, m) = a `divMod` b in d + if m > 0 then 1 else 0
-}
