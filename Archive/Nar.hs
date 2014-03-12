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
    ( CipherAlg
    , CompressionAlg
    , NarRegister
    , defaultNarRegister
    , addCiphers
    , addCompressors
    -- * default values
    , noCipher
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

-- major = 0, minor 1
currentVersion :: Version
currentVersion = Version 0x0000000100000000

data AppendFile = AppendFile
    { appendFilePath       :: B.ByteString
    , appendFileCiphered   :: Bool
    , appendFileCompressed :: Bool
    , appendFileMarkExecutable :: Bool
    , appendFileReader     :: IO L.ByteString
    }

newtype Key = Key B.ByteString

type CipherF = L.ByteString -> IO L.ByteString

data CipherAlg = CipherAlg
    { cipherInit :: Key -> IO (CipherF, CipherF)
    }

data CompressionAlg = CompressionAlg
    { compressF   :: L.ByteString -> L.ByteString
    , decompressF :: L.ByteString -> L.ByteString
    }

appendFileSimple :: FilePath -> Bool -> Bool -> AppendFile
appendFileSimple file compress isExecutable =
    AppendFile (utf8Encode file) False compress isExecutable (L.readFile file)

-- | Cipher algorithms defined
noCipher :: CipherType
noCipher = CipherType 0

-- | Compression algorithms defined
noCompression, gzipCompression :: CompressionType
noCompression = CompressionType 0
gzipCompression = CompressionType 1

defaultHeader :: Header
defaultHeader = Header headerMagic currentVersion (CipherType 0) (CompressionType 0) pos0 pos0
  where pos0 = Pos 0

type AppendF = AppendFile -> IO ()

data NarRegister = NarRegister
    { narVerbose     :: Int
    , narCiphers     :: [(CipherType, CipherAlg)]
    , narCompressors :: [(CompressionType, CompressionAlg)]
    }

defaultNarRegister :: NarRegister
defaultNarRegister = NarRegister
    { narVerbose     = 0
    , narCiphers     = [(noCipher, noCipherAlg)]
    , narCompressors = [(noCompression, noCompressAlg)
                       ,(gzipCompression, gzipAlg)
                       ]
    }
  where noCompressAlg = CompressionAlg id id
        gzipAlg = CompressionAlg
                    { compressF   = GZip.compressWith GZip.defaultCompressParams { GZip.compressLevel = GZip.bestCompression }
                    , decompressF = GZip.decompressWith GZip.defaultDecompressParams
                    }
        noCipherAlg = CipherAlg { cipherInit = \_ -> return (return . id, return . id) }

addCiphers :: [(CipherType, CipherAlg)] -> NarRegister -> NarRegister
addCiphers l reg = reg { narCiphers = l ++ narCiphers reg }

addCompressors :: [(CompressionType, CompressionAlg)] -> NarRegister -> NarRegister
addCompressors l reg = reg { narCompressors = l ++ narCompressors reg }

-- | create a new archive @archiveFile. the archive is initially empty
-- and the user need to repeatly call the append callback to add file
-- to the archive.
createNar :: NarRegister                   -- ^ a global register/config for NAR.
          -> (CipherType, CompressionType) -- ^ Cipher and compression algorithms to use
          -> FilePath                      -- ^ The archive filename
          -> (AppendF -> IO ())            -- ^ the append callback
          -> IO ()
createNar narReg (cipherTy, compressionTy) archiveFile f = do
    let cipherAlg   = maybe (error "cannot find cipher") id $ lookup cipherTy (narCiphers narReg)
        compressAlg = maybe (error "cannot find compressor") id $ lookup compressionTy (narCompressors narReg)
    withFile archiveFile WriteMode $ \handle -> do
        let hdr = defaultHeader { cipherType      = cipherTy
                                , compressionType = compressionTy
                                }
        writeStorable handle hdr
        f (doAppend cipherAlg compressAlg handle)
  where doAppend cipherAlg compressAlg handle af = do
            let fp = appendFilePath af

            -- align to next file and record the position
            hAlignWrite handle 8
            pos <- hTell handle
            -- write a blank per file header
            writeStorable handle $ PerFileHeader perFileHeaderMagic
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
            let pfh = PerFileHeader perFileHeaderMagic
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
        unless (version hdr <= currentVersion) $ error "unknown version"

        let cipherAlg   = maybe (error "cannot find cipher") id $ lookup (cipherType hdr) (narCiphers narReg)
            compressAlg = maybe (error "cannot find compressor") id $ lookup (compressionType hdr) (narCompressors narReg)

        doRead cipherAlg compressAlg handle
  where doRead cipherAlg compressAlg handle = do
            eof <- hAlignRead handle 8
            if eof
                then return ()
                else do
                    pfh   <- readStorable handle
                    let fsz = fromIntegral $ unLength $ fLength pfh
                    -- TODO figure out filepath ciphering
                    fpath <- B.hGet handle (fromIntegral $ unLength $ fpathLength pfh)
                    hAlignRead handle 8
                    pos <- hTell handle
                    -- LAZY issue ?
                    f (fpath, readContent fsz (isFileFlagSet Compressed pfh))
                    pos2 <- hTell handle
                    -- if pos2 hasn't moved, nobody consumed the file, skip it.
                    _ <- if pos2 > pos
                             then hAlignRead handle fsz >> hAlignRead handle 8
                             else hAlignRead handle 8
                    doRead cipherAlg compressAlg handle
          where readContent fsz compressed = do
                    -- TODO decipher
                    (if compressed then decompressF compressAlg else id) <$> L.hGet handle fsz

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
