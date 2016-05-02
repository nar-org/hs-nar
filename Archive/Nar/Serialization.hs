-- |
-- Module      : Archive.Nar.Serialization
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Archive.Nar.Serialization
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Archive.Nar.Types
import System.IO
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

-- | header magic being in ASCII: [ NARH ]
headerMagic = HeaderType 0x5d204852414e205b

-- | per file header magic being in ASCII: [ FILE ]
perFileHeaderMagic = HeaderType 0x5d20454c4946205b

writeStorable :: Storable a => Handle -> a -> IO ()
writeStorable handle a = B.hPut handle =<< B.create (sizeOf a) (\ptr -> poke (castPtr ptr) a)

readStorable :: Storable a => Handle -> IO a
readStorable handle = doRead undefined
  where doRead :: Storable a => a -> IO a
        doRead storable = B.hGet handle (sizeOf storable) >>= \bs -> do
            let (fptr, ofs, _) = B.toForeignPtr bs
            withForeignPtr fptr $ \ptr -> peek (castPtr (ptr `plusPtr` ofs))
