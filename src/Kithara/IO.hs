{-|
Description : Common IO operations.

This module contains basic IO primitives that can be useful
in the context of a sound creation.
Examples include converting a melody to a ByteString ('soundToByteString')
and playing it via [ffplay](https://ffmpeg.org/ffplay.html) ('playSong').
-}
module Kithara.IO (
    soundToByteString, saveByteStringSound,
    saveSong, saveSongWithNoises,
    playSong
) where

import Kithara.Types
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as BSLazy
import System.Process (runCommand)
import Text.Printf (printf)
import Data.Foldable (fold)

-- |Convert 'Kithara.Sound' to a
-- [BSLazy.ByteString](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Lazy.html#t:ByteString).
soundToByteString :: Sound -> BSLazy.ByteString
soundToByteString s = BSBuilder.toLazyByteString $ fold builders
    where
        builders = map BSBuilder.floatLE s

-- |Save
-- [BSLazy.ByteString](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Lazy.html#t:ByteString)
-- to a file with provided path.
saveByteStringSound :: FilePath -> BSLazy.ByteString -> IO ()
saveByteStringSound = BSLazy.writeFile

-- |Save 'Kithara.Sound' to a file with provided path.
saveSong :: Sound -> FilePath -> IO ()
saveSong s fp = saveByteStringSound fp (soundToByteString s)

-- |Saves song that includes randomly generated noises
-- and has type 'IO' 'Sound' because of that.
saveSongWithNoises :: IO Sound -> FilePath -> IO ()
saveSongWithNoises s fp = do
    s' <- s
    saveSong s' fp

-- |Play a sound stored in a file with provided path using
-- [ffplay](https://ffmpeg.org/ffplay.html).
playSong :: FilePath
         -> Samples
         -> Int      -- ^ Showmode for ffplay (0, 1 or 2).
         -> String   -- ^ File encoding (e.g. "f32le").
         -> IO ()
playSong fp s shm enc = do
    _ <- runCommand $ printf "ffplay -autoexit -showmode %d -f %s -ar %d %s" shm enc s fp
    return ()
