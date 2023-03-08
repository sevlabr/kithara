{-|
Description : Common IO operations.

This module contains basic IO primitives that can be useful
in the context of a sound creation.
Examples include converting a melody to a ByteString ('soundToByteString')
and playing it via [ffplay](https://ffmpeg.org/ffplay.html) ('play'').
-}
module Kithara.IO where

import Kithara.Types
import Kithara.Utils
import Kithara.Ops
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as BSLazy
import System.Process (runCommand)
import Text.Printf (printf)
import Data.Foldable (fold)

outFileName :: FilePath
outFileName = "output.bin"

volume :: Volume
volume = 0.2

samples :: Samples
samples = 44100

a4Base :: Hz
a4Base = 440.0

beatsPerMinute :: Beats
beatsPerMinute = 120

quaterDuartion :: Seconds
quaterDuartion = 60.0 / (fromIntegral beatsPerMinute)

a4Quater :: Note
a4Quater = Note { frequency = a4Base, duration = quaterDuartion}

soundToByteString :: Sound -> BSLazy.ByteString
soundToByteString s = BSBuilder.toLazyByteString $ fold builders
    where
        builders = map BSBuilder.floatLE s

saveByteStringSound :: FilePath -> BSLazy.ByteString -> IO ()
saveByteStringSound fp bs = BSLazy.writeFile fp bs

a4QuaterSound :: Sound
a4QuaterSound = sinusoid samples volume a4Quater

a4QuaterSoundSquare :: Sound
a4QuaterSoundSquare = square samples volume a4Quater

a4QuaterSoundTriangle :: Sound
a4QuaterSoundTriangle = triangle samples volume a4Quater

a4QuaterSoundSawSmooth5 :: Sound
a4QuaterSoundSawSmooth5 = sawSmooth samples 5 volume a4Quater

a4QuaterSoundSawSharp :: Sound
a4QuaterSoundSawSharp = sawSharp samples volume a4Quater

a4QuaterByteString :: BSLazy.ByteString
a4QuaterByteString = soundToByteString a4QuaterSound

song :: Sound
song = compose [a4QuaterSound, a4QuaterSoundSquare,
                a4QuaterSoundTriangle, a4QuaterSound,
                a4QuaterSoundSawSmooth5, a4QuaterSoundSawSharp]

songByteString :: BSLazy.ByteString
songByteString = soundToByteString song

save' :: IO ()
save' = saveByteStringSound outFileName songByteString

play' :: IO ()
play' = do
    _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %d %s" samples outFileName
    return ()

run' :: IO ()
run' = do
    save'
    play'
