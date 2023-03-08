{-
Description : Default constant parameters.

Default constant parameters that are used to produce a sound
which you can hear by running the executable kithara-exe.
-}
module Config where

import Kithara.Types
import Kithara.Utils (quarterDuration)

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

quarterDur :: Seconds
quarterDur = quarterDuration beatsPerMinute

showMode :: Int
showMode = 1

fileEncoding :: String
fileEncoding = "f32le"
