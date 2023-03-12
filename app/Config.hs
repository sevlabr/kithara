{-
Description : Default constant parameters.

Default constant parameters that are used to produce a sound
which you can hear by running the executable kithara-exe.
-}
module Config (
    outFileName, outNoisesFileName,
    volume, samples, quarterDur,
    showMode, fileEncoding,
    a4Base, c5, e5, g5
) where

import Kithara.Types
import Kithara.Utils (quarterDuration, noteFreq)

outFileName :: FilePath
outFileName = "output.bin"

outNoisesFileName :: FilePath
outNoisesFileName = "withNoises.bin"

volume :: Volume
volume = 0.2

samples :: Samples
samples = 44100

beatsPerMinute :: Beats
beatsPerMinute = 120

quarterDur :: Seconds
quarterDur = quarterDuration beatsPerMinute

showMode :: Int
showMode = 1

fileEncoding :: String
fileEncoding = "f32le"


-- Actual sound frequences start here
a4Base :: Hz
a4Base = 440.0

c5 :: Hz
c5 = noteFreq a4Base 5 1

e5 :: Hz
e5 = noteFreq a4Base 5 5

g5 :: Hz
g5 = noteFreq a4Base 5 8
