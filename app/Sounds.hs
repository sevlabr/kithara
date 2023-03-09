{-
Description : Define sounds for the executable.

Sounds that will be produced if running kithara-exe.
The final sound sequence is 'song' and alike.
-}
module Sounds where

import Config
import Kithara.Types
import Kithara.Ops
import Kithara.Utils (compose, chord)
import GHC.Base (returnIO)

-- Notes
a4Quarter :: Note
a4Quarter = Note { frequency = a4Base, duration = quarterDur }

c5Quarter :: Note
c5Quarter = Note { frequency = c5, duration = quarterDur }

e5Quarter :: Note
e5Quarter = Note { frequency = e5, duration = quarterDur }

g5Quarter :: Note
g5Quarter = Note { frequency = g5, duration = quarterDur }

-- Different wave shapes
a4QuarterSound :: Sound
a4QuarterSound = sinusoid samples volume a4Quarter

a4QuarterSoundSquare :: Sound
a4QuarterSoundSquare = square samples volume a4Quarter

a4QuarterSoundTriangle :: Sound
a4QuarterSoundTriangle = triangle samples volume a4Quarter

a4QuarterSoundSawSmooth5 :: Sound
a4QuarterSoundSawSmooth5 = sawSmooth samples 5 volume a4Quarter

a4QuarterSoundSawSharp :: Sound
a4QuarterSoundSawSharp = sawSharp samples volume a4Quarter

randNoise :: IO Sound
randNoise = noise samples volume a4Quarter

-- Sounds
c5QuarterSound :: Sound
c5QuarterSound = sinusoid samples volume c5Quarter

e5QuarterSound :: Sound
e5QuarterSound = sinusoid samples volume e5Quarter

g5QuarterSound :: Sound
g5QuarterSound = sinusoid samples volume g5Quarter

-- Chords
c5QuarterMajorTriad :: Sound
c5QuarterMajorTriad = chord [c5QuarterSound, e5QuarterSound, g5QuarterSound]

-- Sounds for executable
song :: Sound
song = compose [a4QuarterSound, a4QuarterSoundSquare,
                a4QuarterSoundTriangle, a4QuarterSound,
                a4QuarterSoundSawSmooth5, a4QuarterSoundSawSharp,
                c5QuarterMajorTriad, c5QuarterMajorTriad]

noisySong :: IO Sound
noisySong = do
    noisyPart <- randNoise
    let fullSong = compose [song, noisyPart]
    return fullSong
