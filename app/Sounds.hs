{-
Description : Define sounds for the executable.

Sounds that will be produced if running kithara-exe.
The final sound sequence is 'song' and alike.
-}
module Sounds (
    song, noisySong
) where

import Config
import Kithara.Types
import Kithara.Ops
import Kithara.Utils (compose, chord)

-- Notes
a4Quarter :: Note
a4Quarter = Note { frequency = a4Base, duration = quarterDur }

c5Quarter :: Note
c5Quarter = Note { frequency = c5, duration = quarterDur }

e5Quarter :: Note
e5Quarter = Note { frequency = e5, duration = quarterDur }

g5Quarter :: Note
g5Quarter = Note { frequency = g5, duration = quarterDur }

-- ADSRs
basicADSR :: ADSR
basicADSR = ADSR { attack           = 0.08,
                   decay            = 0.02,
                   release          = 0.12,
                   attackAmplitude  = 1.0,
                   sustainAmplitude = 0.8
                 }

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

-- ADSR envelope sounds
a4QuarterADSRSound :: Sound
a4QuarterADSRSound = transformToADSR samples a4QuarterSound basicADSR

c5QuarterADSRSound :: Sound
c5QuarterADSRSound = transformToADSR samples c5QuarterSound basicADSR

e5QuarterADSRSound :: Sound
e5QuarterADSRSound = transformToADSR samples e5QuarterSound basicADSR

g5QuarterADSRSound :: Sound
g5QuarterADSRSound = transformToADSR samples g5QuarterSound basicADSR

c5QuarterMajorTriadADSR :: Sound
c5QuarterMajorTriadADSR = chord [c5QuarterADSRSound, e5QuarterADSRSound, g5QuarterADSRSound]

c5QuarterMajorTriadADSR' :: Sound
c5QuarterMajorTriadADSR' = transformToADSR samples c5QuarterMajorTriad basicADSR

-- Sounds for executable
song :: Sound
song = compose [a4QuarterSound, a4QuarterSoundSquare,
                a4QuarterSoundTriangle, a4QuarterSound,
                a4QuarterSoundSawSmooth5, a4QuarterSoundSawSharp,
                c5QuarterMajorTriad, c5QuarterMajorTriadADSR,
                a4QuarterSound, a4QuarterADSRSound,
                c5QuarterSound, c5QuarterADSRSound,
                e5QuarterSound, e5QuarterADSRSound,
                g5QuarterSound, g5QuarterADSRSound,
                c5QuarterMajorTriadADSR, c5QuarterMajorTriadADSR,
                c5QuarterMajorTriadADSR', c5QuarterMajorTriadADSR']

noisySong :: IO Sound
noisySong = do
    noisyPart <- randNoise
    let fullSong = compose [song, noisyPart]
    return fullSong
