{-
Description : Define sounds for the executable.

Sounds that will be produced if running kithara-exe.
The final sound sequence is 'song'.
-}
module Sounds where

import Config
import Kithara.Types
import Kithara.Ops
import Kithara.Utils (compose)

a4Quarter :: Note
a4Quarter = Note { frequency = a4Base, duration = quarterDur}

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

song :: Sound
song = compose [a4QuarterSound, a4QuarterSoundSquare,
                a4QuarterSoundTriangle, a4QuarterSound,
                a4QuarterSoundSawSmooth5, a4QuarterSoundSawSharp]
