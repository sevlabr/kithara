{-|
Description : Operations to produce sounds.

This module contains basic primitives to create sounds.
For example, 'Oscillator' is used to define a certain shapes
of soundwaves.
-}
module Kithara.Ops where

import Kithara.Types
import Kithara.Utils
import Data.Foldable (foldl')
import Control.Monad (replicateM)

-- |Waves can come in a form of sinusoids but not only.
class Oscillator a where
    -- |Basic primitive that helps to define common waveshapes by taking
    -- a function which encodes the waveshape as the first argument.
    oscillate :: (Hz -> Seconds -> Float -> Oscillation)
              -> Samples -> Volume -> a -> Sound

    -- |Simple \(sin\) function.
    sinusoid :: Samples -> Volume -> a -> Sound
    -- |[Square wave](https://en.wikipedia.org/wiki/Square_wave).
    square :: Samples -> Volume -> a -> Sound
    -- |[Triangle wave](https://en.wikipedia.org/wiki/Triangle_wave).
    triangle :: Samples -> Volume -> a -> Sound
    -- |[Sawtooth wave](https://en.wikipedia.org/wiki/Sawtooth_wave).
    sawSmooth :: Samples
              -> Int                  -- ^ Number of sine waves to sum
                                      -- to produce a saw-shaped wave.
              -> Volume -> a -> Sound
    -- |Like a sawtooth but easier and faster to compute. Gives a triangular
    -- pattern with sharp corners hence the name.
    sawSharp :: Samples -> Volume -> a -> Sound
    -- |Random noise. Only duration is taken from the given note,
    -- frequency is omitted.
    noise :: Samples -> Volume -> a -> IO Sound

instance Oscillator Note where
    oscillate f s v n = map (\t -> v * f freq t samples') [0.0 .. dur]
        where
            freq     = 2 * pi * (frequency n)
            samples' = fromIntegral s
            dur      = duration n * samples'

    sinusoid s v n = oscillate f s v n
        where
            f = sinFT
    
    square s v n = oscillate f s v n
        where
            -- point-free: ((signum .) .) . sinFT
            f fq t s' = signum $ sinFT fq t s'

    triangle s v n = oscillate f s v' n
        where
            f fq t s' = asin $ sinFT fq t s'
            v' = (2 / pi) * v

    -- the less the k the smoother the sound
    sawSmooth s k v n = map (* v') wavesSum
        where
            v' = (2 / pi) * v
            wavesSum = foldl' (zipWith (+)) (replicate dur 0.0) waves
                where
                    dur = round $ duration n * fromIntegral s
            waves = map genWave [1 .. k]
            genWave i = oscillate f s 1.0 n
                where
                    i' = fromIntegral i
                    f fq t s' = (sin $ i' * fq * t / s') / i'
    
    sawSharp s v n = oscillate f s v' n
        where
            v' = (2 / pi) * v
            f fq t s' = (pi / 2) - fq' * pi * (fmod (t / s') (1.0 / fq'))
                where
                    fq' = fq / (2 * pi)
    
    noise s v n = replicateM len (genRandNoise (-v) v)
        where
            len = 1 + (round $ duration n * fromIntegral s)
