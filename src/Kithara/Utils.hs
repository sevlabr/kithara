{-|
Description : Helper functions.

Helper functions.
-}
module Kithara.Utils where

import Kithara.Types
import Data.Foldable (foldl')
import System.Random.Stateful (globalStdGen, uniformRM)

-- |Frequently used combination.
sinFT :: Hz -> Seconds -> Float -> Oscillation
sinFT freq t samples' = sin $ freq * t / samples'

-- |Calculates /mod/ on Float values
-- like [fmod](https://en.cppreference.com/w/cpp/numeric/math/fmod) in C++.
fmod :: Float -> Float -> Float
fmod _ 0 = error "Exception: fmod: Ratio has zero denominator!"
fmod n d = n - dt
    where
        t = truncate $ n / d
        dt = d * fromIntegral t

-- |Combine consecutive sounds into one,
-- i.e. each sound plays alone at a given point in time.
compose :: [Sound] -> Sound
compose sounds = concat sounds

-- |Combine sounds to make a chord,
-- i.e. all sounds are playing together at a given point in time.
-- __Note:__ it is assumed that all provided sounds have the same duration.
chord :: [Sound] -> Sound
chord [] = error "Exception: chord: Can't make chord from empty sound list!"
chord sounds | checkLen sounds = foldl' (zipWith (+)) (replicate len 0.0) sounds
             | otherwise       = error msg
    where
        msg = "Exception: chord: Sounds in chord must have same non-zero duration!"
        len = length $ head sounds
        checkLen ss = head soundDurs /= 0 && all (== head soundDurs) soundDurs
            where soundDurs = map length ss

-- |Get Quarter Note duration in seconds.
quarterDuration :: Beats -> Seconds
quarterDuration bpm = 60.0 / (fromIntegral bpm)

-- |Generates random number in Float range @[l .. r]@.
genRandNoise :: Float -> Float -> IO Float
genRandNoise l r = uniformRM (l, r) globalStdGen
