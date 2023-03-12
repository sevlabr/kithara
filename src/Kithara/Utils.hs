{-|
Description : Helper functions.

Helper functions.
-}
module Kithara.Utils (
    noteFreq', noteFreq,
    sinFT, fmod,
    genRandNoise,
    splitAtPositions,
    compose, chord,
    quarterDuration,
    readADSR
) where

import Kithara.Types
import Data.Foldable (foldl')
import System.Random.Stateful (globalStdGen, uniformRM)

-- |Calculate frequency of a note that is half steps away from the base frequency.
-- Equal tempered scale is assumed.
-- For more info check [this](https://pages.mtu.edu/~suits/NoteFreqCalcs.html).
noteFreq' :: Hz  -- ^ Base frequency (e.g. A4, which is commonly 440 Hz).
          -> Int -- ^ The number of half steps away from the base frequency.
          -> Hz
noteFreq' f0 n = f0 * 2 ** (fromIntegral n / 12)

-- |Given an index of an octave and an index of the note
-- produce a frequency of that note. Scientific notation is used,
-- check [this](https://pages.mtu.edu/~suits/notefreqs.html)
-- for reference.
-- 
-- For example, for \(C_{4}\) note ("Middle C") an octave index
-- is 4 and note index is 1 (i.e. notes start from /C/ with index 1
-- and continue up to /B/ with index 12).
-- 
-- Only indices from 0 to 8 are allowed for octaves and from 1 to 12
-- for notes. Error is thrown in case of other values.
-- 
-- Examples (if \(A_{4}\) is 440 Hz):
-- 
-- \(C_{4}\)
-- 
-- >>> noteFreq 440 4 1
-- 261.62555
-- 
-- \(B_{4}\)
-- 
-- >>> noteFreq 440 4 12
-- 493.8833
-- 
-- \(A_{6}\)
-- 
-- >>> noteFreq 440 6 10
-- 1760.0
noteFreq :: Hz  -- ^ Base frequency (e.g. A4, which is commonly 440 Hz).
         -> Int -- ^ Octave index.
         -> Int -- ^ Note index.
         -> Hz
noteFreq f0 oct n | oct < 0 || oct > 8  = error octMsg
                  | n   < 1 || n   > 12 = error noteMsg
                  | otherwise = noteFreq' f0 (12 * (oct - 5) + n + 2)
    where
        octMsg  = "Exception: noteFreq: Octave index must be in range [0 .. 8]!"
        noteMsg = "Exception: noteFreq: Note index must be in range [1 .. 12]!"

-- |Frequently used combination.
sinFT :: Hz -> Seconds -> Float -> Oscillation
sinFT freq t samples' = sin $ freq * t / samples'

-- |Calculates /mod/ on Float values
-- like [fmod](https://en.cppreference.com/w/cpp/numeric/math/fmod) in C++.
fmod :: Float -> Float -> Float
fmod _ 0 = error "Exception: fmod: Ratio has zero denominator!"
fmod n d = n - dt
    where
        t = (truncate $ n / d) :: Integer
        dt = d * fromIntegral t

-- |Generates random number in Float range @[l .. r]@.
-- 'globalStdGen' is used for generation.
genRandNoise :: Float -> Float -> IO Float
genRandNoise l r = uniformRM (l, r) globalStdGen

-- |Split a given list at a given positions.
-- Each position is relative, meaning that if
-- positions are @[1,2]@ the list will be splitted
-- after the 1st element and the 3rd, not the 2nd one.
-- 
-- Examples:
--
-- >>> splitAtPositions [1,2] [1,2,3,4,5]
-- [[1],[2,3],[4,5]]
-- 
-- >>> splitAtPositions [1,3,1] [1,2,3,4,5]
-- [[1],[2,3,4],[5]]
-- 
-- >>> splitAtPositions [1,3,100] [1,2,3,4,5]
-- [[1],[2,3,4],[5]]
-- 
-- >>> splitAtPositions [3] [1,2,3,4,5]
-- [[1,2,3],[4,5]]
splitAtPositions :: [Int] -> [a] -> [[a]]
splitAtPositions _      [] = []
splitAtPositions []     l  = [l]
splitAtPositions (p:ps) l  =
    let
        (h, t) = splitAt p l
        -- ps' = map (\p' -> p' - length h) ps
    in
        h : splitAtPositions ps t

-- |Combine consecutive sounds into one,
-- i.e. each sound plays alone at a given point in time.
compose :: [Sound] -> Sound
compose = concat

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
quarterDuration bpm = 60.0 / fromIntegral bpm

-- |Unpack 'ADSR' characteristics.
readADSR :: ADSR
         -> ( Seconds, Seconds, Seconds
            , Volume, Volume
            )
readADSR adsr = (at, dt, rt, aAmpl, sAmpl)
    where
        (at, dt, rt)   = (attack adsr, decay adsr, release adsr)
        (aAmpl, sAmpl) = (attackAmplitude adsr, sustainAmplitude adsr)
