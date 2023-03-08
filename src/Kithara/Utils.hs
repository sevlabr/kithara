{-|
Description : Helper functions.

Helper functions.
-}
module Kithara.Utils where

import Kithara.Types

-- |Frequently used combination.
sinFT :: Hz -> Seconds -> Float -> Oscillation
sinFT freq t samples' = sin $ freq * t / samples'

-- |Calculates /mod/ on Float values
-- like [fmod](https://en.cppreference.com/w/cpp/numeric/math/fmod) in C++.
fmod :: Float -> Float -> Float
fmod _ 0 = error "Exception: Ratio has zero denominator"
fmod n d = n - dt
    where
        t = truncate $ n / d
        dt = d * fromIntegral t

-- |Combine consecutive sounds into one.
compose :: [Sound] -> Sound
compose sounds = concat sounds
