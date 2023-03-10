{-|
Description : Basic types to represent sounds.

Basic types to represent sounds.
-}
module Kithara.Types where

-- |Amplitude of an underlying soundwave.
type Volume      = Float
-- |Frequency of an underlying soundwave.
type Hz          = Float
-- |Duration of a sound.
type Seconds     = Float
-- |Value of a soundwave at a certain point in time.
type Oscillation = Float
-- |Discretized representation of a soundwave.
type Sound       = [Oscillation]
-- |How many discretization points are used to
-- represent one second of a sound.
type Samples     = Int
-- |How many /beats/ there are in a minute.
-- This is a human-level definition.
-- A Quarter Note defaults to one beat. For example,
-- if a music writer sets 120 /beats per minute/,
-- then Quarter Note is \(\frac{60}{120} = 0.5\) seconds.
type Beats       = Int

-- |Note is a combination of frequency and duration.
data Note = Note { frequency :: Hz, duration :: Seconds }
    deriving (Show, Eq)

-- |Contains instrument-specific information about
-- [ADSR](https://en.wikipedia.org/wiki/Envelope_\(music\)\#ADSR)
-- envelope. This is used to modify an amplitude of 
-- a raw soundwave to make it sound more natural.
data ADSR = ADSR { attack           :: Seconds,
                   decay            :: Seconds,
                   release          :: Seconds,
                   attackAmplitude  :: Volume,
                   sustainAmplitude :: Volume
                 } deriving (Show, Eq)
