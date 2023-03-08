-- module Lib
--     ( someFunc
--     ) where
module Lib where

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Lazy as BSLazy
import System.Process (runCommand)
import Text.Printf (printf)
import Data.Foldable (fold, foldl')

type Volume      = Float
type Hz          = Float
type Seconds     = Float
type Oscillation = Float
type Sound       = [Oscillation]
type Samples     = Int
type Beats       = Int

data Note = Note { frequency :: Hz, duration :: Seconds }
    deriving (Show)

class Oscillator a where
    oscillate :: (Hz -> Seconds -> Float -> Oscillation) -> Volume -> a -> Sound

    sinusoid :: Volume -> a -> Sound
    square :: Volume -> a -> Sound
    triangle :: Volume -> a -> Sound
    sawSmooth :: Int -> Volume -> a -> Sound
    sawSharp :: Volume -> a -> Sound

sinFT :: (Hz -> Seconds -> Float -> Oscillation)
sinFT freq t samples' = sin $ freq * t / samples'

fmod :: (Float -> Float -> Float)
fmod n d = n - dt
    where
        t = truncate $ n / d
        dt = d * fromIntegral t

instance Oscillator Note where
    oscillate f v n = map (\t -> v * f freq t samples') [0.0 .. dur]
        where
            freq     = 2 * pi * (frequency n)
            samples' = fromIntegral samples
            dur      = duration n * samples'

    sinusoid v n = oscillate f v n
        where
            f = sinFT
    
    square v n = oscillate f v n
        where
            -- point-free: ((signum .) .) . sinFT
            f fq t s = signum $ sinFT fq t s

    triangle v n = oscillate f v' n
        where
            f fq t s = asin $ sinFT fq t s
            v' = (2 / pi) * v

    -- the less the k the smoother the sound
    sawSmooth k v n = map (* v') wavesSum
        where
            v' = (2 / pi) * v
            wavesSum = foldl' (zipWith (+)) (replicate dur 0.0) waves
                where
                    dur = round $ duration n * fromIntegral samples
            waves = map genWave [1 .. k]
            genWave i = oscillate f 1.0 n
                where
                    i' = fromIntegral i
                    f fq t s = (sin $ i' * fq * t / s) / i'
    
    sawSharp v n = oscillate f v' n
        where
            v' = (2 / pi) * v
            f fq t s = (pi / 2) - fq' * pi * (fmod (t / s) (1.0 / fq'))
                where
                    fq' = fq / (2 * pi)

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

quaterDuartion :: Seconds
quaterDuartion = 60.0 / (fromIntegral beatsPerMinute)

a4Quater :: Note
a4Quater = Note { frequency = a4Base, duration = quaterDuartion}

soundToByteString :: Sound -> BSLazy.ByteString
soundToByteString s = BSBuilder.toLazyByteString $ fold builders
    where
        builders = map BSBuilder.floatLE s

saveByteStringSound :: FilePath -> BSLazy.ByteString -> IO ()
saveByteStringSound fp bs = BSLazy.writeFile fp bs

compose :: [Sound] -> Sound
compose sounds = concat sounds

a4QuaterSound :: Sound
a4QuaterSound = sinusoid volume a4Quater

a4QuaterSoundSquare :: Sound
a4QuaterSoundSquare = square volume a4Quater

a4QuaterSoundTriangle :: Sound
a4QuaterSoundTriangle = triangle volume a4Quater

a4QuaterSoundSawSmooth5 :: Sound
a4QuaterSoundSawSmooth5 = sawSmooth 5 volume a4Quater

a4QuaterSoundSawSharp :: Sound
a4QuaterSoundSawSharp = sawSharp volume a4Quater

a4QuaterByteString :: BSLazy.ByteString
a4QuaterByteString = soundToByteString a4QuaterSound

song :: Sound
song = compose [a4QuaterSound, a4QuaterSoundSquare,
                a4QuaterSoundTriangle, a4QuaterSound,
                a4QuaterSoundSawSmooth5, a4QuaterSoundSawSharp]

songByteString :: BSLazy.ByteString
songByteString = soundToByteString song

save' :: IO ()
save' = saveByteStringSound outFileName songByteString

play' :: IO ()
play' = do
    _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %d %s" samples outFileName
    return ()

run' :: IO ()
run' = do
    save'
    play'
