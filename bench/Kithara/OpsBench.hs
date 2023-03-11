{-# LANGUAGE NumericUnderscores #-}
module Kithara.OpsBench (benchmarks) where

import Kithara.Ops
import Kithara.Types
import Criterion (Benchmark, bench, nf)

data ADSRContainer = ADSRContainer Samples Sound ADSR

transformToADSR' :: ADSRContainer -> Sound
transformToADSR' (ADSRContainer samp s a) = transformToADSR samp s a

sound :: Sound
sound = map (\x -> sin $ x * 1e-3) [1.0 .. 1e+6]

samples :: Samples
samples = 50_000

adsr :: ADSR
adsr = ADSR { attack           = 5.0,
              decay            = 3.0,
              release          = 7.0,
              attackAmplitude  = 1.2,
              sustainAmplitude = 0.75
            }

benchmarks :: [Benchmark]
benchmarks =
    [ bench "1 kk, 20 sec" $ nf transformToADSR' (ADSRContainer samples sound adsr)
    ]
