{-# LANGUAGE NumericUnderscores #-}
module Kithara.UtilsBench (benchmarks) where

import Kithara.Utils (splitAtPositions)
import Criterion (Benchmark, bench, nf)

data Container a = Container [Int] [a]

splitAtPositions' :: Container a -> [[a]]
splitAtPositions' (Container indices l) = splitAtPositions indices l

cont :: Container Int
cont = Container ([1, 5 .. 245_000] ++ [300_000, 301_000 .. 990_000]) [0 .. 1_000_000]

cont' :: Container Float
cont' = Container ([1, 10 .. 245_000] ++ [300_000, 305_000 .. 990_000]) [0.0 .. 1_000_000.0]

benchmarks :: [Benchmark]
benchmarks =
    [ bench "1 kk, Int"                     $ nf splitAtPositions' cont
    , bench "1 kk, Float, longer intervals" $ nf splitAtPositions' cont'
    ]
