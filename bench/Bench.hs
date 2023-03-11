module Main (main) where

import qualified Kithara.OpsBench as KOB
import qualified Kithara.UtilsBench as KUB
import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "transformToADSR"  KOB.benchmarks
    , bgroup "splitAtPositions" KUB.benchmarks
    ]
