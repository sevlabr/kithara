module Kithara.UtilsSpec (spec) where

import Kithara.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "sinFT" $ do
        it "calculates sin(freq * t / samples)" $ do
            sinFT 0.2 0.11 100.0 `shouldBe` 2.2e-4
    
    describe "fmod" $ do
        it "calculates `mod` on Float values like fmod in C++" $ do
            -- TODO: make an utility function for reading these values from a file
            fmod 1.9    0.8    `shouldBe` 0.29999995
            fmod (-1.9) 0.8    `shouldBe` (-0.29999995)
            fmod 1.9    (-0.8) `shouldBe` 0.29999995
            fmod (-1.9) (-0.8) `shouldBe` (-0.29999995)

            fmod 1.9    0.5    `shouldBe` 0.39999998
            fmod (-1.9) 0.5    `shouldBe` (-0.39999998)
            fmod 1.9    (-0.5) `shouldBe` 0.39999998
            fmod (-1.9) (-0.5) `shouldBe` (-0.39999998)

        it "fails on 0 denominator" $ do
            evaluate (fmod 1.9 0.0) `shouldThrow` anyException

        prop "idempotency" $
            \n d -> if d /= 0 then
                       fmod n d == fmod (fmod n d) d
                    else
                        True -- checked d = 0 case above
    
    describe "splitAtPositions" $ do
        it "passes edge cases" $ do
            splitAtPositions []        ([] :: [Int]) `shouldBe` ([] :: [[Int]])
            splitAtPositions [1, 2, 3] ([] :: [Int]) `shouldBe` ([] :: [[Int]])
            splitAtPositions []        ['c', 'a']    `shouldBe` [['c', 'a']]
        
        it "splits a list at a given positions" $ do
            splitAtPositions [1, 2, 2]  [0, 1, 2, 3, 6]                `shouldBe` [[0], [1, 2], [3, 6]]
            splitAtPositions [1, 2, 10] [0, 1, 2, 3, 6]                `shouldBe` [[0], [1, 2], [3, 6]]
            splitAtPositions [1, 2, 2]  ['a', 'b', 'c', 'd', 'e', 'f'] `shouldBe` [['a'], ['b', 'c'], ['d', 'e'], ['f']]
            splitAtPositions [2]        [1.0, 2.3, 4.5, 1.2, 0.0]      `shouldBe` [[1.0, 2.3], [4.5, 1.2, 0.0]]

    describe "compose" $ do
        it "concatenates 'sounds' in time" $ do
            let makeSound' lst = map sin lst
            let s1 = makeSound' [0.2, (-1.2), 0.0, 1e-11, 3e+11]
            let s2 = makeSound' $ replicate (length s1) 0.0
            let s3 = makeSound' [1, 2]
            let s4 = makeSound' [5]
            let s5 = makeSound' []
            let sounds' = [s1, s2, s3, s4, s5]
            compose sounds' `shouldBe` concat sounds'

    describe "chord" $ do
        it "fails on empty 'sounds'" $ do
            evaluate (chord []) `shouldThrow` anyException
        
        it "fails on 'sounds' that have different duration" $ do
            evaluate (chord [[1.0, 2.1], [0.1, 6.5, 3.1]]) `shouldThrow` anyException
        
        it "fails on 'sounds' with zero duration" $ do
            evaluate (chord [[], []]) `shouldThrow` anyException

        it "sums 'sounds' for the same time interval" $ do
            chord [[1, 2, 3], [(-1.2), 0.6, 0.0]] `shouldBe` [(-0.20000005), 2.6, 3.0]
    
    describe "quarterDuration" $ do
        it "calculates Quarter Note duration in seconds" $ do
            quarterDuration 60  `shouldBe` 1.0
            quarterDuration 120 `shouldBe` 0.5
