module Kithara.UtilsSpec (spec) where

import Kithara.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "noteFreq'" $ do
        it "calculates frequencies correctly" $ do
            noteFreq' 440 0     `shouldBe` 440.0
            noteFreq' 440 (-57) `shouldBe` 16.351597
            noteFreq' 440 50    `shouldBe` 7902.132
            noteFreq' 432 50    `shouldBe` 7758.4565
    
    describe "noteFreq" $ do
        it "calculates frequencies of given notes correctly" $ do
            noteFreq 440 4 10 `shouldBe` 440.0
            noteFreq 440 0 1  `shouldBe` 16.351597
            noteFreq 440 8 12 `shouldBe` 7902.132
            noteFreq 432 8 12 `shouldBe` 7758.4565
            noteFreq 440 7 7  `shouldBe` 2959.9553
            noteFreq 446 1 3  `shouldBe` 37.208664
        
        it "fails on octaves that aren't in [0 .. 8]" $ do
            evaluate (noteFreq 440 (-1) 3) `shouldThrow` anyException
            evaluate (noteFreq 440 9    3) `shouldThrow` anyException
        
        it "fails on notes that aren't in [1 .. 12]" $ do
            evaluate (noteFreq 440 5 0)  `shouldThrow` anyException
            evaluate (noteFreq 440 5 13) `shouldThrow` anyException

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
            -- Note: checked d = 0 case above
            \n d -> d == 0 || (fmod n d == fmod (fmod n d) d)
    
    describe "splitAtPositions" $ do
        it "passes edge cases" $ do
            splitAtPositions []        ([] :: [Int]) `shouldBe` ([] :: [[Int]])
            splitAtPositions [1, 2, 3] ([] :: [Int]) `shouldBe` ([] :: [[Int]])
            splitAtPositions []        ['c', 'a']    `shouldBe` [['c', 'a']]
        
        it "splits a list at a given positions" $ do
            splitAtPositions [1, 2, 2]  [0 :: Int, 1, 2, 3, 6]             `shouldBe` [[0], [1, 2], [3, 6]]
            splitAtPositions [1, 2, 10] [0 :: Int, 1, 2, 3, 6]             `shouldBe` [[0], [1, 2], [3, 6]]
            splitAtPositions [1, 2, 2]  ['a', 'b', 'c', 'd', 'e', 'f']     `shouldBe` [['a'], ['b', 'c'], ['d', 'e'], ['f']]
            splitAtPositions [2]        [1.0 :: Float, 2.3, 4.5, 1.2, 0.0] `shouldBe` [[1.0, 2.3], [4.5, 1.2, 0.0]]

    describe "compose" $ do
        it "concatenates 'sounds' in time" $ do
            let makeSound' = map sin
            let s1 = makeSound' [0.2, -1.2, 0.0, 1e-11, 3e+11]
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
            chord [[1, 2, 3], [-1.2, 0.6, 0.0]] `shouldBe` [-0.20000005, 2.6, 3.0]
    
    describe "quarterDuration" $ do
        it "calculates Quarter Note duration in seconds" $ do
            quarterDuration 60  `shouldBe` 1.0
            quarterDuration 120 `shouldBe` 0.5
