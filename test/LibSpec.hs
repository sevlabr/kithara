module LibSpec (spec) where

import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)

spec :: Spec
spec = do
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
