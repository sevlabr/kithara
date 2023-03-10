module Kithara.OpsSpec (spec) where

import Kithara.Ops
import Kithara.Types
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "transformToADSR" $ do
        it "changes an amplitude of a given sound to ADSR pattern" $ do
            let sound = (replicate 15 1.0) :: Sound
            let samples = 10
            let bADSR = ADSR { attack           = 0.2,
                               decay            = 0.2,
                               release          = 0.2,
                               attackAmplitude  = 1.0,
                               sustainAmplitude = 0.5
                             }
            let result = [0.0, 0.5, 1.0, 0.75] ++ replicate 9 0.5 ++ [0.25, 0.0]
            let realResult = transformToADSR samples sound bADSR
            length realResult `shouldBe` length result
            realResult        `shouldBe` result

            let bADSR' = ADSR { attack           = 0.5,
                                decay            = 0.4,
                                release          = 0.4,
                                attackAmplitude  = 1.0,
                                sustainAmplitude = 0.5
                              }
            let result' = [0.0, 0.2 .. 1.0] ++ [0.875, 0.75 .. 0.5] ++ [0.5, 0.375 .. 0.0]
            let realResult' = transformToADSR samples sound bADSR'
            length realResult' `shouldBe` length result'
            realResult'        `shouldBe` result'

            let sound' = (replicate 153123 1.0) :: Sound
            let samples' = 48923
            let realResult'' = transformToADSR samples' sound' bADSR'
            length realResult'' `shouldBe` length sound'
        
        it "throws an exception when ADSR is too long for the sound" $ do
            let sound = (replicate 17 0.42) :: Sound
            let samples = 1400
            let badADSR = ADSR { attack           = 0.19,
                                 decay            = 0.7,
                                 release          = 0.8,
                                 attackAmplitude  = 1.2,
                                 sustainAmplitude = 0.15
                               }
            let result = transformToADSR samples sound badADSR
            evaluate (result) `shouldThrow` anyException
