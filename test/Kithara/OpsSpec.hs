module Kithara.OpsSpec (spec) where

import Kithara.Ops
import Kithara.Types
import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "transformToADSR" $ do
        it "changes an amplitude of a given sound to ADSR pattern" $ do
            let sound = replicate 15 1.0 :: Sound
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

            let sound' = replicate 153123 1.0 :: Sound
            let samples' = 48923
            let realResult'' = transformToADSR samples' sound' bADSR'
            length realResult'' `shouldBe` length sound'
        
        it "throws an exception when ADSR is too long for the sound" $ do
            let sound = replicate 17 0.42 :: Sound
            let samples = 1400
            let badADSR = ADSR { attack           = 0.19,
                                 decay            = 0.7,
                                 release          = 0.8,
                                 attackAmplitude  = 1.2,
                                 sustainAmplitude = 0.15
                               }
            let result = transformToADSR samples sound badADSR
            evaluate result `shouldThrow` anyException
        
        it "changes an amplitude of a given sound to AR pattern" $ do
            let sound = replicate 15 1.0 :: Sound
            let samples = 10
            let ar = ADSR { attack           = 0.745,
                            decay            = 0.01,
                            release          = 0.745,
                            attackAmplitude  = 1.0,
                            sustainAmplitude = 1.0
                          }
            let result = [ 0.0,        0.13422818, 0.26845637, 0.40268457
                         , 0.53691274, 0.6711409,  0.80536914, 1.0
                         , 0.86577183, 0.73154366, 0.59731543, 0.46308726
                         , 0.3288591,  0.19463086, 6.040269e-2
                         ]
            let realResult = transformToADSR samples sound ar
            realResult `shouldBe` result

            let sound' = replicate 44100 1.0 :: Sound
            let samples' = 44100
            let ar' = ADSR { attack           = 0.5,
                             decay            = 0.0,
                             release          = 0.5,
                             attackAmplitude  = 1.0,
                             sustainAmplitude = 1.0
                           }
            let realResult' = transformToADSR samples' sound' ar'
            realResult'!!22051 `shouldNotBe` 1.0
            realResult'!!22049 `shouldNotBe` 1.0
            realResult'!!22050 `shouldBe`    1.0

            let ar'' = ar' { attack = 0.1, release = 0.9 }
            let realResult'' = transformToADSR samples' sound' ar''
            realResult''!!4411 `shouldNotBe` 1.0
            realResult''!!4409 `shouldNotBe` 1.0
            realResult''!!4410 `shouldBe`    1.0
        
        it "changes an amplitude of a given sound to ADR pattern" $ do
            let sound = replicate 24 1.0 :: Sound
            let samples = 8
            let adr = ADSR { attack           = 1.0,
                             decay            = 1.0,
                             release          = 1.0,
                             attackAmplitude  = 1.0,
                             sustainAmplitude = 0.5
                           }
            let result =     [0.0,    0.125 .. 1.0]
                          ++ [0.9375, 0.875 .. 0.5]
                          ++ [0.4375, 0.375 .. 6.25e-2]
            let realResult = transformToADSR samples sound adr
            realResult `shouldBe` result

            let adr' = adr { attack = 2.0, decay = 0.5, release = 0.5,
                             sustainAmplitude = 2.0
                           }
            let result' =    [0.0, 6.25e-2 .. 1.0]
                          ++ [1.25, 1.5 .. 2.0]
                          ++ [1.5, 1.0, 0.5]
            let realResult' = transformToADSR samples sound adr'
            realResult' `shouldBe` result'
