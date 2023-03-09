module Main (main) where

import Kithara.IO
import Sounds (song, noisySong)
import Config

main :: IO ()
main = do
    saveSong song outFileName
    playSong outFileName samples showMode fileEncoding

    -- Example for impure sound with noises
    -- saveSongWithNoises noisySong outNoisesFileName
    -- playSong outNoisesFileName samples showMode fileEncoding
