module Main (main) where

import Kithara.IO
import Sounds (song)
import Config

main :: IO ()
main = do
    saveSong song outFileName
    playSong outFileName samples showMode fileEncoding
