module Main where

import Lib

main :: IO ()
main = putTextLn ("Hello from turn-engine! 1 + 1 = " <> show (add 1 1))
