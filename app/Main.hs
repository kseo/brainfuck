module Main where

import Brainfuck

main :: IO ()
main = getContents >>= runBrainfuck . parseBrainfuck
