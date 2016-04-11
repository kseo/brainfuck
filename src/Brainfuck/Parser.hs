module Brainfuck.Parser
  ( parseBrainfuck
  ) where

import Brainfuck.Types

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = fmap charToBF
  where
    charToBF x = case x of
                   '>' -> GoRight
                   '<' -> GoLeft
                   '+' -> Increment
                   '-' -> Decrement
                   '.' -> Print
                   ',' -> Read
                   '[' -> LoopL
                   ']' -> LoopR
                   c -> Comment c

