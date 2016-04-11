module Brainfuck.Parser
  ( parseBrainfuck
  ) where

import Brainfuck.Types
import Data.Maybe (mapMaybe)

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = mapMaybe charToBF
  where
    charToBF x = case x of
                   '>' -> Just GoRight
                   '<' -> Just GoLeft
                   '+' -> Just Increment
                   '-' -> Just Decrement
                   '.' -> Just Print
                   ',' -> Just Read
                   '[' -> Just LoopL
                   ']' -> Just LoopR
                   c -> Nothing

