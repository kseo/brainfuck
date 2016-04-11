module Brainfuck.Types
  ( BrainfuckCommand(..)
  , BrainfuckSource
  ) where

data BrainfuckCommand = GoRight
  | GoLeft
  | Increment
  | Decrement
  | Print
  | Read
  | LoopL
  | LoopR

type BrainfuckSource = [BrainfuckCommand]

