module Brainfuck.AST
  (  AST
  , Instruction(..)
  ) where

type AST = [Instruction]

data Instruction = Add Int
                 | Move Int
                 | Loop [Instruction]
                 | Input
                 | Output
                 deriving (Show, Eq)

