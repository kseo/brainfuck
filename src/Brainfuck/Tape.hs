module Brainfuck.Tape
  ( emptyTape
  , current
  , modify
  , move
  , Tape
  ) where

import Data.List.Zipper (Zipper(..))
import qualified Data.List.Zipper as Z

type Tape = Zipper Int

emptyTape :: Tape
emptyTape = Zip zeros zeros
  where
    zeros = repeat 0

moveRight :: Tape -> Tape
moveRight = Z.right

moveLeft :: Tape -> Tape
moveLeft = Z.left

move :: Int -> Tape -> Tape
move n tape =
  case n of
    0 -> tape
    n | n > 0 -> iterate moveRight tape !! n
    n | n < 0 -> iterate moveLeft tape !! (negate n)

current :: Tape -> Int
current = Z.cursor

modify :: (Int -> Int) -> Tape -> Tape
modify f tape = Z.replace (f $ current tape) tape

