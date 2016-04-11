module Brainfuck.Tape
  ( emptyTape
  , moveLeft
  , moveRight
  , Tape(..)
  ) where

data Tape a = Tape [a]  -- Left of the pivot element
              a         -- Pivot element
              [a]       -- Right of the pivot element

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

