module Brainfuck.Interpreter
  ( runBrainfuck
  ) where

import Brainfuck.AST
import Brainfuck.Tape

import Control.Monad (void)
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)

runBrainfuck :: AST -> IO ()
runBrainfuck = void . run emptyTape

move :: Int -> Tape Int -> Tape Int
move n tape =
  case n of
    0 -> tape
    n | n > 0 -> iterate moveRight tape !! n
    n | n < 0 -> iterate moveLeft tape !! (negate n)

runLoop :: Tape Int -> AST -> IO (Tape Int)
runLoop tape ops =
  case tape of
    (Tape l 0 r) -> return tape
    otherwise -> do
      newTape <- run tape ops
      runLoop newTape ops

run :: Tape Int -> AST -> IO (Tape Int)
run tape [] = return tape
run tape@(Tape l p r) (op:ops) = do
  case op of
    (Add n) -> run (Tape l (p+n) r) ops
    (Move n) -> run (move n tape) ops
    (Loop loopOps) -> do
      newTape <- runLoop tape loopOps
      run newTape ops
    Input -> do
      p <- getChar
      run tape ops
    Output -> do
      putChar (chr p)
      hFlush stdout
      run tape ops
