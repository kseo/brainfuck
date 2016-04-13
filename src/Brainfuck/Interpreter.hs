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

runLoop :: Tape -> AST -> IO Tape
runLoop tape ops =
  if current tape == 0
     then return tape
     else do
      newTape <- run tape ops
      runLoop newTape ops

run :: Tape -> AST -> IO Tape
run tape [] = return tape
run tape (op:ops) = do
  case op of
    (Add n) -> run (modify (+n) tape) ops
    (Move n) -> run (move n tape) ops
    (Loop loopOps) -> do
      newTape <- runLoop tape loopOps
      run newTape ops
    Input -> do
      p <- getChar
      run tape ops
    Output -> do
      putChar $ chr $ current tape
      hFlush stdout
      run tape ops
