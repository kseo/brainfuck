{-# LANGUAGE FlexibleContexts #-}

module Brainfuck.Parser
  ( parseBrainfuck
  ) where

import Brainfuck.AST
import Control.Applicative hiding (many, (<|>)) -- conflicts with Parsec
import Data.Maybe (mapMaybe)
import Text.Parsec ((<|>), token, Parsec, ParseError, parse, many, many1)
import Text.Parsec.Pos (newPos)
import Text.Parsec.Combinator (between)

data BrainfuckCommand = GoRight
  | GoLeft
  | Increment
  | Decrement
  | Print
  | Read
  | LoopL
  | LoopR
  deriving (Eq, Show)

type Parser = Parsec [(Int, BrainfuckCommand)] ()

bfToken :: BrainfuckCommand -> Parser BrainfuckCommand
bfToken x = token showTok posFromTok testTok
  where
    showTok (pos,t)     = show t
    posFromTok (pos,t)  = newPos "" 0 pos
    testTok (pos,t)     = if x == t then Just t else Nothing

program :: Parser AST
program = many instruction

instruction :: Parser Instruction
instruction = loop <|> operator

loop :: Parser Instruction
loop = Loop <$> between (bfToken LoopL) (bfToken LoopR) program

operator :: Parser Instruction
operator = Add . length <$> many1 (bfToken Increment)
       <|> Add . negate . length <$> many1 (bfToken Decrement)
       <|> Move . length <$> many1 (bfToken GoRight)
       <|> Move . negate . length <$> many1 (bfToken GoLeft)
       <|> (bfToken Read >> pure Input)
       <|> (bfToken Print >> pure Output)

parseBrainfuck :: String -> Either ParseError AST
parseBrainfuck s =
  let commands = mapMaybe charToBF s
   in parse program "" $ zip [0..] commands
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

