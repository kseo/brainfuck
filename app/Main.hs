module Main where

import Brainfuck
import Options.Applicative

data Option = Option
  { filename :: String
  }

main :: IO ()
main = do
  option <- execParser opts
  contents <- readFile (filename option)
  case parseBrainfuck contents of
    Left e -> print e
    Right ast -> runBrainfuck ast
  where
    bfOption :: Parser Option
    bfOption = Option <$> argument str (metavar "FILE")

    opts :: ParserInfo Option
    opts = info (helper <*> bfOption)
      ( fullDesc
      <> progDesc "Brainfuck interpreter"
      <> header "Brainfuck" )

