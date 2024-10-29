module Main (main) where

import Lib
import Assembler

import Text.Megaparsec

main :: IO ()
main = do
  let filename = "test.asm"
  input <- readFile filename
  case parse parseAssembly filename input of
    Left err -> putStrLn $ errorBundlePretty err
    Right asm -> do
      let cmp = assemble asm
      print cmp
      output "test.hex" cmp
