module Main (main) where

import Lib
import Assembler

import Text.Megaparsec
import System.Environment

main :: IO ()
main = do
  filename <- getArgs >>= \case
    [filename] -> return filename
    _ -> error "Usage: assembler <filename>"
  input <- readFile filename
  case parse parseAssembly filename input of
    Left err -> putStrLn $ errorBundlePretty err
    Right asm -> do
      let cmp = assemble asm
      print cmp
      output "out.hex" cmp
