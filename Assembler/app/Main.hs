module Main (main) where

import Assembler
import Parser

import Control.Monad
import Data.Char
import System.Environment

main :: IO ()
main = do
  (filename,verbose) <- getArgs >>= \case
    [filename] -> return (filename,False)
    [_, filename] -> return (filename,True)
    _ -> error "Usage: assembler [-v] <filename>"
  input <- readFile filename
  case parseAssembly filename input of
    Left err -> putStrLn err
    Right asm -> do
      let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
      cmp <- if verbose then debugAssemble (name ++ ".out") asm else return $ assemble asm
      when verbose $ print cmp
      output (name ++ ".hex") cmp
