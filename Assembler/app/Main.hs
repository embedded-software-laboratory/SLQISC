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
      cmp <- if verbose then debugAssemble asm else return $ assemble asm
      when verbose $ print cmp
      let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
      output (name ++ ".hex") cmp
