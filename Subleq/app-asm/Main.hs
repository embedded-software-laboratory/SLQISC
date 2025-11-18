module Main (main) where

import Assembler
import CMM.Parser
import CMM.Compiler
import Control.Monad
import Data.Char
import Parser
import System.Environment

mainAssembly :: Bool -> String -> String -> IO ()
mainAssembly verbose filename input = case parseAssembly filename input of
  Left err -> putStrLn err
  Right asm -> do
    let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
    (_,cmp) <- if verbose then debugAssemble (name ++ ".out") asm else return $ assemble asm
    when verbose $ print cmp
    outputLogisim (name ++ ".hex") cmp
    outputMif (name ++ ".mif") cmp

mainCMM :: Bool -> String -> String -> IO ()
mainCMM verbose filename input = case parseCMM filename input of
  Left err -> putStrLn err
  Right prog -> do
    let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
    let asm = compile prog
    when verbose $ print asm
    (_,cmp) <- if verbose then debugAssemble (name ++ ".out") asm else return $ assemble asm
    when verbose $ print cmp
    outputLogisim (name ++ ".hex") cmp
    outputMif (name ++ ".mif") cmp

main :: IO ()
main = do
  (filename, verbose) <-
    getArgs >>= \case
      [filename] -> return (filename, False)
      [_, filename] -> return (filename, True)
      _ -> error "Usage: assembler [-v] <filename>"
  input <- readFile filename
  if drop (length filename - 4) filename == ".cmm"
    then mainCMM verbose filename input
    else mainAssembly verbose filename input
