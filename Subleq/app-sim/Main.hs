module Main (main) where

import Assembler
import CMM.Parser
import CMM.Compiler
import Control.Monad
import Data.Char
import Parser
import System.Environment
import Simulator.Concrete

mainAssembly :: Bool -> Bool -> String -> String -> IO ()
mainAssembly verbose linePrint filename input = case parseAssembly filename input of
  Left err -> putStrLn err
  Right asm -> do
    let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
    (traps,cmp) <- if verbose then debugAssemble (name ++ ".out") asm else assemble asm
    when verbose $ print cmp
    runSimulator verbose linePrint traps cmp

mainCMM :: Bool -> Bool -> String -> String -> IO ()
mainCMM verbose linePrint filename input = case parseCMM filename input of
  Left err -> putStrLn err
  Right prog -> do
    let name = reverse $ takeWhile isAlphaNum $ reverse $ takeWhile (/= '.') filename
    let asm = compile prog
    when verbose $ print asm
    (traps,cmp) <- if verbose then debugAssemble (name ++ ".out") asm else assemble asm
    when verbose $ print cmp
    runSimulator verbose linePrint traps cmp

main :: IO ()
main = do
  (filename, verbose, linePrint) <-
    getArgs >>= \case
      [filename] -> return (filename, False, False)
      [f1, filename] -> return (filename, f1 == "-v", f1 == "-p")
      [_, _, filename] -> return (filename, True, True)
      _illegal -> error "Usage: simulator [-v] [-p] <filename>"
  input <- readFile filename
  if drop (length filename - 4) filename == ".cmm"
    then mainCMM verbose linePrint filename input
    else mainAssembly verbose linePrint filename input
