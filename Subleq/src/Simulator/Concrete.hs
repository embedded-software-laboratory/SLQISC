module Simulator.Concrete (runSimulator) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char
import Data.Int (Int16)
import Data.Map qualified as M
import Assembler (disassemble)

data ProgramState = ProgramState
  { memory :: !(M.Map Int16 Int16),
    pc :: !Int16
  }

charize :: Int16 -> Char
charize 0 = ' '
charize 27 = '!'
charize x | x < 0 = '-'
charize x | x < 27 = chr (fromIntegral (x-1) + ord 'A')
charize x | x < 38 = chr (fromIntegral (x-28) + ord '0')
charize _ = '@'

instance Show ProgramState where
  show ps = show (pc ps) ++ show (memory ps)

step :: (Monad m) => m Int16 -> (Int16 -> m ()) -> ProgramState -> m ProgramState
step input output state = do
  let locA = M.findWithDefault 0 (pc state) $ memory state
  let locB = M.findWithDefault 0 (pc state + 1) $ memory state
  let targ = M.findWithDefault 0 (pc state + 2) $ memory state
  let valA = M.findWithDefault 0 locA $ memory state
  let valB = M.findWithDefault 0 locB $ memory state
  let res = valB - valA
  state' <-
    if locA == -2 && valA > 0
      then do
        i <- input
        return $ state {memory = M.insert locA (-i) (memory state)}
      else return state
  let mem = M.insert locB res $ memory state'
  when (locB == -1) $ output valA
  return $ ProgramState {memory = mem, pc = if res <= 0 then targ else pc state' + 3}

stepDeb :: (MonadIO m) => m Int16 -> (Int16 -> m ()) -> ProgramState -> m ProgramState
stepDeb input _ state = do
  let locA = M.findWithDefault 0 (pc state) $ memory state
  let locB = M.findWithDefault 0 (pc state + 1) $ memory state
  let targ = M.findWithDefault 0 (pc state + 2) $ memory state
  let valA = M.findWithDefault 0 locA $ memory state
  let valB = M.findWithDefault 0 locB $ memory state
  let res = valB - valA
  state' <-
    if locA == -2 && valA > 0
      then do
        i <- input
        return $ state {memory = M.insert locA (-i) (memory state)}
      else return state
  let mem = M.insert locB res $ memory state'
  liftIO $ putStrLn $ "Executing SLQ " ++ show locA ++ " " ++ show locB ++ " " ++ show targ
  liftIO $ putStrLn $ "  [" ++ show locA ++ "] = " ++ show valA ++ "  [" ++ show locB ++ "] = " ++ show valB
  liftIO $ putStrLn $ "  storing " ++ show res ++ " in [" ++ show locB ++ "]"
  when (locB == -1) $ liftIO $ putStrLn $ "  outputting " ++ show valA
  return $ ProgramState {memory = mem, pc = if res <= 0 then targ else pc state' + 3}

initialState :: [Int] -> ProgramState
initialState prog = ProgramState {memory = M.fromList $ filter ((/= 0) . snd) $ zipWith (\i v -> (fromIntegral i, fromIntegral v)) [0 .. 65535 :: Int] prog, pc = 0}

debugBreak :: [Int16] -> ProgramState -> IO Bool
debugBreak traps ps = do
  putStrLn "BREAK"
  ln <- getLine
  case ln of
    "state" -> print (disassemble (pc ps) (memory ps)) >> debugBreak traps ps
    "continue" -> return True
    "exit" -> return False
    errCom -> putStrLn ("unknown command " ++ errCom) >> debugBreak traps ps

simLoopDeb :: [Int16] -> ProgramState -> IO ()
simLoopDeb traps ps = do
  ps' <-
    stepDeb
      readLn
      print
      ps
  if pc ps' `elem` (traps >>= \t -> [t-2,t-1,t]) then do
    cont <- debugBreak traps ps'
    when cont $ simLoopDeb traps ps'
  else
    simLoopDeb traps ps'

simLoop :: Bool -> ProgramState -> IO ()
simLoop printLinear ps = do
  ps' <-
    step
      readLn
      (if printLinear then (\v -> putStrLn $ charize v : " (" ++ show v ++ ")") else (\v -> putStr [charize v]))
      ps
  simLoop printLinear ps'

runSimulator :: Bool -> Bool -> [Int] -> [Int] -> IO ()
runSimulator verbose lp traps prog = do
  let is = initialState prog
  when verbose $ print is
  if verbose
    then
      simLoopDeb (map fromIntegral traps) is
    else
      simLoop lp is
