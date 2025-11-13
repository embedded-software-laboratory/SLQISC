module Simulator (progFix, Const, ProgramState (..)) where

import Data.List (nub, (\\))
import Data.Map qualified as M
import Data.Maybe

data ProgramState a = ProgramState
  { memory :: M.Map a a,
    pc :: Int
  }

instance (Eq a) => Eq (ProgramState a) where
  (==) a b = memory a == memory b && pc a == pc b

instance (Show a) => Show (ProgramState a) where
  show state = "{" ++ show (memory state) ++ "@pc=" ++ show (pc state) ++ "}"

class (Ord a) => MemType a where
  zero :: a
  constant :: Int -> a
  interp :: a -> [Int]
  sub :: a -> a -> a
  leq0 :: a -> (a, a)
  pop :: a -> Bool

class Abstract a where
  join :: [a] -> a
  bjoin :: a -> a -> a
  bjoin a b = join [a, b]

instance (Abstract a, Ord b) => Abstract (M.Map b a) where
  join = M.unionsWith bjoin

instance (Abstract a, Ord a) => Abstract (ProgramState a) where
  join xs = ProgramState (join (map memory xs)) (maximum (map pc xs))

instance MemType (Maybe Int) where
  zero = Just 0
  constant = Just
  interp = maybeToList
  sub = liftA2 (-)
  leq0 Nothing = (Nothing, Nothing)
  leq0 (Just i) = if i <= 0 then (Just i, Nothing) else (Nothing, Just i)
  pop = isJust

data Const = Bot | Const Int | Top deriving (Eq, Ord)

instance Show Const where
  show Bot = "{}"
  show (Const i) = show i
  show Top = "T"

instance MemType Const where
  zero = Const 0
  constant = Const
  interp Bot = []
  interp (Const i) = [i]
  interp Top = [-32768 .. 32767]
  sub Bot _ = Bot
  sub _ Bot = Bot
  sub (Const a) (Const b) = Const (a - b)
  sub _ _ = Top
  leq0 Bot = (Bot, Bot)
  leq0 (Const i) = if i <= 0 then (Const i, Bot) else (Bot, Const i)
  leq0 Top = (Top, Top)
  pop Bot = False
  pop _ = True

instance Abstract Const where
  join xs = join' (nub xs \\ [Bot])
    where
      join' [] = Bot
      join' [Const i] = Const i
      join' _ = Top

step :: (MemType a) => ProgramState a -> [ProgramState a]
step state =
  let locA = M.findWithDefault zero (constant (pc state)) $ memory state
      locB = M.findWithDefault zero (constant (pc state + 1)) $ memory state
      targ = M.findWithDefault zero (constant (pc state + 2)) $ memory state
      valA = M.findWithDefault zero locA $ memory state
      valB = M.findWithDefault zero locB $ memory state
      res = sub valB valA
      (l, r) = leq0 res
      meml = M.insert locB l $ memory state
      memr = M.insert locB r $ memory state
   in [ProgramState meml t | pop l, t <- interp targ] ++ [ProgramState memr (pc state + 3) | pop r, locA /= locB]

progToState :: (MemType a) => [Int] -> ProgramState a
progToState prog = ProgramState (M.fromList $ zip (map constant [0 ..]) (map constant prog)) 0

pcStep :: (MemType a, Abstract a) => M.Map Int (ProgramState a) -> M.Map Int (ProgramState a)
pcStep m =
  let post = M.elems m >>= step
      postJoined = foldr (\state -> M.insertWith bjoin (pc state) state) M.empty post
   in M.unionWith bjoin m postJoined

pcFix :: (MemType a, Abstract a) => M.Map Int (ProgramState a) -> M.Map Int (ProgramState a)
pcFix m =
  let post = pcStep m
   in if post == m then m else pcFix post

progFix :: (MemType a, Abstract a) => [Int] -> ProgramState a
progFix prog =
  let state = progToState prog
   in join $ M.elems $ pcFix $ M.singleton 0 state