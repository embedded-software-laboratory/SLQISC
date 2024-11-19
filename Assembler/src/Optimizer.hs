module Optimizer (preOptimize, postOptimize, debugPostOptimize) where

import Assembly
import Data.List (intercalate, nub, (\\))
import Data.Maybe

mul2 :: Directive -> [Directive]
mul2 x = [DMacro (MAdd x x)]

mul3 :: Directive -> [Directive]
mul3 x = nobranch x (DImm 0) ++ nobranch (DImm 0) x ++ nobranch (DImm 0) x ++ nobranch (DImm 0) (DImm 0)

mul5 :: Directive -> [Directive]
mul5 x = nobranch x (DImm 0) ++ nobranch (DImm 0) x ++ nobranch (DImm 0) x ++ nobranch (DImm 0) x ++ nobranch (DImm 0) x ++ nobranch (DImm 0) (DImm 0)

pdecomp :: Int -> (Int, Int, Int, Int)
pdecomp k
  | even k = let (t2, t3, t5, r) = pdecomp (div k 2) in (t2 + 1, t3, t5, r)
  | mod k 3 == 0 = let (t2, t3, t5, r) = pdecomp (div k 3) in (t2, t3 + 1, t5, r)
  | mod k 5 == 0 = let (t2, t3, t5, r) = pdecomp (div k 5) in (t2, t3, t5 + 1, r)
  | otherwise = (0, 0, 0, k)

singleReplacements :: Directive -> [Directive]
singleReplacements (DMacro (MMul x (DImm k))) = let (t2, t3, t5, r) = pdecomp k in concat (replicate t2 (mul2 x) ++ replicate t3 (mul3 x) ++ replicate t5 (mul5 x)) ++ [DMacro $ MMul x (DImm r) | r /= 1]
singleReplacements (DMacro (MAdd x (DImm k))) = [DMacro (MSub x (DImm (-k)))]
singleReplacements (DMacro (MMov x (DImm 0))) = [DMacro (MSub x x)]
singleReplacements (DMacro (MMov x (DImm k))) = [DMacro (MSub x x), DMacro (MSub x (DImm (-k)))]
singleReplacements (DMacro (MSub _ (DImm 0))) = []
singleReplacements d = [d]

optimizeMacros :: [Directive] -> [Directive]
optimizeMacros ls = ls >>= singleReplacements

preOptimize :: Assembly -> Assembly
preOptimize = map (\s -> s {directives = labelPreservingMap optimizeMacros (directives s)})

data CFA = CFA [Int] [(Int, (Int, Int, Int), Int)]

newtype BBCFA = BBCFA [(Int, [(Int, Int)], Int)]

label :: Int -> Int -> Int -> Int -> String
label a b c s
  | a + 3 == s && c /= b = "$" ++ show c ++ " -= $" ++ show b
  | a + 3 == s = "$" ++ show c ++ " = 0"
  | c /= b = "$" ++ show c ++ " -= $" ++ show b ++ " [JMP]"
  | otherwise = "$" ++ show c ++ " = 0 [JMP]"

njlabel :: Int -> Int -> String
njlabel b c
  | c /= b = "$" ++ show c ++ " -= $" ++ show b
  | otherwise = "$" ++ show c ++ " = 0"

bblabel :: Int -> [(Int, Int)] -> Int -> String
bblabel _ bb _ = intercalate "\\n" $ map (uncurry njlabel) bb

instance Show CFA where
  show (CFA _ e) = "digraph G {" ++ concatMap (\(a, (b, c, _), s) -> show a ++ " -> " ++ show s ++ " [label=\"" ++ label a b c s ++ "\"]\n") e ++ "}"

instance Show BBCFA where
  show (BBCFA e) =
    "digraph G {"
      ++ concatMap
        ( \case
            (a, [], s) -> show a ++ " -> " ++ show s
            (a, bb, s) -> "v" ++ show a ++ "_" ++ show s ++ "[shape=rect,label=\"" ++ bblabel a bb s ++ "\"]\n" ++ show a ++ " -> " ++ "v" ++ show a ++ "_" ++ show s ++ "\n" ++ "v" ++ show a ++ "_" ++ show s ++ " -> " ++ show s ++ "\n"
        )
        e
      ++ "}"

basicBlock' :: [(Int, [(Int, Int)], Int)] -> [(Int, [(Int, Int)], Int)]
basicBlock' [] = []
basicBlock' ((a, bb, s) : xs) =
  let succs = filter (\(a', _, _) -> a' == s) xs
   in case succs of
        [scc@(a', bb', s')] | not $ any (\(_, _, s'') -> a' == s'') xs -> basicBlock' ((a, bb ++ bb', s') : (xs \\ [scc]))
        [] -> (a, bb, s) : (s, [], a) : basicBlock' xs
        _ -> (a, bb, s) : basicBlock' xs

basicBlock :: CFA -> BBCFA
basicBlock (CFA _ e) = BBCFA $ basicBlock' $ map (\(pre, (a, b, _), post) -> (pre, [(a, b)], post)) e

cfaStep :: (Int -> (Int, Int, Int)) -> ([Int], CFA) -> ([Int], CFA)
cfaStep _ ([], CFA v e) = ([], CFA v e)
cfaStep prog (n : open, CFA v e) =
  let (a, b, c) = prog n
      succs = if a /= b then nub [n + 3, c] else [c]
      openSuccs = (succs \\ (n : open)) \\ v
   in cfaStep prog (openSuccs ++ open, CFA (v ++ [n]) (e ++ [(n, (a, b, c), s) | s <- succs]))

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? i = xs !? (i - 1)

buildCFA :: [Int] -> CFA
buildCFA xs =
  let fetcher i = (fromMaybe 0 (xs !? i), fromMaybe 0 (xs !? (i + 1)), fromMaybe 0 (xs !? (i + 2)))
   in snd $ cfaStep fetcher ([0], CFA [] [])

debugPostOptimize :: [Int] -> IO [Int]
debugPostOptimize xs = do
  let cfa = buildCFA xs
  print cfa
  return xs

postOptimize :: [Int] -> [Int]
postOptimize = id