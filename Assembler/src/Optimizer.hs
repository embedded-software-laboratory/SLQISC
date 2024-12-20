module Optimizer (preOptimize, postOptimize, debugPostOptimize) where

import Assembly
import Data.List (intercalate, nub, (\\))
import Data.Map.Strict qualified as M
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

multiReplacements :: [Directive] -> [Directive]
multiReplacements [] = []
multiReplacements (DMacro (MSub a b) : DMacro (MJLeq c t) : ds) | a == c = multiReplacements (DMacro (MSLQ b a t) : ds)
multiReplacements (d : ds) = d : multiReplacements ds

optimizeMacros :: [Directive] -> [Directive]
optimizeMacros ls = multiReplacements (ls >>= singleReplacements)

preOptimize :: Assembly -> Assembly
preOptimize = map (\s -> s {directives = labelPreservingMap optimizeMacros (directives s)})

data Edge = Edge {source :: Int, label :: (Int, Int, Int), target :: Int} deriving (Eq, Ord)

instance Show Edge where
  show (Edge a (b, c, _) s) = show a ++ " -> " ++ show s ++ " [label=\"" ++ dotLabel a b c s ++ "\"]\n"

data CFA = CFA [Int] [Edge]

newtype BBCFA = BBCFA {basicBlocks :: [(Int, [(Int, Int)], Int)]}

dotLabel :: Int -> Int -> Int -> Int -> String
dotLabel a b c s
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
  show (CFA _ e) = "digraph G {" ++ concatMap show e ++ "}"

instance Show BBCFA where
  show (BBCFA e) =
    "digraph G {0[label=\"0\"]\n"
      ++ concatMap
        ( \case
            (a, [], s) -> show a ++ " -> " ++ show s ++ "\n"
            (a, bb, s) -> "v" ++ show a ++ "_" ++ show s ++ "[shape=rect,label=\"" ++ bblabel a bb s ++ "\"]\n" ++ show a ++ " -> " ++ "v" ++ show a ++ "_" ++ show s ++ "\n" ++ "v" ++ show a ++ "_" ++ show s ++ " -> " ++ show s ++ "\n"
        )
        e
      ++ "}"

bbStep' :: CFA -> Int -> M.Map Int [Int] -> M.Map Int [Int]
bbStep' (CFA _ e) v blocks =
  if not (v `M.member` blocks)
    then blocks
    else
      let exit = last (blocks M.! v)
          succs = filter (\edge -> source edge == exit) e
       in if length succs /= 1
            then blocks
            else
              let edge = head succs
               in if length (filter (\edge' -> target edge' == target edge) e) /= 1
                    then blocks
                    else
                      M.adjust (++ (blocks M.! target edge)) v $ M.delete (target edge) blocks

bbStep :: CFA -> M.Map Int [Int] -> M.Map Int [Int]
bbStep cfa@(CFA v e) blocks =
  let folded = foldl (flip (bbStep' cfa)) blocks v
   in if blocks == folded then blocks else bbStep (CFA v e) folded

outgoing :: [Edge] -> Int -> [Edge]
outgoing e v = filter (\edge -> source edge == v) e

blockEdge :: [Edge] -> [Int] -> (Int, [(Int, Int)], Int)
blockEdge e b =
  let esource = head b
      post = outgoing e (last b)
      elabel = zipWith (\epred esucc -> (\(ra, rb, _) -> (ra, rb)) $ label $ head $ filter (\edge -> source edge == epred && target edge == esucc) e) b (tail b) ++ case post of [Edge _ (x, y, _) _] -> [(x, y)]; _ -> []
      etarget = case post of [Edge _ _ t] -> t; _ -> last b
   in (esource, elabel, etarget)

isSource :: [(Int, [(Int, Int)], Int)] -> Int -> Bool
isSource edges v = not $ any (\(_, _, t) -> t == v) edges

buildBlocks :: CFA -> [[Int]] -> BBCFA
buildBlocks (CFA _ e) blocks =
  let blockEdges = map (blockEdge e) $ filter (\b -> length b > 1) blocks
      tweenEdges = map (\edge -> (source edge, [(\(a, b, _) -> (a, b)) $ label edge], target edge)) $ filter (\edge -> source edge `elem` map last blocks) e
   in BBCFA $ (\es -> filter (\(v, _, _) -> v == 0 || not (isSource es v)) es) (blockEdges ++ tweenEdges)

basicBlock :: CFA -> BBCFA
basicBlock (CFA v e) =
  let iblocks = M.fromList $ map (\vx -> (vx, [vx])) v
   in buildBlocks (CFA v e) $ M.elems $ bbStep (CFA v e) iblocks

cfaStep :: (Int -> (Int, Int, Int)) -> ([Int], CFA) -> ([Int], CFA)
cfaStep _ ([], CFA v e) = ([], CFA v e)
cfaStep prog (n : open, CFA v e) =
  let (a, b, c) = prog n
      succs = if a /= b then nub [n + 3, c] else [c]
      openSuccs = (succs \\ (n : open)) \\ v
   in cfaStep prog (openSuccs ++ open, CFA (v ++ [n]) (e ++ [Edge n (a, b, c) s | s <- succs]))

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
  let bbcfa = basicBlock cfa
  print bbcfa
  putStr "Reads: "
  print (staticReads bbcfa)
  putStr "Writes: "
  print (staticWrites bbcfa)
  -- putStr "Values: "
  -- print (progFix xs :: ProgramState Const)
  return xs

blockOptimize :: [(Int, Int)] -> ([(Int, Int)], Int -> Int)
blockOptimize x = (x, id)

staticWrites :: BBCFA -> [Int]
staticWrites (BBCFA e) = nub (e >>= (\(_, ll, _) -> map snd ll))

staticReads :: BBCFA -> [Int]
staticReads (BBCFA e) = nub (e >>= (\(_, ll, _) -> ll >>= (\(a, b) -> [a, b])))

postOptimize :: [Int] -> [Int]
postOptimize xs =
  let cfa = buildCFA xs
      bb = basicBlock cfa
   in xs