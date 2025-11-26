module Optimizer (preOptimize, postOptimize, debugPostOptimize) where

import Assembly

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
multiReplacements (DMacro (MSub a b) : DMacro (MJLeq c t) : ds) | a == c = multiReplacements ([b, a, t] ++ ds)
multiReplacements (DMacro (MInc a) : DMacro (MJLeq c t) : ds) | a == c = multiReplacements ([DImm (-1), a, t] ++ ds)
multiReplacements (DMacro (MDec a) : DMacro (MJLeq c t) : ds) | a == c = multiReplacements ([DImm 1, a, t] ++ ds)
multiReplacements (d : ds) = d : multiReplacements ds

optimizeMacros :: [Directive] -> [Directive]
optimizeMacros ls = multiReplacements (ls >>= singleReplacements)

preOptimize :: Assembly -> Assembly
preOptimize = map (\s -> s {directives = labelPreservingMap optimizeMacros (directives s)})

debugPostOptimize :: [Int] -> IO [Int]
debugPostOptimize = return

postOptimize :: [Int] -> [Int]
postOptimize = id
