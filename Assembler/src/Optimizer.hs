module Optimizer (preOptimize, postOptimize) where

import Assembly

pdecomp :: Int -> (Int, Int)
pdecomp k
  | odd k = (0, k)
  | otherwise = let (t, r) = pdecomp (div k 2) in (t + 1, r)

singleReplacements :: Directive -> [Directive]
singleReplacements (DMacro (MMul x (DImm k))) = let (t, r) = pdecomp k in replicate t (DMacro (MAdd x x)) ++ [DMacro $ MMul x (DImm r) | r /= 1]
singleReplacements (DMacro (MAdd x (DImm k))) = [DMacro (MSub x (DImm (-k)))]
singleReplacements (DMacro (MMov x (DImm 0))) = [DMacro (MSub x x)]
singleReplacements (DMacro (MMov x (DImm k))) = [DMacro (MSub x x), DMacro (MSub x (DImm (-k)))]
singleReplacements (DMacro (MSub _ (DImm 0))) = []
singleReplacements d = [d]

optimizeMacros :: [Directive] -> [Directive]
optimizeMacros ls = ls >>= singleReplacements

preOptimize :: Assembly -> Assembly
preOptimize = map (\s -> s {directives = labelPreservingMap optimizeMacros (directives s)})

postOptimize :: [Directive] -> [Directive]
postOptimize = id