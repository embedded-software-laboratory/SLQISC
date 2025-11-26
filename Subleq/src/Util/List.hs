module Util.List (safeMaximum)  where

safeMaximum :: (Ord a) => a -> [a] -> a
safeMaximum m [] = m
safeMaximum m [x] = max x m
safeMaximum m (x:xs) = safeMaximum (max x m) xs
