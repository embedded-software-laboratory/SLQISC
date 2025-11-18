{-# LANGUAGE LambdaCase #-}

module Assembler (assemble, debugAssemble, outputLogisim, outputMif) where

import Assembly
  ( Assembly,
    Directive (..),
    LDirective (..),
    Reg (..),
    Section (..),
    charValue,
    implem,
    sectionSize,
  )
import Control.Arrow
import Control.Monad
import Data.List (elemIndex, findIndex, nub, sortOn)
import qualified Data.List.NonEmpty as LNE
import Data.Maybe
import Optimizer (debugPostOptimize, postOptimize, preOptimize)
import System.IO
import Text.Printf

cConst :: Directive -> Maybe Int
cConst (DImm i) = Just i
cConst (DImmChar c) = Just (charValue c)
cConst (DBreak x) = cConst x
cConst _ = Nothing

collectConsts :: [LDirective] -> [Int]
collectConsts l = l >>= (\case LabelledDirective _ d -> maybeToList $ cConst d; RawDirective d -> maybeToList $ cConst d)

replaceConst' :: [Int] -> Directive -> Directive
replaceConst' consts (DImm i) = DSum (DLabel "_c") $ DNumber (fromJust $ elemIndex i consts)
replaceConst' consts (DImmChar c) = DSum (DLabel "_c") $ DNumber (fromJust $ elemIndex (charValue c) consts)
replaceConst' consts (DSum a b) = DSum (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DDiff a b) = DDiff (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DMul a b) = DMul (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DBreak x) = DBreak (replaceConst' consts x)
replaceConst' _ d = d

replaceConst :: [Int] -> LDirective -> LDirective
replaceConst consts (LabelledDirective l d) = LabelledDirective l (replaceConst' consts d)
replaceConst consts (RawDirective d) = RawDirective (replaceConst' consts d)

constReplace :: [Int]-> Section -> Section
constReplace consts s = Section (sName s) (location s) (map (replaceConst consts) (directives s))

constSection :: Assembly -> Assembly
constSection a =
  let consts = nub $ a >>= (collectConsts . directives)
      preSection c = Section { sName = "__consts", location = Nothing, directives = LabelledDirective "_c" (DNumber $ LNE.head c) : map (RawDirective . DNumber) (LNE.tail c) }
      resolved c = map (constReplace $ LNE.toList c) a
   in maybe a (\c -> preSection c : resolved c) (LNE.nonEmpty consts)

firstFit :: [(Int, Int)] -> Int -> Int
firstFit [] _ = 0
firstFit [(s,l)] _ = s+l 
firstFit ((s0,l0):r@((s1,_):_)) len = if len <= s1-(s0+l0) then s0+l0 else firstFit r len

placeSection :: Assembly -> Section -> Section
placeSection a s =
  let pos = sortOn fst $ mapMaybe (\sec -> location sec >>= \loc -> return (loc, sectionSize sec)) a
      ff = firstFit pos (sectionSize s)
   in Section (sName s) (Just ff) (directives s)

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x : xs) = f x : xs
replace f i (x : xs) = x : replace f (i - 1) xs
replace _ _ _ = []

placeSections :: Assembly -> Assembly
placeSections a =
  let i = findIndex (isNothing . location) a
   in if isNothing i
        then
          a
        else
          placeSections (replace (placeSection a) (fromJust i) a)

padSections :: Assembly -> Assembly
padSections a =
  let a' = sortOn (fromJust . location) a
   in head a' : concat (zipWith (\l r -> [let s = (+ length (directives l)) <$> location l in Section "pad" s (replicate (fromJust (location r) - fromJust s) (RawDirective 0)), r]) a' (tail a'))

squash :: Assembly -> [LDirective]
squash a =
  let a' = sortOn (fromJust . location) a
   in a' >>= directives

findLabel :: [LDirective] -> String -> Int
findLabel l s = fromJust $ elemIndex (Just s) $ map (\case LabelledDirective ll _ -> Just ll; _ -> Nothing) l

resolveLabel :: (String -> Int) -> Directive -> Directive
resolveLabel f (DLabel l) = DNumber $ f l
resolveLabel f (DSum a b) = DSum (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DDiff a b) = DDiff (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DMul a b) = DMul (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DBreak x) = DBreak (resolveLabel f x)
resolveLabel _ d = d

resolveCur :: Int -> Directive -> Directive
resolveCur i DCur = DNumber i
resolveCur i (DSum a b) = DSum (resolveCur i a) (resolveCur i b)
resolveCur i (DDiff a b) = DDiff (resolveCur i a) (resolveCur i b)
resolveCur i (DMul a b) = DMul (resolveCur i a) (resolveCur i b)
resolveCur i (DBreak x) = DBreak (resolveCur i x)
resolveCur _ d = d

resolveLabels :: [LDirective] -> [Directive]
resolveLabels ds =
  zipWith resolveCur [0 ..] $
    map
      ( resolveLabel (findLabel ds)
          . (\case LabelledDirective _ d -> d; RawDirective d -> d)
      )
      ds

regToInt :: Reg -> Int
regToInt ROut = -1
regToInt RIn = -2
regToInt RSP = -3
regToInt (RGPR i) = -(i + 17)

toInt :: Directive -> Int
toInt (DNumber i) = i
toInt (DImm _) = undefined
toInt (DChar c) = charValue c
toInt (DImmChar _) = undefined
toInt (DLabel _) = undefined
toInt DCur = undefined
toInt (DSum a b) = toInt a + toInt b
toInt (DDiff a b) = toInt a - toInt b
toInt (DMul a b) = toInt a * toInt b
toInt (DMacro _) = undefined
toInt (DReg r) = regToInt r
toInt (DBreak r) = toInt r

toInts :: [Directive] -> [Int]
toInts = map toInt

resolveMacrosD' :: Directive -> Directive -> ([Directive], Int)
resolveMacrosD' pc (DMacro m) = implem ((DLabel "_v" +) . DNumber) pc m
resolveMacrosD' _ d = ([d], 0)

resolveMacrosD :: String -> LDirective -> ([LDirective], Int)
resolveMacrosD _ (LabelledDirective l d) = first (\case (x : xs) -> LabelledDirective l x : map RawDirective xs; _ -> []) (resolveMacrosD' (DLabel l) d)
resolveMacrosD free (RawDirective d) = first (\case (x : xs) -> LabelledDirective free x : map RawDirective xs; _ -> []) (resolveMacrosD' (DLabel free) d)

resolveMacros :: Assembly -> Assembly
resolveMacros a =
  let resolved s = zipWith (\i -> resolveMacrosD ("_" ++ sName s ++ show i)) [0 :: Int ..] (directives s)
      newDirs = map (\s -> s {directives = resolved s >>= fst}) a
      vCount = maximum $ a >>= (map snd . resolved)
      preSection = Section "__vars" Nothing (LabelledDirective "_v" 0 : replicate (vCount - 1) (RawDirective 0))
   in if vCount > 0 then preSection : newDirs else newDirs

extractTraps :: [Directive] -> [Int]
extractTraps ds = map fst $ filter (\case (_,DBreak _) -> True; _ -> False) $ zip [0..] ds

assemble :: Assembly -> ([Int],[Int])
assemble = preOptimize
  >>> resolveMacros
  >>> constSection
  >>> placeSections
  >>> padSections
  >>> squash
  >>> resolveLabels
  >>> (extractTraps &&& (toInts >>> postOptimize))

debugAssemble :: String -> Assembly -> IO ([Int],[Int])
debugAssemble f a = withFile f WriteMode $ \h -> do
  hPutStrLn h "Input: "
  hPrint h a
  let preOpt = preOptimize a
  hPutStrLn h "Pre-Optimized: "
  hPrint h preOpt
  let demacro = resolveMacros preOpt
  hPutStrLn h "Demacro: "
  hPrint h demacro
  let constS = constSection demacro
  hPutStrLn h "Const section: "
  hPrint h constS
  let placed = placeSections constS
  hPutStrLn h "Placed: "
  hPrint h placed
  let padded = padSections placed
  hPutStrLn h "Padded: "
  hPrint h padded
  let squashed = squash padded
  hPutStrLn h "Squashed: "
  hPrint h squashed
  let resolved = resolveLabels squashed
  hPutStrLn h "Resolved Labels: "
  hPrint h resolved
  let traps = extractTraps resolved
  hPutStrLn h "Traps: "
  hPrint h traps
  let assembled = toInts resolved
  hPutStrLn h "Assembled: "
  hPrint h assembled
  postOptimized <- debugPostOptimize assembled
  hPutStrLn h "Post-Optimized: "
  hPrint h postOptimized
  return (traps,postOptimized)

outputLogisim :: String -> [Int] -> IO ()
outputLogisim f dat = withFile f WriteMode $ \h -> do
  hPutStrLn h "v3.0 hex bytes plain big-endian"
  hPutStrLn h $ unwords $ map (printf "%04hx") dat

outputMif :: String -> [Int] -> IO ()
outputMif f dat = withFile f WriteMode $ \h -> do
  hPutStrLn h "WIDTH = 16;"
  hPutStrLn h "DEPTH = 65536;"
  hPutStrLn h ""
  hPutStrLn h "ADDRESS_RADIX = HEX;"
  hPutStrLn h "DATA_RADIX = HEX;"
  hPutStrLn h "CONTENT BEGIN"
  hPutStrLn h $ unlines $ zipWith (\i d -> "  " ++ printf "%04hx" i ++ ": " ++ printf "%04hx" d ++ ";") [0 :: Integer ..] dat
  when (length dat < 65535) $
    hPutStrLn h $
      "  [" ++ printf "%04hx" (length dat) ++ "..ffff] : XXXX;"
  when (length dat == 65535) $
    hPutStrLn h "  ffff: XXXX;"
  hPutStrLn h "END;"
