{-# LANGUAGE LambdaCase, Arrows #-}

module Assembler (assemble, debugAssemble, disassemble, outputLogisim, outputMif) where

import Assembly
  ( Assembly,
    Directive (..),
    LDirective (..),
    Macro (..),
    Reg (..),
    Section (..),
    charValue,
    implem,
    sectionSize,
    rawDir
  )
import Codec.Picture (DynamicImage, readImage)
import Control.Arrow (Arrow (first),returnA)
import Control.Monad (guard, when, forM)
import Data.Function ((&))
import Data.Int (Int16)
import Data.List (elemIndex, findIndex, sortOn)
import Data.List.NonEmpty qualified as LNE
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as S
import Optimizer (debugPostOptimize, postOptimize, preOptimize)
import System.IO (IOMode (WriteMode), hPrint, hPutStrLn, withFile)
import Text.Printf (printf)
import Util.Image (slqimg)
import Util.List

cConst :: Directive -> Maybe Int
cConst (DImm i) = Just i
cConst (DImmChar c) = Just (charValue c)
cConst (DBreak x) = cConst x
cConst _ = Nothing

collectConsts :: [LDirective] -> S.Set Int
collectConsts l = S.fromList $ l >>= (\case LabelledDirective _ d -> maybeToList $ cConst d; RawDirective d -> maybeToList $ cConst d)

replaceConst' :: [Int] -> Directive -> Directive
replaceConst' consts (DImm i) = DSum (DLabel "_c") $ DNumber (fromMaybe 0 $ elemIndex i consts)
replaceConst' consts (DImmChar c) = DSum (DLabel "_c") $ DNumber (fromMaybe 0 $ elemIndex (charValue c) consts)
replaceConst' consts (DSum a b) = DSum (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DDiff a b) = DDiff (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DMul a b) = DMul (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DBreak x) = DBreak (replaceConst' consts x)
replaceConst' _ d = d

replaceConst :: [Int] -> LDirective -> LDirective
replaceConst consts (LabelledDirective l d) = LabelledDirective l (replaceConst' consts d)
replaceConst consts (RawDirective d) = RawDirective (replaceConst' consts d)

constReplace :: [Int] -> Section -> Section
constReplace consts s = Section (sName s) (location s) (map (replaceConst consts) (directives s))

constSection :: Assembly -> Assembly
constSection a =
  let consts = S.unions $ map (collectConsts . directives) a
      preSection c = Section {sName = "__consts", location = Nothing, directives = LabelledDirective "_c" (DNumber $ LNE.head c) : map (RawDirective . DNumber) (LNE.tail c)}
      resolved c = map (constReplace $ LNE.toList c) a
   in maybe a (\c -> preSection c : resolved c) (LNE.nonEmpty $ S.toList consts)

firstFit :: [(Int, Int)] -> Int -> Int
firstFit [] _ = 0
firstFit [(s, l)] _ = s + l
firstFit ((s0, l0) : r@((s1, _) : _)) len = if len <= s1 - (s0 + l0) then s0 + l0 else firstFit r len

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
   in maybe a (\i' -> placeSections (replace (placeSection a) i' a)) i

padSections :: Assembly -> Assembly
padSections [] = []
padSections a@(_ : _) =
  let a' = sortOn location a
   in case a' of [] -> []; (s0 : sr) -> s0 : concat (zipWith (\l r -> [let s = (+ length (directives l)) <$> location l in Section {sName = "pad", location = s, directives = replicate (fromJust (location r) - fromJust s) (RawDirective 0)}, r]) (s0 : sr) sr)

squash :: Assembly -> [LDirective]
squash a =
  let a' = sortOn location a
   in a' >>= directives

findLabel :: [LDirective] -> String -> Maybe Int
findLabel l s = elemIndex (Just s) $ map (\case LabelledDirective ll _ -> Just ll; _ -> Nothing) l

resolveLabel :: (String -> Maybe Int) -> Directive -> Directive
resolveLabel f (DLabel l) = maybe 0 DNumber $ f l
resolveLabel f (DSum a b) = DSum (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DDiff a b) = DDiff (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DMul a b) = DMul (resolveLabel f a) (resolveLabel f b)
resolveLabel f (DBreak x) = DBreak (resolveLabel f x)
resolveLabel _ d = d

resolveCur :: Int -> Directive -> Directive
resolveCur i DCur = DNumber i
resolveCur i (DSum a b) = resolveCur i a + resolveCur i b
resolveCur i (DDiff a b) = resolveCur i a - resolveCur i b
resolveCur i (DMul a b) = resolveCur i a * resolveCur i b
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

toInt :: Directive -> Maybe Int
toInt (DNumber i) = return i
toInt (DImm _) = Nothing
toInt (DChar c) = return $ charValue c
toInt (DImmChar _) = Nothing
toInt (DLabel _) = Nothing
toInt DCur = Nothing
toInt (DSum a b) = (+) <$> toInt a <*> toInt b
toInt (DDiff a b) = (-) <$> toInt a <*> toInt b
toInt (DMul a b) = (*) <$> toInt a <*> toInt b
toInt (DMacro _) = Nothing
toInt (DReg r) = return $ regToInt r
toInt (DBreak r) = toInt r

toInts :: [Directive] -> [Int]
toInts = mapMaybe toInt

resolveMacrosD' :: M.Map String [Int] -> Directive -> Directive -> ([Directive], Int)
resolveMacrosD' imgs pc (DMacro m) = implem imgs ((DLabel "_v" +) . DNumber) pc m
resolveMacrosD' _ _ d = ([d], 0)

resolveMacrosD :: M.Map String [Int] -> String -> LDirective -> ([LDirective], Int)
resolveMacrosD imgs _ (LabelledDirective l d) = first (\case (x : xs) -> LabelledDirective l x : map RawDirective xs; [] -> []) (resolveMacrosD' imgs (DLabel l) d)
resolveMacrosD imgs free (RawDirective d) = first (\case (x : xs) -> LabelledDirective free x : map RawDirective xs; [] -> []) (resolveMacrosD' imgs (DLabel free) d)

resolveMacros ::  M.Map String [Int] -> Assembly -> Assembly
resolveMacros imgs a =
  let resolved s = zipWith (\i -> resolveMacrosD imgs ("_" ++ sName s ++ show i)) [0 :: Int ..] (directives s)
      newDirs = map (\s -> s {directives = resolved s >>= fst}) a
      vCount = safeMaximum 0 $ a >>= (map snd . resolved)
      preSection = Section {sName = "__vars", location = Nothing, directives = LabelledDirective "_v" 0 : replicate (vCount - 1) (RawDirective 0)}
   in if vCount > 0 then preSection : newDirs else newDirs

extractTraps :: [Directive] -> [Int]
extractTraps ds = map fst $ filter (\case (_, DBreak _) -> True; _nonbreak -> False) $ zip [0 ..] ds

fetchImages :: Assembly -> IO (M.Map String [Int])
fetchImages asm = do
  let dirs = map rawDir (asm >>= directives)
  let imgs = dirs >>= (\case DMacro (MImg uri) -> [uri]; _ -> [])
  lst <- forM imgs $ \uri -> do
    load <- readImage uri
    case load of
      Left err -> error err
      Right di -> return (uri,slqimg di)
  return $ M.fromList lst

assemble :: Assembly -> IO ([Int], [Int])
assemble a = do
  imgs <- fetchImages a
  a & proc asm -> do
    opt <- preOptimize -< asm
    rsm <- (resolveMacros imgs) -< opt
    cns <- constSection -< rsm
    plc <- placeSections -< cns
    pad <- padSections -< plc
    sqh <- squash -< pad
    rsl <- resolveLabels -< sqh
    trp <- extractTraps -< rsl
    int <- toInts -< rsl
    pop <- postOptimize -< int
    returnA -< return (trp,pop)

debugAssemble :: String -> Assembly -> IO ([Int], [Int])
debugAssemble f a = withFile f WriteMode $ \h -> do
  imgs <- fetchImages a
  hPutStrLn h "Input: "
  hPrint h a
  let preOpt = preOptimize a
  hPutStrLn h "Pre-Optimized: "
  hPrint h preOpt
  let demacro = resolveMacros imgs preOpt
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
  return (traps, postOptimized)

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

type DisAssGuess = Int16 -> M.Map Int16 Int16 -> Maybe (Maybe Int16, Macro)

guessTrap :: DisAssGuess
guessTrap pc memoryMap = do
  let a = fromMaybe 0 $ M.lookup pc memoryMap
  let b = fromMaybe 0 $ M.lookup (pc + 1) memoryMap
  let c = fromMaybe 0 $ M.lookup (pc + 2) memoryMap
  guard (a == b && c == pc)
  return (Nothing, MTrap)

guessMov :: DisAssGuess
guessMov pc memoryMap =
  case map (\d -> fromMaybe 0 $ M.lookup (pc + d) memoryMap) [0 .. 11] of
    [a0, a1, a2, b0, b1, b2, c0, c1, c2, d0, d1, d2] -> do
      guard
        ( a0 == a1 && a0 == c1
            && b1 == c0 && b1 == d0 && b1 == d1
            && a2 == pc + 3
            && b2 == pc + 6
            && c2 == pc + 9
            && d2 == pc + 12
        )
      return (Just (pc + 12), MMov (DNumber $ fromIntegral a0) (DNumber $ fromIntegral b0))
    _ie -> Nothing

guessAdd :: DisAssGuess
guessAdd pc memoryMap =
  case map (\d -> fromMaybe 0 $ M.lookup (pc + d) memoryMap) [0 .. 8] of
    [a0, a1, a2, b0, b1, b2, c0, c1, c2] -> do
      guard
        ( a1 == b0
            && a1 == c0
            && a1 == c1
            && a2 == pc + 3
            && b2 == pc + 6
            && c2 == pc + 9
        )
      return (Just (pc + 9), MMov (DNumber $ fromIntegral b1) (DNumber $ fromIntegral a0))
    _ie -> Nothing

guessSub :: DisAssGuess
guessSub pc memoryMap =
  case map (\d -> fromMaybe 0 $ M.lookup (pc + d) memoryMap) [0 .. 2] of
    [a0, a1, a2] -> do
      guard (a2 == pc + 3)
      return (Just (pc + 3), MSub (DNumber $ fromIntegral a1) (DNumber $ fromIntegral a0))
    _ie -> Nothing

guessOut :: DisAssGuess
guessOut pc memoryMap = do
  case map (\d -> fromMaybe 0 $ M.lookup (pc + d) memoryMap) [0 .. 2] of
    [a0, a1, a2] -> do
      guard (a1 == -1 && a2 == pc + 3)
      return (Just (pc + 3), MOut (DNumber $ fromIntegral a0))
    _ie -> Nothing

guessInstruction :: Int16 -> M.Map Int16 Int16 -> (Maybe Int16, Macro)
guessInstruction pc memoryMap =
  let guesses = [guessTrap, guessMov, guessAdd, guessOut, guessSub]
      a = DNumber $ fromIntegral $ fromMaybe 0 $ M.lookup pc memoryMap
      b = DNumber $ fromIntegral $ fromMaybe 0 $ M.lookup (pc + 1) memoryMap
      cVal = fromMaybe 0 $ M.lookup (pc + 2) memoryMap
      c = DNumber $ fromIntegral cVal
   in fromMaybe (Just (pc + 3), MSLQ a b c) $ listToMaybe (mapMaybe (\g -> g pc memoryMap) guesses)

siStep :: M.Map Int16 Int16 -> (Maybe Int16, [Macro]) -> (Maybe Int16, [Macro])
siStep _ (Nothing, res) = (Nothing, res)
siStep memoryMap (Just pc, res)
  | let a = fromMaybe 0 $ M.lookup pc memoryMap
        b = fromMaybe 0 $ M.lookup (pc + 1) memoryMap
        c = fromMaybe 0 $ M.lookup (pc + 2) memoryMap
     in a == 0 && b == 0 && c == 0 =
      (Nothing, res)
siStep memoryMap (Just pc, res) =
  let (next, m) = guessInstruction pc memoryMap
      res' = res ++ [m]
   in (next, res')

siFix :: Int16 -> M.Map Int16 Int16 -> (Maybe Int16, [Macro]) -> [Macro]
siFix _ _ (Nothing, m) = m
siFix pc memoryMap state =
  let (o, res) = siStep memoryMap state
      open' = o >>= (\x -> guard (x > pc) >> return x)
   in siFix pc memoryMap (open', res)

saturateInstructions :: Int16 -> M.Map Int16 Int16 -> [Macro]
saturateInstructions pc memoryMap = siFix pc memoryMap (Just pc, [])

disassemble :: Int16 -> M.Map Int16 Int16 -> Assembly
disassemble pc memoryMap = [Section {sName = "da" ++ show pc, location = Just $ fromIntegral pc, directives = map (RawDirective . DMacro) $ saturateInstructions pc memoryMap}]
