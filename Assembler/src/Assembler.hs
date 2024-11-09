{-# LANGUAGE LambdaCase #-}

module Assembler (assemble, output, parseAssembly) where

import Assembly
import Data.Char
import Data.List (elemIndex, findIndex, nub, sortOn)
import Data.Maybe
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

cConst :: Directive -> Maybe Int
cConst (DImm i) = Just i
cConst (DImmChar c) = Just (charValue c)
cConst _ = Nothing

collectConsts :: [LDirective] -> [Int]
collectConsts l = l >>= (\case LabelledDirective _ d -> maybeToList $ cConst d; RawDirective d -> maybeToList $ cConst d)

replaceConst' :: [Int] -> Directive -> Directive
replaceConst' consts (DImm i) = DSum (DLabel "_c") $ DNumber (fromJust $ elemIndex i consts)
replaceConst' consts (DImmChar c) = DSum (DLabel "_c") $ DNumber (fromJust $ elemIndex (charValue c) consts)
replaceConst' consts (DSum a b) = DSum (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DDiff a b) = DDiff (replaceConst' consts a) (replaceConst' consts b)
replaceConst' consts (DMul a b) = DMul (replaceConst' consts a) (replaceConst' consts b)
replaceConst' _ d = d

replaceConst :: [Int] -> LDirective -> LDirective
replaceConst consts (LabelledDirective l d) = LabelledDirective l (replaceConst' consts d)
replaceConst consts (RawDirective d) = RawDirective (replaceConst' consts d)

constReplace :: [Int] -> Section -> Section
constReplace consts s = Section (name s) (location s) (map (replaceConst consts) (directives s))

constSection :: Assembly -> Assembly
constSection a =
  let consts = nub $ a >>= (collectConsts . directives)
      preSection = Section "__consts" Nothing (LabelledDirective "_c" (DNumber $ head consts) : map (RawDirective . DNumber) (tail consts))
      resolved = map (constReplace consts) a
   in if null consts then a else preSection : resolved

firstFit :: [(Int, Int)] -> Int -> Int
firstFit taken len =
  let diffs = zipWith (\(s0, l0) (s1, _) -> s1 - (s0 + l0)) taken (tail taken)
   in if null taken
        then
          0
        else
          if any (>= len) diffs
            then
              uncurry (+) $ (taken !!) $ fromJust $ findIndex (>= len) diffs
            else
              uncurry (+) $ last taken

placeSection :: Assembly -> Section -> Section
placeSection a s =
  let pos = sortOn fst $ mapMaybe (\sec -> location sec >>= \loc -> return (loc, sectionSize sec)) a
      ff = firstFit pos (sectionSize s)
   in Section (name s) (Just ff) (directives s)

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
resolveLabel _ d = d

resolveCur :: Int -> Directive -> Directive
resolveCur i DCur = DNumber i
resolveCur i (DSum a b) = DSum (resolveCur i a) (resolveCur i b)
resolveCur i (DDiff a b) = DDiff (resolveCur i a) (resolveCur i b)
resolveCur i (DMul a b) = DMul (resolveCur i a) (resolveCur i b)
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
regToInt (RGPR i) = -(i + 16)

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

toInts :: [Directive] -> [Int]
toInts = map toInt

resolveMacrosD' :: Directive -> Directive -> [Directive]
resolveMacrosD' pc (DMacro m) = implem pc m
resolveMacrosD' _ d = [d]

resolveMacrosD :: String -> LDirective -> [LDirective]
resolveMacrosD _ (LabelledDirective l d) = (\case (x : xs) -> LabelledDirective l x : map RawDirective xs; _ -> []) (resolveMacrosD' (DLabel l) d)
resolveMacrosD free (RawDirective d) = (\case (x : xs) -> LabelledDirective (show free) x : map RawDirective xs; _ -> []) (resolveMacrosD' (DLabel (show free)) d)

resolveMacros :: Assembly -> Assembly
resolveMacros = map (\s -> s {directives = zip [0 :: Int ..] (directives s) >>= (\(i, d) -> resolveMacrosD ("_" ++ name s ++ show i) d)})

assemble :: Assembly -> [Int]
assemble = toInts . resolveLabels . squash . padSections . placeSections . constSection . resolveMacros

output :: String -> [Int] -> IO ()
output f dat = withFile f WriteMode $ \h -> do
  hPutStrLn h "v3.0 hex bytes plain big-endian"
  hPutStrLn h $ unwords $ map (printf "%04hx") dat

parseCalcDirective :: Parsec Void String Directive
parseCalcDirective = do
  d1 <- parseDirective
  _ <- many (oneOf " \n")
  op <- oneOf "+-*"
  _ <- many (oneOf " \n")
  d2 <- parseDirective
  return $ case op of
    '+' -> DSum d1 d2
    '-' -> DDiff d1 d2
    '*' -> DMul d1 d2
    _ -> undefined

parseMacro :: Parsec Void String Macro
parseMacro =
  ( do
      _ <- string "MOV"
      a <- parseDirective
      MMov a <$> parseDirective
  )
    <|> ( do
            _ <- string "STI"
            a <- parseDirective
            MSTI a <$> parseDirective
        )
    <|> ( do
            _ <- string "LDI"
            a <- parseDirective
            MLDI a <$> parseDirective
        )
    <|> ( do
            _ <- string "PUSH"
            MPush <$> parseDirective
        )
    <|> ( do
            _ <- string "POP"
            MPop <$> parseDirective
        )
    <|> ( do
            _ <- string "ADD"
            a <- parseDirective
            MAdd a <$> parseDirective
        )
    <|> ( do
            _ <- string "SUB"
            a <- parseDirective
            MSub a <$> parseDirective
        )
    <|> ( do
            _ <- string "MUL"
            a <- parseDirective
            MMul a <$> parseDirective
        )
    <|> ( do
            _ <- string "DIV"
            a <- parseDirective
            MDiv a <$> parseDirective
        )
    <|> ( do
            _ <- string "MOD"
            a <- parseDirective
            MMod a <$> parseDirective
        )
    <|> ( do
            _ <- string "AND"
            a <- parseDirective
            MAnd a <$> parseDirective
        )
    <|> ( do
            _ <- string "OR"
            a <- parseDirective
            MOr a <$> parseDirective
        )
    <|> ( do
            _ <- string "NOT"
            MNot <$> parseDirective
        )
    <|> ( do
            _ <- string "JMP"
            MJmp <$> parseDirective
        )
    <|> ( do
            _ <- string "INC"
            MInc <$> parseDirective
        )
    <|> ( do
            _ <- string "DEC"
            MDec <$> parseDirective
        )
    <|> ( do
            _ <- string "OUT"
            MOut <$> parseDirective
        )
    <|> ( do
            _ <- string "DOUT"
            MDOut <$> parseDirective
        )
    <|> ( do
            _ <- string "PRNT"
            MPrint <$> parseDirective
        )
    <|> ( do
            _ <- string "STR"
            _ <- many (oneOf " \n")
            _ <- char '\"'
            r <- MString <$> some (noneOf "\"\n")
            _ <- char '\"'
            return r
        )
    <|> ( do
            _ <- string "CALL"
            MCall <$> parseDirective
        )
    <|> ( do
            _ <- string "RET"
            return MRet
        )

register :: Parsec Void String Reg
register = do
  (string "O" >> return ROut)
    <|> (string "I" >> return RIn)
    <|> (string "SP" >> return RSP)
    <|> (RGPR . read <$> some digitChar)

parseDirective :: Parsec Void String Directive
parseDirective = do
  _ <- many (oneOf " \n")
  n <- oneOf "#!@?><(-" <|> digitChar <|> lowerChar
  case n of
    '#' -> DImm . read <$> many (noneOf " \n")
    '!' -> DChar <$> anySingle
    '@' -> DImmChar <$> anySingle
    '?' -> return DCur
    '$' -> DReg <$> register
    '(' -> parseCalcDirective <* char ')'
    nn | isDigit nn || nn == '-' -> DNumber . read . (nn :) <$> many (noneOf ")( \n")
    nn | isLower nn -> DLabel . (nn :) <$> many (noneOf " \n")
    _ -> fail "no directive"

parseMDirective :: Parsec Void String Directive
parseMDirective = do
  _ <- many (oneOf " \n")
  (DMacro <$> parseMacro) <|> parseDirective

parseStringLDirective :: Parsec Void String LDirective
parseStringLDirective = do
  l <- some lowerChar
  (char ':' *> (LabelledDirective l <$> parseMDirective)) <|> return (RawDirective (DLabel l))

parseLDirective :: Parsec Void String LDirective
parseLDirective = parseStringLDirective <|> RawDirective <$> parseMDirective

parseSection :: Parsec Void String Section
parseSection = do
  _ <- many (oneOf " \n")
  nme <- string "SECTION " *> many (noneOf " [\n") <* optional (char ' ')
  loc <- optional (char '[' *> char '@' *> (read <$> many (noneOf "]\n") <* char ']')) <* char '\n'
  _ <- many (oneOf " \n")
  dirs <- many $ do
    d <- parseLDirective
    _ <- many (oneOf " \n")
    return d
  return $ Section nme loc dirs

parseAssembly :: Parsec Void String Assembly
parseAssembly = many parseSection