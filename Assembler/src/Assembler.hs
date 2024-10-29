{-# LANGUAGE LambdaCase #-}

module Assembler (assemble, output, parseAssembly) where

import Data.Char
import Data.Char (isLower)
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

charArray :: String
charArray = " ABCDEFGHIJKLMNOPQRSTUVWXYZ!abcdefghijklmnopqrstuvwxyz0123456789"

charValue :: Char -> Int
charValue c = fromJust $ elemIndex c charArray

data Macro
  = MMov Directive Directive
  | MAdd Directive Directive
  | MSub Directive Directive
  | MJmp Directive
  | MInc Directive
  | MDec Directive
  | MOut Directive
  | MString String

implem :: Macro -> [Directive]
implem (MMov a b) = nobranch a a ++ implem (MAdd a b)
implem (MAdd a b) = nobranch b (DImm 0) ++ nobranch (DImm 0) a ++ nobranch (DImm 0) (DImm 0)
implem (MSub a b) = nobranch b a
implem (MJmp a) = [DImm 0, DImm 0, a]
implem (MInc a) = implem (MSub a (DImm (-1)))
implem (MDec a) = implem (MSub a (DImm 1))
implem (MOut a) = implem (MSub DOut a)
implem (MString s) = map DChar s

instance Show Macro where
  show (MMov a b) = "MOV " ++ show a ++ ", " ++ show b
  show (MAdd a b) = "ADD " ++ show a ++ ", " ++ show b
  show (MSub a b) = "SUB " ++ show a ++ ", " ++ show b
  show (MJmp a) = "JMP " ++ show a
  show (MInc a) = "INC " ++ show a
  show (MDec a) = "DEC " ++ show a
  show (MOut a) = "OUT " ++ show a
  show (MString a) = "STR " ++ show a

nobranch :: Directive -> Directive -> [Directive]
nobranch a b = [a, b, DSum DCur (DNumber 1)]

data Directive
  = DNumber Int
  | DImm Int
  | DChar Char
  | DImmChar Char
  | DLabel String
  | DCur
  | DOut
  | DIn
  | DSum Directive Directive
  | DDiff Directive Directive
  | DMul Directive Directive
  | DMacro Macro

instance Show Directive where
  show (DNumber i) = show i
  show (DImm i) = "#" ++ show i
  show (DChar c) = "!" ++ show c
  show (DImmChar c) = "@" ++ show c
  show (DLabel l) = l
  show DCur = "?"
  show DOut = ">"
  show DIn = "<"
  show (DSum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (DDiff a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (DMul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (DMacro m) = show m

data LDirective = LabelledDirective String Directive | RawDirective Directive

instance Show LDirective where
  show (LabelledDirective l d) = l ++ ": " ++ show d
  show (RawDirective d) = show d

data Section = Section
  { name :: String,
    location :: Maybe Int,
    directives :: [LDirective]
  }

instance Show Section where
  show s = "SECTION " ++ name s ++ " [@" ++ maybe "???" show (location s) ++ "]\n" ++ unlines (map (("  " ++) . show) (directives s))

sectionSize :: Section -> Int
sectionSize s = length $ directives s

type Assembly = [Section]

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
   in preSection : resolved

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

toInt :: Directive -> Int
toInt (DNumber i) = i
toInt (DImm _) = undefined
toInt (DChar c) = charValue c
toInt (DImmChar _) = undefined
toInt (DLabel _) = undefined
toInt DCur = undefined
toInt DOut = -1
toInt DIn = -2
toInt (DSum a b) = toInt a + toInt b
toInt (DDiff a b) = toInt a - toInt b
toInt (DMul a b) = toInt a * toInt b
toInt (DMacro _) = undefined

toInts :: [Directive] -> [Int]
toInts = map toInt

resolveMacrosD' :: Directive -> [Directive]
resolveMacrosD' (DMacro m) = implem m
resolveMacrosD' d = [d]

resolveMacrosD :: LDirective -> [LDirective]
resolveMacrosD (LabelledDirective l d) = (\case (x : xs) -> LabelledDirective l x : map RawDirective xs; _ -> []) (resolveMacrosD' d)
resolveMacrosD (RawDirective d) = map RawDirective (resolveMacrosD' d)

resolveMacros :: Assembly -> Assembly
resolveMacros = map (\s -> s {directives = directives s >>= resolveMacrosD})

assemble :: Assembly -> [Int]
assemble = toInts . resolveLabels . squash . placeSections . constSection . resolveMacros

output :: String -> [Int] -> IO ()
output f dat = withFile f WriteMode $ \h -> do
  hPutStrLn h "v3.0 hex bytes plain big-endian"
  hPutStrLn h $ unwords $ map (printf "%04hx") dat

testAssembly :: Assembly
testAssembly =
  [ Section "main" (Just 0) [RawDirective (DNumber (-2)), RawDirective (DNumber (-2)), RawDirective (DLabel "loop")],
    Section "data" Nothing [LabelledDirective "data" $ DChar 'H', RawDirective $ DChar 'E', RawDirective $ DChar 'L', RawDirective $ DChar 'L', RawDirective $ DChar 'O', RawDirective $ DChar '!'],
    Section "loop" Nothing [LabelledDirective "loop" $ DLabel "data", RawDirective DOut, RawDirective (DSum DCur (DNumber 1)), RawDirective $ DImm (-1), RawDirective $ DLabel "loop", RawDirective (DLabel "loop")]
  ]

macroAssembly :: Assembly
macroAssembly =
  [ Section "main" (Just 0) [RawDirective $ DMacro $ MJmp (DLabel "loop")],
    Section "data" Nothing [LabelledDirective "data" $ DMacro (MString "HELLO!")],
    Section "loop" Nothing [LabelledDirective "loop" $ DMacro (MOut (DLabel "data")), RawDirective $ DMacro (MSub (DLabel "loop") (DImm (-1))), RawDirective $ DMacro (MJmp (DLabel "loop"))]
  ]

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
parseMacro = do
  ( do
      _ <- string "MOV"
      a <- parseDirective
      MMov a <$> parseDirective
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
            _ <- string "STR"
            _ <- many (oneOf " \n")
            _ <- char '\"'
            r <- MString <$> some (noneOf "\"\n")
            _ <- char '\"'
            return r
        )

parseDirective :: Parsec Void String Directive
parseDirective = do
  _ <- many (oneOf " \n")
  n <- oneOf "#!@?><(-" <|> digitChar <|> lowerChar
  case n of
    '#' -> DImm . read <$> many (noneOf " \n")
    '!' -> DChar <$> anySingle
    '@' -> DImmChar <$> anySingle
    '?' -> return DCur
    '>' -> return DOut
    '<' -> return DIn
    '(' -> parseCalcDirective <* char ')'
    nn | isDigit nn || nn == '-' -> DNumber . read . (nn :) <$> many (noneOf " \n")
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