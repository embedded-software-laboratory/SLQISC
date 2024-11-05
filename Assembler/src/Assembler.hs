{-# LANGUAGE LambdaCase #-}

module Assembler (assemble, output, parseAssembly) where

import Data.Char
import Data.List (elemIndex, findIndex, nub, sortOn)
import Data.Maybe
import Data.Void
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf

charArray :: String
charArray = " ABCDEFGHIJKLMNOPQRSTUVWXYZ!0123456789abcdefghijklmnopqrstuvwxyz"

charValue :: Char -> Int
charValue c = fromJust $ elemIndex c charArray

data Macro
  = MMov Directive Directive
  | MSTI Directive Directive
  | MLDI Directive Directive
  | MPush Directive
  | MPop Directive
  | MAdd Directive Directive
  | MSub Directive Directive
  | MMul Directive Directive
  | MDiv Directive Directive
  | MMod Directive Directive
  | MAnd Directive Directive
  | MOr Directive Directive
  | MNot Directive
  | MJmp Directive
  | MInc Directive
  | MDec Directive
  | MOut Directive
  | MPrint Directive
  | MDOut Directive
  | MString String

implem :: Directive -> Macro -> [Directive]
implem pc (MMov a b) = nobranch a a ++ implem (DSum pc (DNumber 3)) (MAdd a b)
implem _ (MSTI a b) =
  nobranch a (DImm 0)
    ++ nobranch (DImm 0) (DSum DCur (DNumber 9))
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch b (DImm 0)
    ++ nobranch (DImm 0) (DNumber 0)
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DDiff DCur (DNumber 5)) (DDiff DCur (DNumber 6))
implem _ (MLDI a b) =
  nobranch b (DImm 0)
    ++ nobranch (DImm 0) (DSum DCur (DNumber 5))
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DNumber 0) (DImm 0)
    ++ nobranch (DImm 0) a
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DDiff DCur (DNumber 9)) (DDiff DCur (DNumber 10))
implem pc (MPush a) = implem pc (MSTI (DReg RSP) a) ++ implem (DSum pc (DNumber 21)) (MDec (DReg RSP))
implem pc (MPop a) = implem pc (MInc (DReg RSP)) ++ implem (DSum pc (DNumber 3)) (MLDI (DReg RSP) a)
implem _ (MAdd a b) = nobranch b (DImm 0) ++ nobranch (DImm 0) a ++ nobranch (DImm 0) (DImm 0)
implem _ (MSub a b) = nobranch b a
implem _ (MMul a b) =
  [DSum DCur (DNumber 7), DSum DCur (DNumber 6), DSum DCur (DNumber 1), DSum DCur (DNumber 3), DSum DCur (DNumber 2), DSum DCur (DNumber 3), DNumber 0, DNumber 0]
    ++ nobranch b (DImm 0)
    ++ [DImm 0, DDiff DCur (DNumber 6), DSum DCur (DNumber 16)]
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DImm 1) (DDiff DCur (DNumber 12))
    ++ nobranch a (DImm 0)
    ++ nobranch (DImm 0) (DDiff DCur (DNumber 17))
    ++ [DImm 0, DImm 0, DDiff DCur (DNumber 17)]
    ++ nobranch a a
    ++ nobranch (DDiff DCur (DNumber 25)) (DImm 0)
    ++ nobranch (DImm 0) a
    ++ nobranch (DImm 0) (DImm 0)
implem _ (MDiv a b) =
  [DSum DCur (DNumber 3), DSum DCur (DNumber 2), DSum DCur (DNumber 2), DNumber 0]
    ++ nobranch (DImm (-1)) a
    ++ nobranch (DImm (-1)) (DDiff DCur (DNumber 5))
    ++ [b, a, DSum DCur (DNumber 4)]
    ++ [DImm 0, DImm 0, DDiff DCur (DNumber 8)]
    ++ nobranch a a
    ++ nobranch (DDiff DCur (DNumber 16)) (DImm 0)
    ++ nobranch (DImm 0) a
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DImm 1) a
implem pc (MMod a b) =
  nobranch (DImm (-1)) a
    ++ [b, a, DSum DCur (DNumber 4)]
    ++ [DImm 0, DImm 0, DDiff DCur (DNumber 5)]
    ++ implem (DSum pc (DNumber 9)) (MAdd a b)
    ++ nobranch (DImm 1) a
implem pc (MAnd a b) = implem pc (MMul a b)
implem pc (MNot a) =
  nobranch a (DImm 1)
    ++ implem (DSum pc (DNumber 3)) (MMov a (DImm 1))
    ++ nobranch (DImm 1) (DImm 1)
    ++ nobranch (DImm (-1)) (DImm 1)
implem pc (MOr a b) =
  implem pc (MAdd a b)
    ++ [DImm 1, a, DSum DCur (DNumber 4)]
    ++ implem (DSum pc (DNumber 12)) (MMov a (DImm 1))
implem _ (MJmp a) = [DImm 0, DImm 0, a]
implem pc (MInc a) = implem pc (MSub a (DImm (-1)))
implem pc (MDec a) = implem pc (MSub a (DImm 1))
implem pc (MOut a) = implem pc (MSub (DReg ROut) a)
implem pc (MDOut a) =
  let c = DImm (charValue '0')
   in implem pc (MAdd c a)
        ++ implem (DSum pc (DNumber 9)) (MOut c)
        ++ nobranch c c
        ++ nobranch (DImm (-charValue '0')) c
implem pc (MPrint a) =
  let x = DSum pc (DNumber 3)
   in [DSum DCur (DNumber 3), DSum DCur (DNumber 2), DSum DCur (DNumber 2), DNumber 0]
        ++ ( zip
               (map (DSum pc . DNumber) [4, 7 ..])
               [ MMov x a,
                 MDiv x (DImm 10000),
                 MDOut x,
                 MMov x a,
                 MDiv x (DImm 1000),
                 MMod x (DImm 10),
                 MDOut x,
                 MMov x a,
                 MDiv x (DImm 100),
                 MMod x (DImm 10),
                 MDOut x,
                 MMov x a,
                 MDiv x (DImm 10),
                 MMod x (DImm 10),
                 MDOut x,
                 MMov x a,
                 MMod x (DImm 10),
                 MDOut x
               ]
               >>= uncurry implem
           )
implem _ (MString s) = map DChar s

instance Show Macro where
  show (MMov a b) = "MOV " ++ show a ++ ", " ++ show b
  show (MSTI a b) = "STI " ++ show a ++ ", " ++ show b
  show (MLDI a b) = "LDI " ++ show a ++ ", " ++ show b
  show (MPush a) = "PUSH " ++ show a
  show (MPop a) = "POP " ++ show a
  show (MAdd a b) = "ADD " ++ show a ++ ", " ++ show b
  show (MSub a b) = "SUB " ++ show a ++ ", " ++ show b
  show (MMul a b) = "MUL " ++ show a ++ ", " ++ show b
  show (MDiv a b) = "DIV " ++ show a ++ ", " ++ show b
  show (MMod a b) = "MOD " ++ show a ++ ", " ++ show b
  show (MAnd a b) = "AND " ++ show a ++ ", " ++ show b
  show (MOr a b) = "OR " ++ show a ++ ", " ++ show b
  show (MNot a) = "NOT " ++ show a
  show (MJmp a) = "JMP " ++ show a
  show (MInc a) = "INC " ++ show a
  show (MDec a) = "DEC " ++ show a
  show (MOut a) = "OUT " ++ show a
  show (MDOut a) = "DOUT " ++ show a
  show (MPrint a) = "PRNT " ++ show a
  show (MString a) = "STR " ++ show a

nobranch :: Directive -> Directive -> [Directive]
nobranch a b = [a, b, DSum DCur (DNumber 1)]

data Reg = ROut | RIn | RSP | RGPR Int

instance Show Reg where
  show ROut = "O"
  show RIn = "I"
  show RSP = "SP"
  show (RGPR i) = show i

data Directive
  = DNumber Int
  | DImm Int
  | DChar Char
  | DImmChar Char
  | DLabel String
  | DCur
  | DSum Directive Directive
  | DDiff Directive Directive
  | DMul Directive Directive
  | DMacro Macro
  | DReg Reg

instance Show Directive where
  show (DNumber i) = show i
  show (DImm i) = "#" ++ show i
  show (DChar c) = "!" ++ show c
  show (DImmChar c) = "@" ++ show c
  show (DLabel l) = l
  show DCur = "?"
  show (DSum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (DDiff a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (DMul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (DMacro m) = show m
  show (DReg m) = "$" ++ show m

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
assemble = toInts . resolveLabels . squash . placeSections . constSection . resolveMacros

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