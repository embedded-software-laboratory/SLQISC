module Parser (parseAssembly) where

import Assembly
import Data.Char
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

calcDirective :: Parser Directive
calcDirective = do
  d1 <- directive
  _ <- many (oneOf " \n")
  op <- oneOf "+-*"
  _ <- many (oneOf " \n")
  d2 <- directive
  return $ case op of
    '+' -> DSum d1 d2
    '-' -> DDiff d1 d2
    '*' -> DMul d1 d2
    _ -> undefined

macroNil :: String -> Macro -> Parser Macro
macroNil name cons = do
  _ <- string name
  return cons

macroUn :: String -> (Directive -> Macro) -> Parser Macro
macroUn name cons = do
  _ <- string name
  cons <$> directive

macroBin :: String -> (Directive -> Directive -> Macro) -> Parser Macro
macroBin name cons = do
  _ <- string name
  cons <$> directive <*> directive

macro :: Parser Macro
macro =
  macroBin "MOV" MMov
    <|> macroBin "STI" MSTI
    <|> macroBin "LDI" MLDI
    <|> macroBin "LDI" MLDI
    <|> macroUn "PUSH" MPush
    <|> macroUn "POP" MPop
    <|> macroBin "ADD" MAdd
    <|> macroBin "SUB" MSub
    <|> macroBin "MUL" MMul
    <|> macroBin "DIV" MDiv
    <|> macroBin "MOD" MMod
    <|> macroBin "AND" MAnd
    <|> macroBin "OR" MOr
    <|> macroUn "NOT" MNot
    <|> macroUn "JMP" MJmp
    <|> macroBin "JLEQ" MJLeq
    <|> macroUn "INC" MInc
    <|> macroUn "DEC" MDec
    <|> macroUn "OUT" MOut
    <|> macroUn "DOUT" MDOut
    <|> macroUn "PRNT" MPrint
    <|> macroUn "CALL" MCall
    <|> macroNil "RET" MRet
    <|> ( do
            _ <- string "STR"
            _ <- many (oneOf " \n")
            _ <- char '\"'
            r <- MString <$> some (noneOf "\"\n")
            _ <- char '\"'
            return r
        )

register :: Parser Reg
register = do
  (string "O" >> return ROut)
    <|> (string "I" >> return RIn)
    <|> (string "SP" >> return RSP)
    <|> (RGPR . read <$> some digitChar)

directive :: Parser Directive
directive = do
  _ <- many (oneOf " \n")
  n <- oneOf "#!@?><(-" <|> digitChar <|> lowerChar
  case n of
    '#' -> DImm . read <$> many (noneOf " \n")
    '!' -> DChar <$> anySingle
    '@' -> DImmChar <$> anySingle
    '?' -> return DCur
    '$' -> DReg <$> register
    '(' -> calcDirective <* char ')'
    nn | isDigit nn || nn == '-' -> DNumber . read . (nn :) <$> many (noneOf ")( \n")
    nn | isLower nn -> DLabel . (nn :) <$> many (noneOf " \n")
    _ -> fail "no directive"

mDirective :: Parser Directive
mDirective = do
  _ <- many (oneOf " \n")
  (DMacro <$> macro) <|> directive

stringLDirective :: Parser LDirective
stringLDirective = do
  l <- some lowerChar
  (char ':' *> (LabelledDirective l <$> mDirective)) <|> return (RawDirective (DLabel l))

lDirective :: Parser LDirective
lDirective = stringLDirective <|> RawDirective <$> mDirective

section :: Parser Section
section = do
  _ <- many (oneOf " \n")
  nme <- string "SECTION " *> many (noneOf " [\n") <* optional (char ' ')
  loc <- optional (char '[' *> char '@' *> (read <$> many (noneOf "]\n") <* char ']')) <* char '\n'
  _ <- many (oneOf " \n")
  dirs <- many $ do
    d <- lDirective
    _ <- many (oneOf " \n")
    return d
  return $ Section nme loc dirs

assembly :: Parser Assembly
assembly = many section

parseAssembly :: String -> String -> Either String Assembly
parseAssembly file code = case parse assembly file code of
  Left err -> Left $ errorBundlePretty err
  Right a -> Right a