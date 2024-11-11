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

macro :: Parser Macro
macro =
  ( do
      _ <- string "MOV"
      a <- directive
      MMov a <$> directive
  )
    <|> ( do
            _ <- string "STI"
            a <- directive
            MSTI a <$> directive
        )
    <|> ( do
            _ <- string "LDI"
            a <- directive
            MLDI a <$> directive
        )
    <|> ( do
            _ <- string "PUSH"
            MPush <$> directive
        )
    <|> ( do
            _ <- string "POP"
            MPop <$> directive
        )
    <|> ( do
            _ <- string "ADD"
            a <- directive
            MAdd a <$> directive
        )
    <|> ( do
            _ <- string "SUB"
            a <- directive
            MSub a <$> directive
        )
    <|> ( do
            _ <- string "MUL"
            a <- directive
            MMul a <$> directive
        )
    <|> ( do
            _ <- string "DIV"
            a <- directive
            MDiv a <$> directive
        )
    <|> ( do
            _ <- string "MOD"
            a <- directive
            MMod a <$> directive
        )
    <|> ( do
            _ <- string "AND"
            a <- directive
            MAnd a <$> directive
        )
    <|> ( do
            _ <- string "OR"
            a <- directive
            MOr a <$> directive
        )
    <|> ( do
            _ <- string "NOT"
            MNot <$> directive
        )
    <|> ( do
            _ <- string "JMP"
            MJmp <$> directive
        )
    <|> ( do
            _ <- string "INC"
            MInc <$> directive
        )
    <|> ( do
            _ <- string "DEC"
            MDec <$> directive
        )
    <|> ( do
            _ <- string "OUT"
            MOut <$> directive
        )
    <|> ( do
            _ <- string "DOUT"
            MDOut <$> directive
        )
    <|> ( do
            _ <- string "PRNT"
            MPrint <$> directive
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
            MCall <$> directive
        )
    <|> ( do
            _ <- string "RET"
            return MRet
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