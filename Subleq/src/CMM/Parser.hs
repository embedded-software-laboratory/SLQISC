module CMM.Parser (parseCMM) where

import CMM.Language

import Data.Void (Void)
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

cWhitespace :: Parser ()
cWhitespace = do
  _ <- many (oneOf " \n" <|> (string "//" >> many (noneOf "\n") >> char '\n' >> return ' '))
  return ()

declaration :: Parser Declaration
declaration = do
  _ <- string "var"
  cWhitespace
  n <- (:) <$> letterChar <*> many alphaNumChar
  return $ Declaration n

literal :: Parser Expression
literal = do
  n <- read <$> some digitChar
  return $ Literal n

variable :: Parser Expression
variable = do
  n <- (:) <$> letterChar <*> many alphaNumChar
  return $ Variable n

uop :: Parser UOp
uop = do
  op <- string "~" <|> string "!" <|> string "^" <|> string "&"
  return $ case op of
    "~" -> Neg
    "!" -> Not
    "^" -> Deref
    "&" -> Addr

unary :: Parser Expression
unary = do
  op <- uop
  Unary op <$> expression

bop :: Parser BOp
bop = do
  op <- string "+" <|> string "-" <|> string "*" <|> string "/=" <|> string "/" <|> string "%" <|> string "&&" <|> string "||" <|> string "=="<|> string "<=" <|> string ">=" <|> string "<" <|> string ">"
  return $ case op of
    "+" -> Add
    "-" -> Sub
    "*" -> Mul
    "/" -> Div
    "%" -> Mod
    "&&" -> And
    "||" -> Or
    "==" -> Eq
    "/=" -> Neq
    "<" -> Lt
    ">" -> Gt
    "<=" -> Le
    ">=" -> Ge

binary :: Parser Expression
binary = do
  op <- bop
  l <- expression
  Binary op l <$> expression

expression :: Parser Expression
expression = do
  cWhitespace
  literal <|> variable <|> unary <|> binary

assignment :: Parser Statement
assignment = do
  n <- (:) <$> letterChar <*> many alphaNumChar
  cWhitespace
  _ <- string ":="
  Assign n <$> expression

ifStatement :: Parser Statement
ifStatement = do
  _ <- string "if"
  cWhitespace
  e <- expression
  cWhitespace
  _ <- string "then"
  cWhitespace
  s1 <- statement
  cWhitespace
  _ <- string "else"
  cWhitespace
  If e s1 <$> statement

whileStatement :: Parser Statement
whileStatement = do
  _ <- string "while"
  cWhitespace
  e <- expression
  cWhitespace
  _ <- string "do"
  cWhitespace
  While e <$> statement

returnStatement :: Parser Statement
returnStatement = do
  _ <- string "return"
  cWhitespace
  Return <$> expression

block :: Parser Statement
block = do
  _ <- string "{"
  cWhitespace
  ss <- statement `sepBy` string ";"
  cWhitespace
  _ <- string "}"
  return $ Block ss

call :: Parser Statement
call = do
  _ <- string "call"
  cWhitespace
  n <- (:) <$> letterChar <*> many alphaNumChar
  _ <- string "("
  cWhitespace
  es <- expression `sepBy` (cWhitespace >> string "," >> cWhitespace)
  cWhitespace
  _ <- string ")"
  cWhitespace
  _ <- string "->"
  cWhitespace
  t <- (:) <$> letterChar <*> many alphaNumChar
  cWhitespace
  return $ Call t n es

out :: Parser Statement
out = do
  _ <- string "out"
  cWhitespace
  Out <$> expression

trap :: Parser Statement
trap = do
  _ <- string "trap"
  return Trap

dbreak :: Parser Statement
dbreak = do
  _ <- string "dbreak"
  return Break

in' :: Parser Statement
in' = do
  _ <- string "in"
  cWhitespace
  n <- (:) <$> letterChar <*> many alphaNumChar
  cWhitespace
  return $ In n

statement :: Parser Statement
statement = do
  cWhitespace
  ifStatement <|> whileStatement <|> returnStatement <|> call <|> trap <|> dbreak <|> out <|> in' <|> block <|> assignment

function :: Parser Function
function = do
  _ <- string "def"
  cWhitespace
  n <- (:) <$> letterChar <*> many alphaNumChar
  cWhitespace
  _ <- string "("
  cWhitespace
  ps <- declaration `sepBy` (cWhitespace >> string "," >> cWhitespace)
  cWhitespace
  _ <- string ")"
  cWhitespace
  _ <- string "["
  cWhitespace
  ls <- declaration `sepBy` (cWhitespace >> string "," >> cWhitespace)
  cWhitespace
  _ <- string "]"
  s <- statement
  cWhitespace
  return $ Function n ps ls s

program :: Parser Program
program = do
  cWhitespace
  dfs <- ((Left <$> declaration) <|> (Right <$> function)) `sepBy` cWhitespace
  return $ Program (lefts dfs) (rights dfs)

parseCMM :: String -> String -> Either String Program
parseCMM file code = case parse program file code of
  Left err -> Left $ errorBundlePretty err
  Right a -> Right a
