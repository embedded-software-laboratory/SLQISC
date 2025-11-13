module CMM.Language where


import Data.List

newtype Declaration = Declaration String

data UOp = Neg | Not | Deref | Addr

data BOp = Add | Sub | Mul | Div | Mod | And | Or | Eq | Neq | Lt | Gt | Le | Ge

data Expression = Literal Int | Variable String | Unary UOp Expression | Binary BOp Expression Expression

data Statement = Assign String Expression | If Expression Statement Statement | While Expression Statement | Return Expression | Block [Statement] | Call String String [Expression] | Out Expression | In String

data Function = Function String [Declaration] [Declaration] Statement

data Program = Program [Declaration] [Function]

instance Show Declaration where
  show (Declaration n) = "var " ++ n

instance Show UOp where
  show Neg = "-"
  show Not = "!"
  show Deref = "*"
  show Addr = "&"

instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or = "||"
  show Eq = "=="
  show Neq = "!="
  show Lt = "<"
  show Gt = ">"
  show Le = "<="
  show Ge = ">="

instance Show Expression where
  show (Literal i) = show i
  show (Variable n) = n
  show (Unary op e) = show op ++ show e
  show (Binary op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"

instance Show Statement where
  show (Assign n e) = n ++ " := " ++ show e
  show (If e s1 s2) = "if (" ++ show e ++ ") " ++ show s1 ++ " else " ++ show s2
  show (While e s) = "while (" ++ show e ++ ") " ++ show s
  show (Return e) = "return " ++ show e ++ ";"
  show (Block ss) = "{\n" ++ intercalate "\n" (map show ss) ++ "\n}"
  show (Call t n es) = t ++ " := " ++ n ++ "(" ++ intercalate ", " (map show es) ++ ")"
  show (Out e) = "out " ++ show e
  show (In n) = "in " ++ n

instance Show Function where
  show (Function n ps ls s) = "def " ++ n ++ "(" ++ intercalate ", " (map show ps) ++ ") {\n" ++ intercalate "\n" (map show ls) ++ "\n" ++ show s ++ "\n}"

instance Show Program where
  show (Program ds fs) = intercalate "\n" (map show ds) ++ "\n" ++ intercalate "\n" (map show fs)

