module Assembly
  ( Assembly,
    Section (..),
    LDirective (..),
    Directive (..),
    Reg (..),
    Macro (..),
    charValue,
    implem,
    sectionSize,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

charArray :: String
charArray = " ABCDEFGHIJKLMNOPQRSTUVWXYZ!0123456789abcdefghijklmnopqrstuvwxyz+-"

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
  | MNeg Directive
  | MAnd Directive Directive
  | MOr Directive Directive
  | MNot Directive
  | MJmp Directive
  | MJLeq Directive Directive
  | MInc Directive
  | MDec Directive
  | MOut Directive
  | MPrint Directive
  | MDOut Directive
  | MString String
  | MCall Directive
  | MRet

implem :: Directive -> Macro -> [Directive]
implem pc (MMov a b) = nobranch a a ++ implem (pc + 3) (MAdd a b)
implem pc (MSTI a b) =
  let p0 = pc + 15
      p1 = pc + 16
      p2 = pc + 22
   in nobranch a (DImm 0)
        ++ nobranch (DImm 0) p0
        ++ nobranch (DImm 0) p1
        ++ nobranch (DImm 0) p2
        ++ nobranch (DImm 0) (DImm 0)
        ++ nobranch (DNumber 0) (DNumber 0)
        ++ nobranch b (DImm 0)
        ++ nobranch (DImm 0) (DNumber 0)
        ++ nobranch (DImm 0) (DImm 0)
        ++ nobranch p0 p0
        ++ nobranch p1 p1
        ++ nobranch p2 p2
implem _ (MLDI a b) =
  nobranch b (DImm 0)
    ++ nobranch (DImm 0) (DCur + 8)
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch a a
    ++ nobranch (DNumber 0) (DImm 0)
    ++ nobranch (DImm 0) a
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DCur - 9) (DCur - 10)
implem pc (MPush a) =
  let p = implem pc (MSTI (DReg RSP) a)
   in p ++ implem (pc + fromIntegral (length p)) (MDec (DReg RSP))
implem pc (MPop a) = implem pc (MInc (DReg RSP)) ++ implem (pc + 3) (MLDI a (DReg RSP))
implem _ (MAdd a b) = nobranch b (DImm 0) ++ nobranch (DImm 0) a ++ nobranch (DImm 0) (DImm 0)
implem _ (MSub a b) = nobranch b a
implem _ (MMul a b) =
  [DCur + 7, DCur + 6, DCur + 1, DCur + 3, DCur + 2, DCur + 3, 0, 0]
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
    ++ nobranch (DImm (-1)) (DCur - 5)
    ++ [b, a, DCur + 4]
    ++ [DImm 0, DImm 0, DCur - 8]
    ++ nobranch a a
    ++ nobranch (DCur - 16) (DImm 0)
    ++ nobranch (DImm 0) a
    ++ nobranch (DImm 0) (DImm 0)
    ++ nobranch (DImm 1) a
implem pc (MMod a b) =
  nobranch (DImm (-1)) a
    ++ [b, a, DCur + 4]
    ++ [DImm 0, DImm 0, DCur - 5]
    ++ implem (pc + 9) (MAdd a b)
    ++ nobranch (DImm 1) a
implem pc (MNeg a) =
  let jmp = implem pc (MJmp (pcT + 1))
      pcT = pc + fromIntegral (length jmp)
   in jmp ++ [DNumber 0] ++ nobranch pcT pcT ++ nobranch a pcT ++ implem (pcT + 6) (MMov a pcT)
implem pc (MAnd a b) = implem pc (MMul a b)
implem pc (MNot a) =
  nobranch a (DImm 1)
    ++ implem (pc + 3) (MMov a (DImm 1))
    ++ nobranch (DImm 1) (DImm 1)
    ++ nobranch (DImm (-1)) (DImm 1)
implem pc (MOr a b) =
  implem pc (MAdd a b)
    ++ [DImm 0, a, DCur + 4]
    ++ implem (pc + 12) (MMov a (DImm 1))
implem _ (MJmp a) = [DImm 0, DImm 0, a]
implem _ (MJLeq a t) = [DImm 0, a, t]
implem pc (MInc a) = implem pc (MSub a (DImm (-1)))
implem pc (MDec a) = implem pc (MSub a (DImm 1))
implem pc (MOut a) = implem pc (MSub (DReg ROut) a)
implem pc (MDOut a) =
  let c = DImm (charValue '0')
   in implem pc (MAdd c a)
        ++ implem (pc + 9) (MOut c)
        ++ nobranch c c
        ++ nobranch (DImm (-charValue '0')) c
implem pc (MPrint a) =
  let x = pc + 3
   in [DCur + 3, DCur + 2, DCur + 2, DNumber 0]
        ++ implemList
          (pc + 4)
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
implem _ (MString s) = map DChar s
implem pc (MCall a) =
  let p = implem pc (MPush (pc + fromIntegral (3 + length p)))
   in p ++ implem (pc + fromIntegral (length p)) (MJmp a) ++ [DCur + 1]
implem pc MRet =
  let p = implem pc (MPop (pc + fromIntegral (2 + length p)))
   in p ++ [DImm 0, DImm 0, DNumber 0]

implemList :: Directive -> [Macro] -> [Directive]
implemList pc = snd . foldl (\(pc', r) m -> let ds = implem pc' m in (pc' + DNumber (length ds), r ++ ds)) (pc, [])

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
  show (MNeg a) = "NEG " ++ show a
  show (MAnd a b) = "AND " ++ show a ++ ", " ++ show b
  show (MOr a b) = "OR " ++ show a ++ ", " ++ show b
  show (MNot a) = "NOT " ++ show a
  show (MJmp a) = "JMP " ++ show a
  show (MJLeq a l) = "JLEQ " ++ show a ++ ", " ++ show l
  show (MInc a) = "INC " ++ show a
  show (MDec a) = "DEC " ++ show a
  show (MOut a) = "OUT " ++ show a
  show (MDOut a) = "DOUT " ++ show a
  show (MPrint a) = "PRNT " ++ show a
  show (MString a) = "STR " ++ show a
  show (MCall a) = "CALL " ++ show a
  show MRet = "RET"

nobranch :: Directive -> Directive -> [Directive]
nobranch a b = [a, b, DCur + 1]

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

instance Num Directive where
  (+) = DSum
  (-) = DDiff
  (*) = DMul
  abs = undefined
  signum = undefined
  fromInteger = DNumber . fromInteger

data LDirective = LabelledDirective String Directive | RawDirective Directive

instance Show LDirective where
  show (LabelledDirective l d) = l ++ ": " ++ show d
  show (RawDirective d) = show d

data Section = Section
  { sName :: String,
    location :: Maybe Int,
    directives :: [LDirective]
  }

instance Show Section where
  show s = "SECTION " ++ sName s ++ " [@" ++ maybe "???" show (location s) ++ "]\n" ++ unlines (map (("  " ++) . show) (directives s))

sectionSize :: Section -> Int
sectionSize s = length $ directives s

type Assembly = [Section]