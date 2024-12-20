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
    labelPreservingMap,
    nobranch,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

charArray :: String
charArray = " ABCDEFGHIJKLMNOPQRSTUVWXYZ!0123456789abcdefghijklmnopqrstuvwxyz+-"

charValue :: Char -> Int
charValue c = fromJust $ elemIndex c charArray

data Macro
  = MSLQ Directive Directive Directive
  | MMov Directive Directive
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
  | MIn Directive
  | MOut Directive
  | MPrint Directive
  | MDOut Directive
  | MString String
  | MCall Directive
  | MRet
  deriving (Eq)

cur :: Directive
cur = DCur

offset :: [Directive] -> Directive
offset = fromIntegral . length

implem :: (Int -> Directive) -> Directive -> Macro -> ([Directive], Int)
implem _ _ (MSLQ a b c) = ([a, b, c], 0)
implem loc pc (MMov a b) =
  let (p, r) = implem loc (pc + 3) (MAdd a b)
   in (nobranch a a ++ p, r)
implem _ pc (MSTI a b) =
  let p0 = pc + 15
      p1 = pc + 16
      p2 = pc + 22
      i =
        nobranch a (DImm 0)
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
   in (i, 0)
implem _ _ (MLDI a b) =
  let i =
        nobranch b (DImm 0)
          ++ nobranch (DImm 0) (cur + 8)
          ++ nobranch (DImm 0) (DImm 0)
          ++ nobranch a a
          ++ nobranch (DNumber 0) (DImm 0)
          ++ nobranch (DImm 0) a
          ++ nobranch (DImm 0) (DImm 0)
          ++ nobranch (cur - 9) (cur - 10)
   in (i, 0)
implem loc pc (MPush a) =
  let (p1, c1) = implem loc pc (MSTI (DReg RSP) a)
      (p2, c2) = implem loc (pc + offset p1) (MDec (DReg RSP))
   in (p1 ++ p2, max c1 c2)
implem loc pc (MPop a) =
  let (p1, c1) = implem loc pc (MInc (DReg RSP))
      (p2, c2) = implem loc (pc + offset p1) (MLDI a (DReg RSP))
   in (p1 ++ p2, max c1 c2)
implem _ _ (MAdd a b) = (nobranch b (DImm 0) ++ nobranch (DImm 0) a ++ nobranch (DImm 0) (DImm 0), 0)
implem _ _ (MSub a b) = (nobranch b a, 0)
implem loc _ (MMul a b) =
  let x = loc 0
      y = loc 1
      i =
        nobranch x x
          ++ nobranch y y
          ++ nobranch b (DImm 0)
          ++ [DImm 0, x, cur + 16]
          ++ nobranch (DImm 0) (DImm 0)
          ++ nobranch (DImm 1) x
          ++ nobranch a (DImm 0)
          ++ nobranch (DImm 0) y
          ++ [DImm 0, DImm 0, cur - 17]
          ++ nobranch a a
          ++ nobranch y (DImm 0)
          ++ nobranch (DImm 0) a
          ++ nobranch (DImm 0) (DImm 0)
   in (i, 2)
implem loc _ (MDiv a b) =
  let x = loc 0
      i =
        nobranch x x
          ++ nobranch (DImm (-1)) a
          ++ nobranch (DImm (-1)) x
          ++ [b, a, cur + 4]
          ++ [DImm 0, DImm 0, cur - 8]
          ++ nobranch a a
          ++ nobranch x (DImm 0)
          ++ nobranch (DImm 0) a
          ++ nobranch (DImm 0) (DImm 0)
          ++ nobranch (DImm 1) a
   in (i, 1)
implem loc pc (MMod a b) =
  let (p, c) = implem loc (pc + 9) (MAdd a b)
      i =
        nobranch (DImm (-1)) a
          ++ [b, a, cur + 4]
          ++ [DImm 0, DImm 0, cur - 5]
          ++ p
          ++ nobranch (DImm 1) a
   in (i, c)
implem loc pc (MNeg a) =
  let (jmp, cj) = implem loc pc (MJmp (pcT + 1))
      pcT = pc + offset jmp
      (mov, cm) = implem loc (pcT + 6) (MMov a pcT)
   in (jmp ++ [DNumber 0] ++ nobranch pcT pcT ++ nobranch a pcT ++ mov, max cj cm)
implem loc pc (MAnd a b) = implem loc pc (MMul a b)
implem loc pc (MNot a) =
  let (p, c) = implem loc (pc + 3) (MMov a (DImm 1))
      i =
        nobranch a (DImm 1)
          ++ p
          ++ nobranch (DImm 1) (DImm 1)
          ++ nobranch (DImm (-1)) (DImm 1)
   in (i, c)
implem loc pc (MOr a b) =
  let (pa, ca) = implem loc pc (MAdd a b)
      (pb, cb) = implem loc (pc + 3 + offset pa) (MMov a (DImm 1))
      i = pa ++ [DImm 0, a, cur + 4] ++ pb
   in (i, max ca cb)
implem _ _ (MJmp a) = ([DImm 0, DImm 0, a], 0)
implem _ _ (MJLeq a t) = ([DImm 0, a, t], 0)
implem loc pc (MInc a) = implem loc pc (MSub a (DImm (-1)))
implem loc pc (MDec a) = implem loc pc (MSub a (DImm 1))
implem _ pc (MIn a) =
  let i =
        nobranch a a
          ++ nobranch (DImm (-1)) a
          ++ [DReg RIn, a, pc]
          ++ nobranch (DImm 1) a
          ++ nobranch (DReg RIn) (DReg RIn)
          ++ nobranch (DImm (-1)) (DReg RIn)
   in (i, 0)
implem loc pc (MOut a) = implem loc pc (MSub (DReg ROut) a)
implem loc pc (MDOut a) =
  let c = DImm (charValue '0')
      (pa, ca) = implem loc pc (MAdd c a)
      (po, co) = implem loc (pc + offset pa) (MOut c)
      i =
        pa
          ++ po
          ++ nobranch c c
          ++ nobranch (DImm (-charValue '0')) c
   in (i, max ca co)
implem loc pc (MPrint a) =
  let x = pc + 3
      (pl, cl) =
        implemList
          loc
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
   in ( [cur + 3, cur + 2, cur + 2, DNumber 0]
          ++ pl,
        cl
      )
implem _ _ (MString s) = (map DChar s, 0)
implem loc pc (MCall a) =
  let (p, c) = implem loc pc (MPush (pc + offset p + 3))
      (p2, c2) = implem loc (pc + offset p) (MJmp a)
   in (p ++ p2 ++ [cur + 1], max c c2)
implem loc pc MRet =
  let (p, c) = implem loc pc (MPop (pc + offset p + 2))
   in (p ++ [DImm 0, DImm 0, DNumber 0], c)

implemList :: (Int -> Directive) -> Directive -> [Macro] -> ([Directive], Int)
implemList loc pc = snd . foldl (\(pc', (r, cr)) m -> let (ds, cs) = implem loc pc' m in (pc' + DNumber (length ds), (r ++ ds, max cs cr))) (pc, ([], 0))

instance Show Macro where
  show (MSLQ a b c) = "SLQ " ++ show a ++ ", " ++ show b ++ ", " ++ show c
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
  show (MIn a) = "IN " ++ show a
  show (MOut a) = "OUT " ++ show a
  show (MDOut a) = "DOUT " ++ show a
  show (MPrint a) = "PRNT " ++ show a
  show (MString a) = "STR " ++ show a
  show (MCall a) = "CALL " ++ show a
  show MRet = "RET"

nobranch :: Directive -> Directive -> [Directive]
nobranch a b = [a, b, DCur + 1]

data Reg = ROut | RIn | RSP | RGPR Int deriving (Eq)

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
  deriving (Eq)

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

labelPreservingMap :: ([Directive] -> [Directive]) -> [LDirective] -> [LDirective]
labelPreservingMap f xs = concatMap (\(l, ds) -> relabel (l, f ds)) $ lsplits xs
  where
    relabel (l, d : ds) = LabelledDirective l d : map RawDirective ds
    relabel (_, []) = []
    lsplits [] = [("", [])]
    lsplits (RawDirective d : ds) = (\case ((_, h) : t) -> ("", d : h) : t; _ -> undefined) $ lsplits ds
    lsplits (LabelledDirective s d : ds) = ("", []) : (\case ((_, h) : t) -> (s, d : h) : t; _ -> undefined) (lsplits ds)

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