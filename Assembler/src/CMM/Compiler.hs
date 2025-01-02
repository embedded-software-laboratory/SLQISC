module CMM.Compiler (compile) where

import CMM.Language
import Assembly
import Data.Map.Strict qualified as M

compileGlobals :: [Declaration] -> (Section,M.Map String Directive)
compileGlobals ds =
  let glabel = DLabel "__globals"
      gDirs = M.fromList $ zipWith (\i (Declaration v) -> (v, glabel + fromInteger i)) [0..] ds
      gSec = Section "__globals" Nothing $ LabelledDirective "__globals" (DNumber 0) : replicate (length ds - 1) (RawDirective (DNumber 0))
  in if null ds then (Section "__globals" Nothing [], M.empty) else (gSec, gDirs)

expRegister :: Int -> Directive
expRegister i = DReg $ RGPR (i+16)

compileUnary :: Int -> UOp -> [Directive]
compileUnary i Neg = [DMacro $ MNeg (expRegister i)]
compileUnary i Not = [DMacro $ MNot (expRegister i)]
compileUnary _ _ = undefined

compileBinary :: Int -> BOp -> [Directive]
compileBinary i Add = [DMacro $ MAdd (expRegister i) (expRegister (i+1))]
compileBinary i Sub = [DMacro $ MSub (expRegister i) (expRegister (i+1))]
compileBinary i Mul = [DMacro $ MMul (expRegister i) (expRegister (i+1))]
compileBinary i Div = [DMacro $ MDiv (expRegister i) (expRegister (i+1))]
compileBinary i Mod = [DMacro $ MMod (expRegister i) (expRegister (i+1))]
compileBinary i And = [DMacro $ MAnd (expRegister i) (expRegister (i+1))]
compileBinary i Or = [DMacro $ MOr (expRegister i) (expRegister (i+1))]
compileBinary i Gt = [DMacro $ MSub (expRegister i) (expRegister (i+1)), DMacro $ MDiv (expRegister i) (expRegister i)]
compileBinary i Neq = [DMacro $ MSub (expRegister i) (expRegister (i+1)), DMacro $ MMul (expRegister i) (expRegister i), DMacro $ MDiv (expRegister i) (expRegister i)]

compileExpression' :: M.Map String Directive -> Int -> Expression -> [Directive]
compileExpression' _ i (Literal z) = [DMacro $ MMov (expRegister i) (DImm z)]
compileExpression' a i (Variable x) = [DMacro $ MMov (expRegister i) (a M.! x)]
compileExpression' a i (Unary op e) = compileExpression' a i e ++ compileUnary i op
compileExpression' a i (Binary op l r) = compileExpression' a i l ++ compileExpression' a (i+1) r ++ compileBinary i op

compileExpression :: M.Map String Directive -> Expression -> [Directive]
compileExpression a = compileExpression' a 0

compileStatement :: Int -> M.Map String Directive -> Statement -> ([LDirective],Int)
compileStatement free allocs (Assign n e) =
  let eDirs = map RawDirective $ compileExpression allocs e
  in (eDirs ++ [RawDirective (DMacro $ MMov (allocs M.! n) (expRegister 0))],free)
compileStatement free allocs (If e s1 s2) =
  let eDirs = map RawDirective $ compileExpression allocs e
      (s1Dirs,f1) = compileStatement free allocs s1
      (s2Dirs,f2) = compileStatement f1 allocs s2
      dirs = eDirs ++ [RawDirective (DMacro $ MJLeq (expRegister 0) (DLabel ("_else" ++ show f2)))] ++ s1Dirs ++ [RawDirective (DMacro $ MJmp (DLabel ("_ifend" ++ show f2))),LabelledDirective ("_else" ++ show f2) (DMacro MNop)] ++ s2Dirs ++ [LabelledDirective ("_ifend" ++ show f2) (DMacro MNop)]
  in (dirs,f2+1)
compileStatement free allocs (While e s) =
  let eDirs = map RawDirective $ compileExpression allocs e
      (sDirs,f) = compileStatement free allocs s
      dirs = [LabelledDirective ("_while" ++ show f) (DMacro MNop)] ++ eDirs ++ [RawDirective (DMacro $ MJLeq (expRegister 0) (DLabel ("_wend" ++ show f)))] ++ sDirs ++ [RawDirective (DMacro $ MJmp (DLabel ("_while" ++ show f))),LabelledDirective ("_wend" ++ show f) (DMacro MNop)]
      in (dirs,f+1)
compileStatement free allocs (Return e) = (map RawDirective (compileExpression allocs e) ++ [RawDirective (DMacro (MMov (DReg (RGPR 0)) (expRegister 0))),RawDirective (DMacro MRet)],free)
compileStatement free allocs (Block ss) =
  let (dirs,f) = foldl (\(ds,fr) s -> let (sDs,sFr) = compileStatement fr allocs s in (ds ++ sDs,sFr)) ([],free) ss
  in (dirs,f)
compileStatement free allocs (Call t n es) =
  let eDirs = concat $ zipWith (\i e -> map RawDirective (compileExpression allocs e) ++ [RawDirective (DMacro $ MMov (DReg $ RGPR (i+4)) (expRegister 0))]) [0..] es
  in (eDirs ++ [RawDirective (DMacro $ MCall (DLabel n)),RawDirective (DMacro $ MMov (allocs M.! t) (DReg $ RGPR 0))],free)
compileStatement free allocs (Out e) =
  let eDirs = map RawDirective $ compileExpression allocs e
  in (eDirs ++ [RawDirective (DMacro $ MOut (expRegister 0))],free)
compileStatement free allocs (In n) = ([RawDirective (DMacro $ MIn (allocs M.! n))],free)

functionAllocations :: M.Map String Directive -> [Declaration] -> [Declaration] -> M.Map String Directive
functionAllocations globals params locals =
  let paramMap = M.fromList $ zipWith (\(Declaration n) i -> (n,DReg (RGPR (i+4)))) params [0..]
      locMap = M.fromList $ zipWith (\(Declaration n) i -> (n,DReg (RGPR (i+8)))) locals [0..]
  in M.unions [globals,paramMap,locMap]

compileFunction :: Int -> M.Map String Directive -> Function -> (Section,Int)
compileFunction free globals (Function n params locals body) =
  let (cBody,fB) = compileStatement free (functionAllocations globals params locals) body
      entry = map RawDirective $ zipWith (((\i -> DMacro (MPush (DReg (RGPR (i + 8))))).) . const) [0..] locals
      exit = map RawDirective $ reverse $ zipWith (((\i -> DMacro (MPop (DReg (RGPR (i + 8))))).) . const) [0..] locals
      loc = if n == "main" then Just 0 else Nothing
      fSec = Section n loc (LabelledDirective n (DMacro MNop) : if n == "main" then cBody else entry ++ cBody ++ exit)
  in (fSec,fB)

compile :: Program -> Assembly
compile (Program ds fs) =
  let (gSec, globals) = compileGlobals ds
      fSecs = fst $ foldl (\(ss,fr) f -> let (fsec,fFr) = compileFunction fr globals f in (ss ++ [fsec],fFr)) ([],0) fs
  in gSec : fSecs