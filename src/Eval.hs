module Eval where
import AST
import Parser

import Text.Megaparsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp)

intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)]

-----------------------------Editing
compOps :: Ord a => [(String, (a -> a -> Bool))]
compOps = [ ("==", (==))
          , ("!=", (/=))
          , ("<", (<))
          , ("<=", (<=))
          , (">", (>))
          , (">=", (>=))]
          --logical op ||, &&, not are in a seperate bool op section
----------------------------------------------------------------------End

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

-----------------------------------------------------Editing
liftCompOp :: (Integer  -> Integer -> Bool) -> Val -> Val -> Val
liftCompOp f (IntVal i1) (IntVal i2) = BoolVal (f i1 i2)
-- liftCompOp f (IntVal i1) (DoubleVal i2) = BoolVal (f i1 i2)
-- liftCompOp f (DoubleVal i1) (IntVal i2) = BoolVal (f i1 i2)
-- liftCompOp f (DoubleVal i1) (DoubleVal i2) = BoolVal (f i1 i2)
-----------------------------------------------------End

-- trans (IntVal i) = i

eval :: SExp -> Env -> Val
eval (SInteger i) _ = IntVal i

---------------------------------Editing
--eval :: SExp -> Env -> Val
-- eval (SDouble i) _ = DoubleVal i
---------------------------------End

eval (SIntOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2

--------------------------------------Editing
eval (SCompOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
   in liftCompOp f v1 v2
--------------------------------------End

