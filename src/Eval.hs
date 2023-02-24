module Eval where
import AST
import Parser

import Text.Megaparsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp)

intOps :: [(String, Integer -> Integer -> Integer)]
intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)]

-- compOps :: Ord a => [(String, a -> a -> Bool)]
compOps :: [(String, Integer -> Integer -> Bool)]
compOps = [ ("<=", (<=))
         , ("<", (<))
         , (">=", (>=))
         , (">", (>))
         , ("==", (==))
         , ("!=", (/=)) ]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]
          
liftIntOp :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

liftCompOp :: (Integer  -> Integer -> Bool) -> Val -> Val -> Val
liftCompOp f (IntVal i1) (IntVal i2) = BoolVal (f i1 i2)
liftCompOp f _           _           = BoolVal False

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False

-- trans (IntVal i) = i

eval :: SExp -> Env -> Val
eval (SInteger i) _ = IntVal i
eval (SBool b) _ = BoolVal b

eval (SCompOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
   in liftCompOp f v1 v2

eval (SIntOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2

eval (SBoolOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op boolOps
   in liftBoolOp f v1 v2