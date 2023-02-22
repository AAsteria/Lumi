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

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

trans (IntVal i) = i

eval :: SExp -> Env -> Val
eval (SInteger i) _ = IntVal i

eval (SIntOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2