module Eval where
import AST
import Parser

import Text.Megaparsec
import System.Console.Haskeline

intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)]

liftIntOp f (SInteger i1) (SInteger i2) = SInteger (f i1 i2)
liftIntOp f _           _           = SInteger 0

eval :: SExp -> Env -> SExp
eval (SInteger i) _ = SInteger i

eval (SIntOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2