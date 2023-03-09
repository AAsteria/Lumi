module Eval where
import AST
import Parser

import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp)

-- Define the evaluation function for SExp
-- eval (SNumericOp op args) = evalNumericOp op args
-- eval x = x

-- Define the evaluation function for NumericOp
-- usage: evalNumericOp Add [SDouble 3.5, SInteger 2]
-- evalNumericOp :: NumericOp -> [SExp] -> SExp
evalNumericOp Add args = foldl add (SInteger 0) args
evalNumericOp Subtract args = foldl subtract' (SInteger 0) args
evalNumericOp Multiply args = foldl multiply (SInteger 1) args
evalNumericOp Divide args = foldl1 divide args

-- Define the helper functions for NumericOp evaluation
add :: SExp -> SExp -> SExp
add (SInteger x) (SInteger y) = SInteger (x + y)
add (SDouble x) (SDouble y) = SDouble (x + y)
add (SInteger x) (SDouble y) = SDouble (fromIntegral x + y)
add (SDouble x) (SInteger y) = SDouble (x + fromIntegral y)

subtract' :: SExp -> SExp -> SExp
subtract' (SInteger x) (SInteger y) = SInteger (x - y)
subtract' (SDouble x) (SDouble y) = SDouble (x - y)
subtract' (SInteger x) (SDouble y) = SDouble (fromIntegral x - y)
subtract' (SDouble x) (SInteger y) = SDouble (x - fromIntegral y)

multiply :: SExp -> SExp -> SExp
multiply (SInteger x) (SInteger y) = SInteger (x * y)
multiply (SDouble x) (SDouble y) = SDouble (x * y)
multiply (SInteger x) (SDouble y) = SDouble (fromIntegral x * y)
multiply (SDouble x) (SInteger y) = SDouble (x * fromIntegral y)

divide :: SExp -> SExp -> SExp
divide (SInteger x) (SInteger y) = SDouble (fromIntegral x / fromIntegral y)
divide (SDouble x) (SDouble y) = SDouble (x / y)
divide (SInteger x) (SDouble y) = SDouble (fromIntegral x / y)
divide (SDouble x) (SInteger y) = SDouble (x / fromIntegral y)

numericOps :: [(String, SExp -> SExp -> SExp)]
numericOps = [ ("+", add)
         , ("-",subtract')
         , ("*",multiply)
         , ("/",divide)]

-- doubleOps :: [(String, Double -> Double -> Double)]
-- doubleOps = [ ("+",(+))
--           , ("-",(-))
--           , ("*",(*))
--           , ("/",(/))]

-- compOps :: [(String, SExp -> SExp -> Bool)]
-- compOps = [ ("<=", (<=))
--          , ("<", (<))
--          , (">=", (>=))
--          , (">", (>))
--          , ("==", (==))
--          , ("!=", (/=)) ]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]
          
-- liftIntOp :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
-- liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
-- liftIntOp f _           _           = IntVal 0

-- liftDoubleOp :: (Double -> Double -> Double) -> Val -> Val -> Val
-- liftDoubleOp f (DoubleVal i1) (DoubleVal i2) = DoubleVal (f i1 i2)
-- liftDoubleOp f _           _           = DoubleVal 0

listNumericOp f (NumericVal i1) (NumericVal i2) = NumericVal (f i1 i2)

liftCompOp f (NumericVal i1) (NumericVal i2) = BoolVal (f i1 i2)
liftCompOp f _           _           = BoolVal False

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False

-- -- trans (Val i) = i

eval :: SExp -> Env -> Val
-- eval (SDouble i) _ = DoubleVal i
-- eval (SInteger i) _ = IntVal i
eval (SNumeric i) _ = NumericVal i
eval (SBool b) _ = BoolVal b

-- eval (SCompOp op e1 e2) env =
--   let v1 = eval e1 env
--       v2 = eval e2 env
--       Just f = lookup op compOps
--    in liftCompOp f v1 v2

eval (SNumericOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op numericOps
   in listNumericOp f v1 v2

-- eval (SDoubleOp op e1 e2) env =
--   let v1 = eval e1 env
--       v2 = eval e2 env
--       Just f = lookup op doubleOps
--    in liftDoubleOp f v1 v2

-- eval (SBoolOp op e1 e2) env =
--   let v1 = eval e1 env
--       v2 = eval e2 env
--       Just f = lookup op boolOps
--    in liftBoolOp f v1 v2