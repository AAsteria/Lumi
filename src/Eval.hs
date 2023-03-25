{-# LANGUAGE GADTs #-}

module Eval where
import AST
import Parser
import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp, Lit (IntegerL))
import Data.Typeable

-- Evaluation function for NumericOp
evalNumericOp :: NumericOp -> [SExp a] -> SExp a
evalNumericOp Add args = foldl add (SInteger 0) args
evalNumericOp Subtract args = foldl subtract' (SInteger 0) args
evalNumericOp Multiply args = foldl multiply (SInteger 1) args
evalNumericOp Divide args = foldl1 divide args

-- Define the helper functions for NumericOp evaluation
add :: SExp a -> SExp a -> SExp a
add (SInteger x) (SInteger y) = SInteger (x + y)
add (SDouble x) (SDouble y) = SDouble (x + y)
add (SInteger x) (SDouble y) = SDouble (fromIntegral x + y)
add (SDouble x) (SInteger y) = SDouble (x + fromIntegral y)
add _ _ = error "Type error: arguments of the wrong type for '+'"

subtract' :: SExp a -> SExp a -> SExp a
subtract' (SInteger x) (SInteger y) = SInteger (x - y)
subtract' (SDouble x) (SDouble y) = SDouble (x - y)
subtract' (SInteger x) (SDouble y) = SDouble (fromIntegral x - y)
subtract' (SDouble x) (SInteger y) = SDouble (x - fromIntegral y)
subtract' _ _ = error "Type error: arguments of the wrong type for '-'"

multiply :: SExp a -> SExp a -> SExp a
multiply (SInteger x) (SInteger y) = SInteger (x * y)
multiply (SDouble x) (SDouble y) = SDouble (x * y)
multiply (SInteger x) (SDouble y) = SDouble (fromIntegral x * y)
multiply (SDouble x) (SInteger y) = SDouble (x * fromIntegral y)
multiply _ _ = error "Type error: arguments of the wrong type for '*'"

divide :: SExp a -> SExp a -> SExp a
divide (SInteger x) (SInteger y) = SDouble (fromIntegral x / fromIntegral y)
divide (SDouble x) (SDouble y) = SDouble (x / y)
divide (SInteger x) (SDouble y) = SDouble (fromIntegral x / y)
divide (SDouble x) (SInteger y) = SDouble (x / fromIntegral y)
divide _ _ = error "Type error: arguments of the wrong type for '/'"

numericOps :: [(NumericOp, SExp a -> SExp a -> SExp a)]
numericOps = [ (Add, add)
             , (Subtract, subtract')
             , (Multiply, multiply)
             , (Divide, divide)
             ]

-- Evaluation function for CompOp
evalCompOp :: (Ord a, Num a) => CompOp -> SExp a -> SExp a -> Bool
evalCompOp LessThan x y = lessThan x y
evalCompOp GreaterThan x y = greaterThan x y
evalCompOp LessThanOrEqual x y = lessThanOrEqual x y
evalCompOp GreaterThanOrEqual x y = greaterThanOrEqual x y
evalCompOp Equal x y = equal x y
evalCompOp NotEqual x y = notEqual x y

lessThan :: SExp a -> SExp a -> Bool
lessThan (SInteger x) (SInteger y) = x < y
lessThan (SDouble x) (SDouble y) = x < y
lessThan (SInteger x) (SDouble y) = fromIntegral x - y < 0.0
lessThan (SDouble x) (SInteger y) = x - fromIntegral y < 0.0
lessThan _ _ = False

greaterThan :: SExp a -> SExp a -> Bool
greaterThan (SInteger x) (SInteger y) = x > y
greaterThan (SDouble x) (SDouble y) = x > y
greaterThan (SInteger x) (SDouble y) = fromIntegral x - y > 0.0
greaterThan (SDouble x) (SInteger y) = x - fromIntegral y > 0.0
greaterThan _ _ = False

lessThanOrEqual :: SExp a -> SExp a -> Bool
lessThanOrEqual (SInteger x) (SInteger y) = x <= y
lessThanOrEqual (SDouble x) (SDouble y) = x <= y
lessThanOrEqual (SInteger x) (SDouble y) = fromIntegral x - y <= 0.0
lessThanOrEqual (SDouble x) (SInteger y) = x - fromIntegral y <= 0.0
lessThanOrEqual _ _ = False

greaterThanOrEqual :: SExp a -> SExp a -> Bool
greaterThanOrEqual (SInteger x) (SInteger y) = x >= y
greaterThanOrEqual (SDouble x) (SDouble y) = x >= y
greaterThanOrEqual (SInteger x) (SDouble y) = fromIntegral x - y >= 0.0
greaterThanOrEqual (SDouble x) (SInteger y) = x - fromIntegral y >= 0.0
greaterThanOrEqual _ _ = False

equal :: SExp a -> SExp a -> Bool
equal (SInteger x) (SInteger y)= x == y
equal (SDouble x) (SDouble y) = x == y
equal (SInteger x) (SDouble y) = fromIntegral x - y == 0
equal (SDouble x) (SInteger y) = x - fromIntegral y == 0
equal _ _ = False

notEqual :: SExp a -> SExp a -> Bool
notEqual (SInteger x) (SInteger y) = x /= y
notEqual (SDouble x) (SDouble y) = x /= y
notEqual (SInteger x) (SDouble y) = fromIntegral x - y /= 0
notEqual (SDouble x) (SInteger y) = x - fromIntegral y /= 0
notEqual _ _ = False

compOps :: [(CompOp, SExp a -> SExp a -> Bool)]
compOps = [(LessThan, lessThan)
         , (GreaterThan, greaterThan)
         , (LessThanOrEqual, lessThanOrEqual)
         , (GreaterThanOrEqual, greaterThanOrEqual)
         , (Equal, equal)
         , (NotEqual, notEqual)]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False

-- trans (Val i) = i

-- Simple Eval Transition Station
eval :: SExp a -> Env -> Val
eval (SNumeric i) _ = NumericVal i
eval (SBool b) _ = BoolVal b

-- TODO: Modify the following two evaluation functions
-- Now only parser works!
-- eval (SNumericOp op e1 e2) env =
--   let v1 = eval e1 env
--       v2 = eval e2 env
--       Just f = lookup op numericOps
--    in liftNumericOp f v1 v2

-- eval (SCompOp op e1 e2) env =
--   let v1 = eval e1 env
--       v2 = eval e2 env
--       Just f = lookup op compOps
--    in liftCompOp f v1 v2

eval (SBoolOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op boolOps
   in liftBoolOp f v1 v2

eval (SId var) env =
   case lookup var env of
      Just val -> val
      Nothing -> IntVal 0

eval (SIdAssign var e1 e2) env =
  let v1 = eval e1 env
   in eval e2 (addToEnv var v1 env)

eval (SIf e1 e2 e3) env =
  let v1 = eval e1 env
   in case v1 of
       BoolVal True -> eval e2 env
       _            -> eval e3 env

-- TODO: Eval function for SFunc
-- eval (SFunc name argExps) env =

eval _ _ = error "Invalid expression"
