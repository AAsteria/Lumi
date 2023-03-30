{-# LANGUAGE GADTs, RankNTypes #-}

module Eval where
import AST
import Parser
import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp, Lit (IntegerL))
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

-- Helper Functions that determins whether a double can be rounded to an int in binary operations
isInteger :: Double -> Bool
isInteger x = fromInteger (round x) == x

resultAsSExp :: Double -> SExp a
resultAsSExp x
  | isInteger x = SInteger (round x)
  | otherwise = SDouble x

-- Define the helper functions for NumericOp evaluation
applyBinaryOp :: (Double -> Double -> Double) -> Val -> Val -> SExp a
applyBinaryOp op (IntVal x) (IntVal y) = resultAsSExp (fromIntegral x `op` fromIntegral y)
applyBinaryOp op (DoubleVal x) (DoubleVal y) = SDouble (x `op` y)
applyBinaryOp op (IntVal x) (DoubleVal y) = SDouble (fromIntegral x `op` y)
applyBinaryOp op (DoubleVal x) (IntVal y) = SDouble (x `op` fromIntegral y)
applyBinaryOp _ _ _ = error "Type error: arguments of the wrong type for binary operation"

add, subtract', multiply :: Val -> Val -> SExp a
add = applyBinaryOp (+)
subtract' = applyBinaryOp (-)
multiply = applyBinaryOp (*)
divide = applyBinaryOp (/)

numericOps :: [(NumericOp, Val -> Val -> SExp a)]
numericOps = [ (Add, add)
             , (Subtract, subtract')
             , (Multiply, multiply)
             , (Divide, divide)
             ]

-- Define the helper functions for CompOp evaluation
applyCompOp :: (Double -> Double -> Bool) -> Val -> Val -> Bool
applyCompOp op (IntVal x) (IntVal y) = fromIntegral x `op` fromIntegral y
applyCompOp op (DoubleVal x) (DoubleVal y) = x `op` y
applyCompOp op (IntVal x) (DoubleVal y) = fromIntegral x `op` y
applyCompOp op (DoubleVal x) (IntVal y) = x `op` fromIntegral y
applyCompOp _ _ _ = False

lessThan, greaterThan, lessThanOrEqual, greaterThanOrEqual, equal, notEqual :: Val -> Val -> Bool
lessThan = applyCompOp (<)
greaterThan = applyCompOp (>)
lessThanOrEqual = applyCompOp (<=)
greaterThanOrEqual = applyCompOp (>=)
equal = applyCompOp (==)
notEqual = applyCompOp (/=)

compOps :: [(CompOp, Val -> Val -> Bool)]
compOps = [ (LessThan, lessThan)
          , (GreaterThan, greaterThan)
          , (LessThanOrEqual, lessThanOrEqual)
          , (GreaterThanOrEqual, greaterThanOrEqual)
          , (Equal, equal)
          , (NotEqual, notEqual)
          ]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False

-- debug
-- debugVal :: String -> Val -> Val
-- debugVal msg val = unsafePerformIO $ do
--   putStrLn (msg ++ ": " ++ show val)
--   return val

-- Simple Eval Transition Station
eval :: SExp a -> Env -> Val
eval (SInteger i) _ = IntVal i
eval (SDouble i) _ = DoubleVal i
eval (SNumeric (SInteger i)) _ = IntVal i
eval (SNumeric (SDouble d)) _ = DoubleVal d
eval (SBool b) _ = BoolVal b


eval (SNumericOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op numericOps
   in eval (f v1 v2) env

eval (SCompOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
   in BoolVal (f v1 v2)

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
