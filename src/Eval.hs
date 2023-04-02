{-# LANGUAGE GADTs, RankNTypes #-}

module Eval where

import AST
import Parser
import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp, Lit (IntegerL))
import Data.Typeable
import GHC.Generics
import Data.Foldable (toList)
import Data.Maybe

-- Evaluation function for NumericOps
-- evalNumericOp Add [SInteger 2, SInteger 2, SDouble 3.5]
evalNumericOp :: (Foldable t, Fractional a) => NumericOp -> t (SExp a) -> SExp a
evalNumericOp Add args = foldl add (SInteger 0) args
evalNumericOp Subtract args = foldl subtract' (SInteger 0) args
evalNumericOp Multiply args = foldl multiply (SInteger 1) args
evalNumericOp Divide args = foldl1 divide args

applyArithOp :: (forall a. Num a => a -> a -> a) -> SExp a -> SExp a -> SExp a
applyArithOp op (SInteger x) (SInteger y) = SInteger (x `op` y)
applyArithOp op (SDouble x) (SDouble y) = SDouble (x `op` y)
applyArithOp op (SInteger x) (SDouble y) = SDouble (fromIntegral x `op` y)
applyArithOp op (SDouble x) (SInteger y) = SDouble (x `op` fromIntegral y)
applyArithOp op (SNumeric x) (SNumeric y) = applyArithOp op x y

-- Define the helper functions for NumericOp evaluation
add :: Num a => SExp a -> SExp a -> SExp a
add = applyArithOp (+)

subtract' :: Num a => SExp a -> SExp a -> SExp a
subtract' = applyArithOp (-)

multiply :: Num a => SExp a -> SExp a -> SExp a
multiply = applyArithOp (*)

divide :: Fractional a => SExp a -> SExp a -> SExp a
divide (SInteger x) (SInteger y) = SDouble (fromIntegral x / fromIntegral y)
divide (SDouble x) (SDouble y) = SDouble (x / y)
divide (SInteger x) (SDouble y) = SDouble (fromIntegral x / y)
divide (SDouble x) (SInteger y) = SDouble (x / fromIntegral y)
divide _ _ = error "Numeric operation with non-numeric arguments."

numericOps :: Fractional a => [(NumericOp, SExp a -> SExp a -> SExp a)]
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

applyCompOp :: (forall a. Ord a => a -> a -> Bool) -> SExp a -> SExp a -> Bool
applyCompOp op (SInteger x) (SInteger y) = x `op` y
applyCompOp op (SDouble x) (SDouble y) = x `op` y
applyCompOp op (SInteger x) (SDouble y) = fromIntegral x `op` y
applyCompOp op (SDouble x) (SInteger y) = x `op` fromIntegral y
applyCompOp _ _ _ = error "False: Invalid Argument Type."

lessThan :: Ord a => SExp a -> SExp a -> Bool
lessThan = applyCompOp (<)

greaterThan :: Ord a => SExp a -> SExp a -> Bool
greaterThan = applyCompOp (>)

lessThanOrEqual :: Ord a => SExp a -> SExp a -> Bool
lessThanOrEqual = applyCompOp (<=)

greaterThanOrEqual :: Ord a => SExp a -> SExp a -> Bool
greaterThanOrEqual = applyCompOp (>=)

equal :: Ord a => SExp a -> SExp a -> Bool
equal = applyCompOp (==)

notEqual :: Ord a => SExp a -> SExp a -> Bool
notEqual = applyCompOp (/=)

compOps :: Ord a => [(CompOp, SExp a -> SExp a -> Bool)]
compOps = [(LessThan, lessThan)
         , (GreaterThan, greaterThan)
         , (LessThanOrEqual, lessThanOrEqual)
         , (GreaterThanOrEqual, greaterThanOrEqual)
         , (Equal, equal)
         , (NotEqual, notEqual)]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]

eval :: (Fractional a, Ord a, Show a) => SExp a -> Env a -> SExp a
eval (SNumeric i) _ = SNumeric i
eval (SBool b) _ = SBool b
eval (SString s) _ = SString s

eval (SNumericOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op numericOps
  in liftNumericOp f v1 v2

eval (SCompOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
  in SBool $ liftCompOp f v1 v2

eval (SBoolOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op boolOps
  in SBool (liftBoolOp f v1 v2)

eval (SId var) env =
  case lookup var env of
    Just val -> val
    Nothing -> SInteger 0

eval (SIdAssign var e1 e2) env =
  let v1 = eval e1 env
   in eval e2 (addToEnv var v1 env)

eval (SIf e1 e2 e3) env =
  let v1 = eval e1 env
  in case v1 of
    SBool True -> eval e2 env
    _ -> eval e3 env

-- TODO: Eval function for SFunc

liftBoolOp :: (Bool -> Bool -> Bool) -> SExp a -> SExp a -> Bool
liftBoolOp f (SBool b1) (SBool b2) = f b1 b2
liftBoolOp f _ _ = False

liftNumericOp :: (SExp a1 -> SExp a2 -> SExp a3) -> SExp a1 -> SExp a2 -> SExp a3
liftNumericOp f (SNumeric i1) (SNumeric i2) = SNumeric (f i1 i2)
liftNumericOp f _ _ = SInteger 0

liftCompOp :: (SExp a -> SExp a -> Bool) -> SExp a -> SExp a -> Bool
liftCompOp f (SNumeric i1) (SNumeric i2) = f i1 i2
liftCompOp f _ _ = False