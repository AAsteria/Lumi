{-# LANGUAGE GADTs #-}

module Eval where

import AST
import Parser
import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp, Lit (IntegerL))
import Data.Typeable
import GHC.Generics

-- Evaluation function for NumericOp
evalNumericOp :: Fractional a => NumericOp -> [AST.Exp a] -> AST.Exp a
evalNumericOp Add args = foldl add (Val 0) args
evalNumericOp Subtract args = foldl subtract' (Val 0) args
evalNumericOp Multiply args = foldl multiply (Val 1) args
evalNumericOp Divide args = foldl1 divide args

-- Define the helper functions for NumericOp evaluation
add :: Num a => AST.Exp a -> AST.Exp a -> AST.Exp a
add (Val i) (Val j) = Val $ i + j

subtract' :: Num a => AST.Exp a -> AST.Exp a -> AST.Exp a
subtract' (Val i) (Val j) = Val $ i - j

multiply :: Num a => AST.Exp a -> AST.Exp a -> AST.Exp a
multiply (Val i) (Val j) = Val $ i * j

divide :: Fractional a => AST.Exp a -> AST.Exp a -> AST.Exp a
divide (Val i) (Val j) = Val $ i / j

numericOps :: Fractional a => [(NumericOp, AST.Exp a -> AST.Exp a -> AST.Exp a)]
numericOps = [ (Add, add)
             , (Subtract, subtract')
             , (Multiply, multiply)
             , (Divide, divide)
             ]

-- Evaluation function for CompOp
evalCompOp :: (Ord a, Num a) => CompOp -> AST.Exp a -> AST.Exp a -> Bool
evalCompOp LessThan x y = lessThan x y
evalCompOp GreaterThan x y = greaterThan x y
evalCompOp LessThanOrEqual x y = lessThanOrEqual x y
evalCompOp GreaterThanOrEqual x y = greaterThanOrEqual x y
evalCompOp Equal x y = equal x y
evalCompOp NotEqual x y = notEqual x y

lessThan :: Ord a => AST.Exp a -> AST.Exp a -> Bool
lessThan (Val x) (Val y) = x < y

greaterThan :: Ord a => AST.Exp a -> AST.Exp a -> Bool
greaterThan (Val x) (Val y) = x > y

lessThanOrEqual :: Ord a => AST.Exp a -> AST.Exp a -> Bool
lessThanOrEqual (Val x) (Val y) = x <= y

greaterThanOrEqual :: Ord a => AST.Exp a -> AST.Exp a -> Bool
greaterThanOrEqual (Val x) (Val y) = x >= y

equal :: Ord a => AST.Exp a -> AST.Exp a -> Bool
equal (Val x) (Val y) = x == y

notEqual :: Ord a => AST.Exp a -> AST.Exp a -> Bool
notEqual (Val x) (Val y) = x /= y

compOps :: Ord a => [(CompOp, AST.Exp a -> AST.Exp a -> Bool)]
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

-- -- TODO: Modify the following two evaluation functions
-- -- Now only parser works!
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

-- -- liftNumericOp :: Num a1 => (AST.Exp a1 -> AST.Exp a1 -> AST.Exp a1) -> Val -> Val -> Val
-- -- liftNumericOp f (i1) (i2) = Val (f i1 i2)
-- -- liftNumericOp f _            _            = IntVal 0

-- -- TODO: Eval function for SFunc
-- -- eval (SFunc name argExps) env =