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
import qualified AST as Map

simplify :: (Fractional a, Integral a) => SExp a -> SExp a
simplify (SInteger x) = SInteger x
simplify (SDouble x) = SDouble x
simplify (SNumeric (SInteger x)) = SInteger x
simplify (SNumeric (SDouble x)) = SDouble x
simplify (SNumeric (SNumeric x)) = simplify x
simplify (SList []) = SList []
simplify (SList (x:xs)) = simplifyList (simplify x) (simplify (SList xs))

simplifyList :: (Fractional a, Integral a) => SExp a -> SExp a -> SExp a
simplifyList (SInteger x) (SList xs) = SList (map (\(SInteger y) -> SInteger (gcd x y)) xs)
simplifyList _ _ = error "Invalid Argument Type."

-- Evaluation function for NumericOps
-- evalNumericOp Add [SInteger 2, SInteger 2, SDouble 3.5]
evalNumericOp :: (Foldable t, Fractional a, Floating a, Integral a) => NumericOp -> t (SExp a) -> SExp a
evalNumericOp Add args = foldl add (SInteger 0) args
evalNumericOp Subtract args = foldl subtract' (SInteger 0) args
evalNumericOp Multiply args = foldl multiply (SInteger 1) args
evalNumericOp Divide args = simplify $ foldl1 divide args
evalNumericOp Modulus args = foldl1 modulus args
evalNumericOp Exponentiate args = foldl1 exponentiate args
evalNumericOp NNExponentiate args = foldl1 nnexponentiate args

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

modulus :: Num a=> SExp a -> SExp a -> SExp a
modulus (SInteger x) (SInteger y) = SInteger (x `mod` y)
modulus _ _ = error "Modulus operation with non-integer arguments."

exponentiate :: Floating a => SExp a -> SExp a -> SExp a
exponentiate (SInteger x) (SInteger y) = SDouble (fromIntegral x ** fromIntegral y)
exponentiate (SDouble x) (SInteger y) = SDouble (x ** fromIntegral y)
exponentiate (SInteger x) (SDouble y) = SDouble (fromIntegral x ** y)
exponentiate (SDouble x) (SDouble y) = SDouble (x ** y)

nnexponentiate :: (Num a) => SExp a -> SExp b -> SExp a
nnexponentiate (SInteger x) (SInteger y) = SDouble (fromIntegral x ^ fromIntegral y)
nnexponentiate (SDouble x) (SInteger y) = SDouble (x ^ fromIntegral y)

numericOps :: (Fractional a, Floating a) => [(NumericOp, SExp a -> SExp a -> SExp a)]
numericOps = [ (Add, add)
             , (Subtract, subtract')
             , (Multiply, multiply)
             , (Divide, divide)
             , (Modulus, modulus)
             , (Exponentiate, exponentiate)
             , (NNExponentiate, nnexponentiate)
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

-- Evaluation functions for SExp
eval :: (Fractional a, Ord a, Show a, Floating a) => SExp a -> Env a -> SExp a
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
    Nothing -> error $ "Variable not found in environment: " ++ var

eval (SList []) env = SList []  -- empty list evaluates to itself
eval (SList (x:xs)) env =
  let xVal = eval x env
      restVal = eval (SList xs) env
      (SList rest) = restVal
  in SList (xVal : rest)

eval (SIf e1 e2 e3) env =
  let v1 = eval e1 env
  in case v1 of
    SBool True -> eval e2 env
    _ -> eval e3 env

-- TODO: Eval function for SFunc
eval (SFunc name args body) env =
  SClosure env args (SStmt body)

-- Add other cases for literals, operators, and conditionals as needed
eval _ _ = error "Evaluation not implemented for this expression"

liftBoolOp :: (Bool -> Bool -> Bool) -> SExp a -> SExp a -> Bool
liftBoolOp f (SBool b1) (SBool b2) = f b1 b2
liftBoolOp f _ _ = False

liftNumericOp :: (SExp a1 -> SExp a2 -> SExp a3) -> SExp a1 -> SExp a2 -> SExp a3
liftNumericOp f (SNumeric i1) (SNumeric i2) = SNumeric (f i1 i2)
liftNumericOp f _ _ = SInteger 0

liftCompOp :: (SExp a -> SExp a -> Bool) -> SExp a -> SExp a -> Bool
liftCompOp f (SNumeric i1) (SNumeric i2) = f i1 i2
liftCompOp f _ _ = False

-- Evaluation functions for Stmt
-- TODO: Combine execStmt and eval functions to use in Main.hs (repl)
execStmt :: (Fractional a, Ord a, Show a, Floating a) => Env a -> Stmt a -> IO (Maybe String, Env a)
execStmt env (SeqStmt []) = return (Nothing, env)
execStmt env (SeqStmt (st:sts)) = do
  (i1, env1) <- execStmt env st
  (i2, env2) <- execStmt env1 (SeqStmt sts)
  return (combine i1 i2, env2)
  where
    combine Nothing b = b
    combine a Nothing = a
    combine (Just a) (Just b) = Just (a ++ "\n" ++ b)

execStmt env (Assign var val) = do
  let val' = eval val env
  return (Nothing, addToEnv var val' env)
  
execStmt env (IfStmt cond tr fl) = execIfStmt env cond tr fl

-- execStmt env (Return val) = do
--   let val' = eval val env
--   case val' of
--     SVal v -> return (Just $ show v, env)
--     _ -> return (Nothing, env)
execStmt env (Return val) = do
  let val' = eval val env
  return (Just $ show val', env)
  
execStmt env (SPrint exp) =
  let val = eval exp env
  in return (Just $ show val, env)

execStmt env (SPrintln exp) =
  let val = eval exp env
  in return (Just $ show val ++ "\n", env)

execStmt env (FuncDecl name args body) = return (Nothing, addToEnv name (SFunc name args body) env)


execIfStmt :: (Fractional a, Ord a, Show a, Floating a) => Env a -> SExp a -> Stmt a -> Stmt a -> IO (Maybe String, Env a)
execIfStmt env condStmt thenStmt elseStmt = do
  let cond = eval condStmt env
  case cond of
    SBool True -> execStmt env thenStmt
    SBool False -> execStmt env elseStmt
    _ -> return (Nothing, env)
    