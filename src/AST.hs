{-# LANGUAGE GADTs #-}

module AST where

import qualified Data.Map as Map
import Data.List (intercalate)

data NumericOp = Add | Subtract | Multiply | Divide | Modulus | Exponentiate | NNExponentiate deriving (Eq, Show)
data CompOp = LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equal | NotEqual deriving (Eq, Show)

data SExp a where
  SSExp :: SExp a -> [SExp a] -> SExp a
  SInteger :: Integer -> SExp a
  SDouble :: Double -> SExp a
  SNumeric :: SExp a -> SExp a
  SString :: String -> SExp a
  SBool :: Bool -> SExp a
  SId :: String -> SExp a
  SCompOp :: CompOp -> SExp a -> SExp a -> SExp a
  SBoolOp :: String -> SExp a -> SExp a -> SExp a
  SNumericOp :: NumericOp -> SExp a -> SExp a -> SExp a
  SIf :: SExp a -> SExp a -> SExp a -> SExp a
  SWhile :: SExp a -> SExp a -> SExp a -> SExp a
  SFunc :: String -> [String] -> Stmt a -> SExp a
  SFuncCall :: String -> [SExp a] -> SExp a --added
  -- SProcedure :: String -> [String] -> Stmt a -> SExp a
  SList :: [SExp a] -> SExp a
  SVal :: a -> SExp a
  SClosure :: Env a -> [String] -> SExp a -> SExp a
  SStmt :: Stmt a -> SExp a
  deriving (Show, Eq)

data Stmt a where
  SeqStmt :: [Stmt a] -> Stmt a
  SPrint :: SExp a -> Stmt a
  SPrintln :: SExp a -> Stmt a
  Assign :: String -> SExp a -> Stmt a
  IfStmt :: SExp a -> Stmt a -> Stmt a -> Stmt a
  -- ProcDecl :: String -> [String] -> Stmt a -> Stmt a
  FuncDecl :: String -> [String] -> Stmt a -> Stmt a
  CallStmt :: String -> [SExp a] -> Stmt a
  Return :: SExp a -> Stmt a
  deriving (Show, Eq)

type Env a = [(String, SExp a)]

emptyEnv :: Env a
emptyEnv = []

addToEnv :: String -> SExp a -> Env a -> Env a
addToEnv k v env = (k, v) : env

lookupEnv :: String -> Map.Map String (SExp a) -> Maybe (SExp a)
lookupEnv = Map.lookup
