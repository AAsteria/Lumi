{-# LANGUAGE GADTs, InstanceSigs #-}

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
  SIdAssign :: String -> SExp a -> SExp a -> SExp a
  SIf :: SExp a -> SExp a -> SExp a -> SExp a
  SWhile :: SExp a -> SExp a -> SExp a -> SExp a
  SFunc :: String -> [String] -> SExp a -> SExp a
  SList :: [SExp a] -> SExp a
  SPrint :: SExp a -> SExp a
  SPrintln :: SExp a -> SExp a
  SVal :: a -> SExp a
  deriving (Show, Eq)

data Stmt a where
  Block :: [Stmt a] -> Stmt a
  Assign :: Show a => String -> SExp a -> Stmt b
  IfStmt :: SExp a -> Stmt a -> Stmt a -> Stmt a
  FunDecl :: String -> [String] -> Stmt a -> Stmt a
  Return :: SExp a -> Stmt a

instance Show a => Show (Stmt a) where
  show (Block stmts) = "{ " ++ intercalate "; " (map show stmts) ++ " }"
  show (Assign var sexp) = var ++ " = " ++ show sexp
  show (IfStmt cond thenStmt elseStmt) =
    "if " ++ show cond ++ " then " ++ show thenStmt ++ " else " ++ show elseStmt
  show (FunDecl name args body) =
    "function " ++ name ++ "(" ++ intercalate ", " args ++ ") { " ++ show body ++ " }"
  show (Return sexp) = "return " ++ show sexp

instance Eq a => Eq (Stmt a) where
  (==) :: Eq a => Stmt a -> Stmt a -> Bool
  Block stmts1 == Block stmts2 = stmts1 == stmts2
  Assign id1 _ == Assign id2 _ = id1 == id2
  IfStmt cond1 then1 else1 == IfStmt cond2 then2 else2 =
    cond1 == cond2 && then1 == then2 && else1 == else2
  FunDecl name1 args1 body1 == FunDecl name2 args2 body2 =
    name1 == name2 && args1 == args2 && body1 == body2
  Return exp1 == Return exp2 = exp1 == exp2
  _ == _ = False

type Env a = [(String, SExp a)]

emptyEnv :: Env a
emptyEnv = []

addToEnv :: String -> SExp a -> Env a -> Env a
addToEnv k v env = (k, v) : env

lookupEnv :: String -> Map.Map String (SExp a) -> Maybe (SExp a)
lookupEnv = Map.lookup