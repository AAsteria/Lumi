{-# LANGUAGE GADTs #-}

module AST where

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
  SFunc :: String -> [SExp a] -> SExp a
  SList :: [SExp a] -> SExp a
  SVal :: a -> SExp a
  deriving (Show, Eq)

type Env a = [(String, SExp a)]

emptyEnv :: Env a
emptyEnv = []

addToEnv :: String -> SExp a -> Env a -> Env a
addToEnv k v env = (k, v) : env
