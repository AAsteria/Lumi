{-# LANGUAGE GADTs #-}

module AST where

data NumericOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)
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
  -- SIf SExp SExp SExp
  -- SFunc String SExp
  deriving (Show, Eq)

data Val where
  IntVal :: Integer -> Val
  DoubleVal :: Double -> Val
  NumericVal :: SExp a -> Val
  BoolVal :: Bool -> Val

instance Show Val where
  show (IntVal n) = show n
  show (DoubleVal d) = show d
  show (BoolVal b) = show b
  show (NumericVal v) = show v


type Env = [(String, Val)]

emptyEnv :: Env
emptyEnv = []

addToEnv :: String -> Val -> Env -> Env
addToEnv k v env = (k, v) : env
