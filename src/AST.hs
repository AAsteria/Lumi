{-# LANGUAGE GADTs #-}

module AST where

newtype Identifier = Identifier
    { getId :: String
    } deriving (Show)

data NumericOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)
data CompOp = LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equal | NotEqual deriving (Eq, Show)
data SExp
    = SSExp   SExp [SExp]
    | SInteger Integer
    | SDouble  Double
    | SNumeric SExp
    | SString  String
    | SBool    Bool
    | SId      Identifier
    -- | SIntOp String SExp SExp
    -- | SDoubleOp String SExp SExp
    | SCompOp String SExp SExp
    | SBoolOp String SExp SExp
    | SNumericOp String SExp SExp
    deriving (Show)

data Val = IntVal Integer
         | DoubleVal Double
         | NumericVal SExp
         | BoolVal Bool
         | StringVal String
         | IdVal Identifier
    deriving (Show)

type Env = [(String, Val)]

emptyEnv :: Env
emptyEnv = []

addToEnv :: k -> v -> [(k,v)] -> [(k,v)]
addToEnv k v env = (k,v):env