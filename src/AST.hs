module AST where

newtype Identifier = Identifier
    { getId :: String
    } deriving (Show)
    
data SExp
    = SSExp   SExp [SExp]
    | SInteger Integer
    | SDouble  Double
    | SNumeric SExp
    | SString  String
    | SBool    Bool
    | SId      Identifier
    | SIntOp String SExp SExp
    | SCompOp String SExp SExp
    | SBoolOp String SExp SExp
    deriving (Show)

data Val = IntVal Integer
         | BoolVal Bool
    deriving (Show)

type Env = [(String, Val)]

emptyEnv :: Env
emptyEnv = []

addToEnv :: k -> v -> [(k,v)] -> [(k,v)]
addToEnv k v env = (k,v):env