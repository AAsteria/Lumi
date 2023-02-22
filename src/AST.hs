module AST where

newtype Identifier = Identifier
    { getId :: String
    } deriving (Show)
    
data SExp
    = SSExp   SExp [SExp]
    | SInteger Integer
    | SIntOp String SExp SExp -- Should we move to somewhere else?
    | SDouble  Double
    | SNumeric SExp
    | SString  String
    | SBool    Bool
    | SId      Identifier
    deriving (Show)

type Env = [(String, SExp)]

emptyEnv :: Env
emptyEnv = []

addToEnv :: k -> v -> [(k,v)] -> [(k,v)]
addToEnv k v env = (k,v):env