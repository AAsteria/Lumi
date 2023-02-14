module AST where

data Exp = IntExp Integer
         | IntOpExp String Exp Exp
    deriving (Show, Eq)

newtype Val = IntVal Integer
    deriving (Show, Eq)

type Env = [(String,Val)]

emptyEnv :: Env
emptyEnv = []