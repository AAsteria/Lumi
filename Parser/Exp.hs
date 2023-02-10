-- module Exp where

-- import System.IO
-- import Control.Monad
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Data.Text (Text)
-- import Data.Void

-- https://markkarpov.com/tutorial/megaparsec.html#parsect-and-parsec-monads

-- newtype Identifier = Identifier
--   { getId :: String
--   } deriving (Show)

-- data AExpr = Var String
--            | IntConst Integer
--            | Neg AExpr
--            | ABinary ABinOp AExpr AExpr
--            deriving (Show)

-- data ABinOp = Add
--             | Subtract
--             | Multiply
--             | Divide
--             deriving (Show)

-- langDef =
--     emptyDef { Token.commentStart    = "/*"
--              , Token.commentEnd      = "*/"
--              , Token.commentLine     = "//"
--              , Token.reservedNames   = [ "if"
--                                        , "then"
--                                        , "else"
--                                        , "while"
--                                        , "do"
--                                        , "skip"
--                                        , "true"
--                                        , "false"
--                                        , "not"
--                                        , "and"
--                                        , "or"
--                                        ]
--              , Token.reservedOpNames = ["+", "-", "*", "/", ":="
--                                        , "<", " ", "and", "or", "not"
--                                        ]
--              }

-- lexer = Token.makeTokenParser langDef