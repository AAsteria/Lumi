{-# LANGUAGE GADTs #-}

module Parser where
import AST

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import Language.Haskell.TH
import Control.Monad (void)
import Control.Monad.Combinators.Expr as C
import GHC.Read (paren)

type Parser = Parsec Void String

bool = label "boolean" $ lexerSpace $ do
  b <- False <$ string "False" <|> True <$ string "True"
  return $ SBool b

integer :: Parser (SExp Integer)
integer = label "integer" $ lexerSpace $ do
  i <- L.signed skipSpace L.decimal
  return $ SInteger i

double :: Parser (SExp Double)
double = label "double" $ lexerSpace $ do
  value <- L.signed skipSpace L.float <* char' 'f'
  return $ SDouble value

numeric :: Parser (SExp a)
numeric = label "number" $ lexerSpace $ do
  value <- L.signed skipSpace L.scientific
  case floatingOrInteger value of
    Left d -> do
      f <- optional $ char' 'f'
      return $ case f of
        Nothing -> SDouble d
        Just _ -> SDouble d
    Right i -> return $ SInteger i

str :: Parser String
str = label "string" $ lexerSpace $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

list :: Parser [SExp a]
list = label "list" $ lexerSpace $ between (char '[') (char ']') (sepBy mmvp (symbol ","))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Reserved Words Dictionary
rwd :: [String]
rwd = ["if","else","in","False","True","OTHER","None","when","eval"
      ,"struct","for","do","MODE","NORMAL","SYMBOL"]

rword :: String -> Parser ()
rword w = (lexerSpace . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = label "identifier" $ lexerSpace $ do
    first <- letterChar <|> char '_'
    seq <- many $ alphaNumChar <|> char '_'
    let ident = first : seq
    if ident `elem` rwd
      then fail $ "Reserved Word As Identifier Exception: " ++ ident
    -- else if ident `elem` gwd
    --   then MODE =< "SYMBOL" -- Apply Assign Function when available
      else pure ident

sexp :: Parser (SExp a)
sexp = label "s-expression" $ lexerSpace $
  between (lexerSpace (char '(')) (char ')') (sexp' <$> mmvp <*> many mmvp)

sexp' :: SExp a -> [SExp a] -> SExp a
sexp' (SId _) [] = error "Identifier must be followed by an expression"
sexp' (SId ident) args = SSExp (SId ident) args
sexp' exp [] = exp
sexp' _ _ = error "Multiple expressions in a single s-expression are not supported"

skipSpace :: Parser ()
skipSpace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexerSpace :: Parser a -> Parser a
lexerSpace = L.lexeme skipSpace

symbol :: String -> Parser String
symbol = L.symbol skipSpace

notFollowedByEq :: [Char] -> Parser [Char]
notFollowedByEq keyword = do
  lexerSpace $ try $ do
    -- Match the keyword, followed by anything other than an equals sign
    string keyword <* notFollowedBy (symbol "=")

arOperators :: [[Operator Parser (SExp a)]]
arOperators =
  [ [ C.InfixL (SNumericOp Exponentiate <$ symbol "**")
    , C.InfixL (SNumericOp NNExponentiate <$ symbol "^")
    , C.InfixL (SNumericOp Modulus <$ symbol "%") ]
  , [C.InfixL (SNumericOp Multiply <$ symbol "*")
    , C.InfixL (SNumericOp Divide <$ notFollowedByEq "/")]
  , [ C.InfixL (SNumericOp Add <$ symbol "+")
    , C.InfixL (SNumericOp Subtract <$ symbol "-") ]
  , [ C.InfixN (SCompOp GreaterThanOrEqual <$ symbol ">=")
   , C.InfixN (SCompOp LessThanOrEqual <$ symbol "<=")
   , C.InfixN (SCompOp GreaterThan <$ symbol ">")
   , C.InfixN (SCompOp LessThan <$ symbol "<")
   , C.InfixN (SCompOp NotEqual <$ symbol "!=")
   , C.InfixN (SCompOp Equal <$ symbol "==") ]
 , [ C.InfixL (SBoolOp "||" <$ symbol "||")
   , C.InfixL (SBoolOp "&&" <$ symbol "&&") ]
  ]

parsePrint :: Parser (SExp a)
parsePrint = do
    rword "print"
    SPrint <$> mmvp

parsePrintln :: Parser (SExp a)
parsePrintln = do
    rword "println"
    SPrintln <$> mmvp

-- usage: parseTest mmvp "if 5 < 8 then Add 2 3; else Add 3 5; ;"
-- if 3 > 8 then 5/6 else 2*8.5;
parseIfElse :: Parser (SExp a)
parseIfElse = do
  rword "if"
  c <- mmvp
  rword "then"
  e1 <- mmvp
  rword "else"
  e2 <- mmvp
  symbol ";"
  return $ SIf c e1 e2

-- parseWhile :: Parser (SExp a)
-- parseWhile = do 
--   rword "while"
--   cond <- mmvp
--   rword "do"
--   block <- many mmvp
--   rword "end"
--   return $ SWhile cond (SList block) (SBool True)

-- when x = 6.5 eval 8.8 + x + 2*x end
assign :: Parser (SExp a)
assign = do
  rword "when"
  v <- identifier
  symbol "="
  e1 <- mmvp
  rword "eval"
  e2 <- mmvp
  rword "end"
  return $ SIdAssign v e1 e2

parseFunc :: Parser (SExp a)
parseFunc = do
  funcName <- identifier
  args <- many (space *> mmvp)
  symbol ";"
  return $ SFunc funcName args

-- Parser to represent expression variants
-- usage: parseTest mvp " "
mvp :: Parser (SExp a)
mvp = SNumeric <$> numeric
   <|> parsePrintln
   <|> parsePrint
   <|> bool
   <|> SString <$> str
   <|> parseIfElse
   <|> assign
   <|> parseFunc
   <|> SId <$> identifier
   <|> parens mmvp

mmvp :: Parser (SExp a)
mmvp = label "expression" $ makeExprParser mvp arOperators

-- Parser helper function
-- usage: case parses "   5.5" of { Left e -> putStrLn e; Right r -> print r }
parses :: String -> Either String (SExp a)
parses input =
  let
    result = parse
      (between skipSpace eof mvp)
      ""
      input
  in
  case result of
    Left err -> Left $ errorBundlePretty err
    Right res -> Right res