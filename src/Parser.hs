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
import qualified Data.Maybe

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

parsePrint :: Parser (AST.Stmt a)
parsePrint = do
    rword "print"
    SPrint <$> mmvp

parsePrintln :: Parser (AST.Stmt a)
parsePrintln = do
    rword "println"
    SPrintln <$> mmvp

-- braces :: Parser a -> AST.Stmt a
-- braces p = label "braces" $ do
--   symbol "{"
--   x <- p
--   symbol "}"
--   return $ SeqStmt x

--Added SeqStmt, commented Block stmt
-- sepEndBy (;) stmt
-- e.g. { print 1 ; print "hi";}
-- e.g. { print 1 ; print "hi"}
-- TODO: stmts separation without brackets
seqStmt :: Parser (AST.Stmt a)
seqStmt = label "sequence statement" $ do
  symbol "{"
  stmts <- stmt `sepEndBy` symbol ";"
  optional (symbol ";")  -- Optional semicolon after the last statement
  symbol "}"
  return $ SeqStmt stmts

-- usage: parseTest stmt "a = 2.5"
assignStmt :: Parser (AST.Stmt a)
assignStmt = label "assignment statement" $ do
    i <- try $ identifier <* symbol "="
    Assign i <$> mmvp

-- usage: parseTest stmt "if (3<5.5) {return True} else {return False}"
ifStmt :: Parser (AST.Stmt a)
ifStmt = label "if statement" $ do
    symbol "if"
    cond <- mmvp
    --Added: replaced block stmt with seq stmt
    thenBranch <- seqStmt
    elseBranch <- optional (symbol "else" *> seqStmt)
    return $ IfStmt cond thenBranch (Data.Maybe.fromMaybe (SeqStmt []) elseBranch)

-- usage: parseTest stmt "def myFunc(a,b) {return 1}"
funDeclStmt :: Parser (AST.Stmt a)
funDeclStmt = label "function declaration statement" $ do
  symbol "def"
  name <- identifier
  args <- parens (identifier `sepBy` symbol ",")
  FunDecl name args <$> seqStmt

returnStmt :: Parser (AST.Stmt a)
returnStmt = label "return statement" $ do
    rword "return"
    Return <$> mmvp

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

-- ============================================================
-- =====================< CORE PARSER >========================
-- ============================================================
-- Parser for statements
-- usage: parseTest stmt "return 6+6"
-- parseTest stmt "if (3<5.5) {return True} else {return False}"
stmt :: Parser (AST.Stmt a)
stmt = assignStmt
--     <|> ifStmt
--     <|> funDeclStmt
--     <|> returnStmt
    <|> parsePrintln
    <|> parsePrint
    <|> try (seqStmt <* optional (symbol ";"))
    

-- Parser for expression variants
mvp :: Parser (SExp a)
mvp = SStmt <$> stmt
   <|> SNumeric <$> numeric
   <|> bool
   <|> SString <$> str
   <|> SList <$> list
   <|> SId <$> identifier
   <|> parens mmvp

mmvp :: Parser (SExp a)
mmvp = label "expression" $ makeExprParser mvp arOperators
-- ============================================================
-- =====================< CORE PARSER >========================
-- ============================================================