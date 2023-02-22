module Parser where
import AST

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific
import Language.Haskell.TH
import Control.Monad.Combinators.Expr as C

type Parser = Parsec Void String

bool :: Parser Bool
bool = label "boolean" $ lexerSpace $ False <$ string "False" <|> True <$ string "True"

integer :: Parser Integer
integer = label "integer" $ lexerSpace $ L.signed skipSpace L.decimal

double :: Parser Double
double = label "double" $ lexerSpace $ L.signed skipSpace L.float <* char' 'f'

numeric :: Parser SExp
numeric = label "number" $ lexerSpace $ do
  value <- L.signed skipSpace L.scientific
  case floatingOrInteger value of
    Left d -> SDouble d <$ optional (char' 'f')
    Right i -> do
      f <- optional $ char' 'f'
      pure $ case f of
        Nothing -> SInteger i
        Just _ -> SDouble $ fromIntegral i

str :: Parser String
str = label "string" $ lexerSpace $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

identifier :: Parser Identifier
identifier = label "identifier" $ lexerSpace $ do
    first <- letterChar <|> char '_'
    seq <- many $ alphaNumChar <|> char '_'
    pure $ Identifier $ first : seq

sexp :: Parser (SExp, [SExp])
sexp = label "s-expression" $ lexerSpace $
  between (lexerSpace (char '(')) (char ')') ((,) <$> ssvp <*> many ssvp)

skipSpace :: Parser ()
skipSpace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexerSpace :: Parser a -> Parser a
lexerSpace = L.lexeme skipSpace

intExp = do SInteger <$> integer

symbol :: String -> Parser String
symbol = L.symbol skipSpace

arOperators :: [[Operator Parser SExp]]
arOperators =
  [ [ C.InfixL (SIntOp "*" <$ symbol "*")
    , C.InfixL (SIntOp "/" <$ symbol "/") ]
  , [ C.InfixL (SIntOp "+" <$ symbol "+")
    , C.InfixL (SIntOp "-" <$ symbol "-") ]
  ]

mvp :: Parser SExp
mvp = intExp <|> mmvp

mmvp :: Parser SExp
mmvp = makeExprParser mvp arOperators

-- Parser to represent SExp-specific variants
-- usage: parseTest ssvp " "
-- TODO: move bool, double, string, identifier to "mvp", make them work!
ssvp :: Parser SExp
ssvp = choice
  [ SBool <$> bool
  , SNumeric <$> numeric
  , SString <$> str
  , SId <$> identifier
  ]

-- Parser helper function
-- usage: case parses "   5.5" of { Left e -> putStrLn e; Right r -> print r }
parses :: String -> Either String SExp
parses input =
  let
    result = parse
      (between skipSpace eof ssvp)
      ""
      input
  in
  case result of
    Left err -> Left $ errorBundlePretty err
    Right res -> Right res