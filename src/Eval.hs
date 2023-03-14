module Eval where
import AST
import Parser
import Text.Megaparsec
import Text.ParserCombinators.Parsec
import System.Console.Haskeline
import Language.Haskell.TH (Exp)

-- Evaluation function for NumericOp
evalNumericOp :: Foldable t => NumericOp -> t SExp -> SExp
evalNumericOp Add args = foldl add (SInteger 0) args
evalNumericOp Subtract args = foldl subtract' (SInteger 0) args
evalNumericOp Multiply args = foldl multiply (SInteger 1) args
evalNumericOp Divide args = foldl1 divide args

-- Define the helper functions for NumericOp evaluation
add :: SExp -> SExp -> SExp
add (SInteger x) (SInteger y) = SInteger (x + y)
add (SDouble x) (SDouble y) = SDouble (x + y)
add (SInteger x) (SDouble y) = SDouble (fromIntegral x + y)
add (SDouble x) (SInteger y) = SDouble (x + fromIntegral y)

subtract' :: SExp -> SExp -> SExp
subtract' (SInteger x) (SInteger y) = SInteger (x - y)
subtract' (SDouble x) (SDouble y) = SDouble (x - y)
subtract' (SInteger x) (SDouble y) = SDouble (fromIntegral x - y)
subtract' (SDouble x) (SInteger y) = SDouble (x - fromIntegral y)

multiply :: SExp -> SExp -> SExp
multiply (SInteger x) (SInteger y) = SInteger (x * y)
multiply (SDouble x) (SDouble y) = SDouble (x * y)
multiply (SInteger x) (SDouble y) = SDouble (fromIntegral x * y)
multiply (SDouble x) (SInteger y) = SDouble (x * fromIntegral y)

divide :: SExp -> SExp -> SExp
divide (SInteger x) (SInteger y) = SDouble (fromIntegral x / fromIntegral y)
divide (SDouble x) (SDouble y) = SDouble (x / y)
divide (SInteger x) (SDouble y) = SDouble (fromIntegral x / y)
divide (SDouble x) (SInteger y) = SDouble (x / fromIntegral y)

numericOps :: [(String, SExp -> SExp -> SExp)]
numericOps = [ ("+", add)
         , ("-",subtract')
         , ("*",multiply)
         , ("/",divide)]

-- Evaluation function for CompOp
evalCompOp :: CompOp -> SExp -> SExp -> Bool
evalCompOp LessThan x y = lessThan x y
evalCompOp GreaterThan x y = greaterThan x y
evalCompOp LessThanOrEqual x y = lessThanOrEqual x y
evalCompOp GreaterThanOrEqual x y = greaterThanOrEqual x y
evalCompOp Equal x y = equal x y
evalCompOp NotEqual x y = notEqual x y

lessThan :: SExp -> SExp -> Bool
lessThan (SInteger x) (SInteger y) = x < y
lessThan (SDouble x) (SDouble y) = x < y
lessThan (SInteger x) (SDouble y) = fromIntegral x - y < 0.0
lessThan (SDouble x) (SInteger y) = x - fromIntegral y < 0.0
lessThan _ _ = False

greaterThan :: SExp -> SExp -> Bool
greaterThan (SInteger x) (SInteger y) = x > y
greaterThan (SDouble x) (SDouble y) = x > y
greaterThan (SInteger x) (SDouble y) = fromIntegral x - y > 0.0
greaterThan (SDouble x) (SInteger y) = x - fromIntegral y > 0.0
greaterThan _ _ = False

lessThanOrEqual :: SExp -> SExp -> Bool
lessThanOrEqual (SInteger x) (SInteger y) = x <= y
lessThanOrEqual (SDouble x) (SDouble y) = x <= y
lessThanOrEqual (SInteger x) (SDouble y) = fromIntegral x - y <= 0.0
lessThanOrEqual (SDouble x) (SInteger y) = x - fromIntegral y <= 0.0
lessThanOrEqual _ _ = False

greaterThanOrEqual :: SExp -> SExp -> Bool
greaterThanOrEqual (SInteger x) (SInteger y) = x >= y
greaterThanOrEqual (SDouble x) (SDouble y) = x >= y
greaterThanOrEqual (SInteger x) (SDouble y) = fromIntegral x - y >= 0.0
greaterThanOrEqual (SDouble x) (SInteger y) = x - fromIntegral y >= 0.0
greaterThanOrEqual _ _ = False

equal :: SExp -> SExp -> Bool
equal (SInteger x) (SInteger y)= x == y
equal (SDouble x) (SDouble y) = x == y
equal (SInteger x) (SDouble y) = fromIntegral x - y == 0
equal (SDouble x) (SInteger y) = x - fromIntegral y == 0
equal _ _ = False

notEqual :: SExp -> SExp -> Bool
notEqual (SInteger x) (SInteger y) = x /= y
notEqual (SDouble x) (SDouble y) = x /= y
notEqual (SInteger x) (SDouble y) = fromIntegral x - y /= 0
notEqual (SDouble x) (SInteger y) = x - fromIntegral y /= 0
notEqual _ _ = False

compOps :: [(String, SExp -> SExp -> Bool)]
compOps = [("<", lessThan)
         , (">", greaterThan)
         , ("<=", lessThanOrEqual)
         , (">=", greaterThanOrEqual)
         , ("==", equal)
         , ("/=", notEqual)]

boolOps :: [(String, Bool -> Bool -> Bool)]
boolOps = [ ("&&",(&&))
          , ("||",(||))]

listNumericOp f (NumericVal i1) (NumericVal i2) = NumericVal (f i1 i2)

liftCompOp :: (SExp -> SExp -> Bool) -> Val -> Val -> Val
liftCompOp f (NumericVal i1) (NumericVal i2) = BoolVal (f i1 i2)
liftCompOp f _ _ = BoolVal False

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False

-- -- trans (Val i) = i

eval :: SExp -> Env -> Val
eval (SNumeric i) _ = NumericVal i
eval (SBool b) _ = BoolVal b

eval (SNumericOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op numericOps
   in listNumericOp f v1 v2

eval (SCompOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
   in liftCompOp f v1 v2

eval (SBoolOp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op boolOps
   in liftBoolOp f v1 v2