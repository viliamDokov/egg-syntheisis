{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Data.Char (isAlpha, isDigit, isSpace)

{-
  This program parses a list of rules given in 'rules.rkt' file into a format that the rust egg library understands
  The main function of this is convert the herbie rules (which are written in racket) to rules that can be used in egg.

  To parse this we use parser combinators.
  A parts of code were borrowed from:
  https://github.com/tsoding/haskell-json/blob/master/Main.hs

-}

-- this data structure keeps track of the file input and the charecter that we are at
data Input = Input
  { inputLoc :: Int,
    inputStr :: String
  }
  deriving (Show, Eq)

-- | this function is used to get a character from an input and maintain the current column...
inputUncons ::
  Input -> -- input to check
  Maybe (Char, Input)
inputUncons (Input _ []) = Nothing
inputUncons (Input loc (x : xs)) = Just (x, Input (loc + 1) xs)

-- Main definition of a rule
-- A rule is of the form:
-- ["rule-name" left-expr right-expr]
data Rule = Rule String Expr Expr deriving (Show, Eq)

-- Main structure of a rule
data Expr
  = Num Int -- interger literals
  | Frac Int Int -- fraction of the form int/int
  | Var String -- variable name
  | Pi -- the pi constant
  | E -- euler's constat
  | Op OpType -- an operation
  deriving (Show, Eq)

data OpType
  = Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Copysign Expr Expr -- copysign, like https://en.cppreference.com/w/cpp/numeric/math/copysign
  | Neg Expr -- negation of a number
  | Sqrt Expr
  | Cbrt Expr -- cube root
  | Fabs Expr -- floating point absolute value
  | FMin Expr Expr -- min
  | FMax Expr Expr -- max
  | Exp Expr -- exponentiation
  | Log Expr -- natural lo
  | Sin Expr -- Trig functions ...
  | Cos Expr
  | Tan Expr
  | Asin Expr
  | Acos Expr
  | Atan Expr
  | Atan2 Expr Expr
  | Sinh Expr
  | Cosh Expr
  | Tanh Expr
  | Asinh Expr
  | Acosh Expr
  | Atanh Expr
  | Remainder Expr Expr
  deriving (Show, Eq)

-- definition of parser
data ParserError = ParserError Int String deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

-- Our parser implements Functor, Applicative, Alternative

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

-- Parser for a signle character
charP ::
  Char ->
  Parser Char
charP x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
          Left $
            ParserError
              (inputLoc input)
              ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left $
        ParserError
          (inputLoc input)
          ("Expected '" ++ [x] ++ "', but reached end of string")

-- Parse for a given string
stringP ::
  String ->
  Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left $
          ParserError
            (inputLoc input)
            ("Expected \"" ++ str ++ "\", but found \"" ++ inputStr input ++ "\"")
      result -> result

-- parse a single charecter that fullfills condition
parseIf ::
  String -> -- name of the predicate
  (Char -> Bool) -> -- predicate
  Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
            Left $
              ParserError
                (inputLoc input)
                ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left $
          ParserError
            (inputLoc input)
            ("Expected " ++ desc ++ ", but reached end of string")

-- parse whitespaces
ws :: Parser String
ws = many $ parseIf "whitespace character" isSpace

-- parse an integer
numP :: Parser Int
numP = (*) <$> signVal <*> (read <$> digits)
  where
    signVal = -1 <$ charP '-' <|> pure 1
    digits = some $ parseIf "digit" isDigit

parseNum :: Parser Expr
parseNum = Num <$> numP

parseFrac :: Parser Expr
parseFrac = Frac <$> numP <* charP '/' <*> numP

parseVar :: Parser Expr
parseVar = Var <$> chars
  where
    chars = some $ parseIf "char" isAlpha

parsePi :: Parser Expr
parsePi = Pi <$ stringP "(PI)"

parseE :: Parser Expr
parseE = Pi <$ stringP "(E)"

-- all of the operation parsers
parseOp :: Parser Expr
parseOp = Op <$> (charP '(' *> ws *> op <* ws <* charP ')')
  where
    op =
      parsePlus
        <|> parseMinus
        <|> parseMul
        <|> parseDiv
        <|> parseNeg
        <|> parsePow
        <|> parseSqrt
        <|> parseCbrt
        <|> parseFabs
        <|> parseCopysign
        <|> parseFmin
        <|> parseFmax
        <|> parseExp
        <|> parseLog
        <|> parseSinh
        <|> parseCosh
        <|> parseTanh
        <|> parseSin
        <|> parseCos
        <|> parseTan
        <|> parseATan2
        <|> parseASinh
        <|> parseACosh
        <|> parseATanh
        <|> parseASin
        <|> parseACos
        <|> parseATan
        <|> parseRemainder

parsePlus :: Parser OpType
parsePlus = uncurry Plus <$> parseOp2 "+"

parseMinus :: Parser OpType
parseMinus = uncurry Minus <$> parseOp2 "-"

parseMul :: Parser OpType
parseMul = uncurry Mul <$> parseOp2 "*"

parseDiv :: Parser OpType
parseDiv = uncurry Div <$> parseOp2 "/"

parsePow :: Parser OpType
parsePow = uncurry Pow <$> parseOp2 "pow"

parseNeg :: Parser OpType
parseNeg = Neg <$> parseOp1 "neg"

parseSqrt :: Parser OpType
parseSqrt = Sqrt <$> parseOp1 "sqrt"

parseCbrt :: Parser OpType
parseCbrt = Cbrt <$> parseOp1 "cbrt"

parseFabs :: Parser OpType
parseFabs = Fabs <$> parseOp1 "fabs"

parseFmin :: Parser OpType
parseFmin = uncurry FMin <$> parseOp2 "fmin"

parseFmax :: Parser OpType
parseFmax = uncurry FMax <$> parseOp2 "fmax"

parseExp :: Parser OpType
parseExp = Exp <$> parseOp1 "exp"

parseLog :: Parser OpType
parseLog = Log <$> parseOp1 "log"

parseSin :: Parser OpType
parseSin = Sin <$> parseOp1 "sin"

parseCos :: Parser OpType
parseCos = Cos <$> parseOp1 "cos"

parseTan :: Parser OpType
parseTan = Tan <$> parseOp1 "tan"

parseASin :: Parser OpType
parseASin = Asin <$> parseOp1 "asin"

parseACos :: Parser OpType
parseACos = Acos <$> parseOp1 "acos"

parseATan :: Parser OpType
parseATan = Atan <$> parseOp1 "atan"

parseATan2 :: Parser OpType
parseATan2 = uncurry Atan2 <$> parseOp2 "atan2"

parseASinh :: Parser OpType
parseASinh = Asinh <$> parseOp1 "asinh"

parseACosh :: Parser OpType
parseACosh = Acosh <$> parseOp1 "acosh"

parseATanh :: Parser OpType
parseATanh = Atanh <$> parseOp1 "atanh"

parseSinh :: Parser OpType
parseSinh = Sinh <$> parseOp1 "sinh"

parseCosh :: Parser OpType
parseCosh = Cosh <$> parseOp1 "cosh"

parseTanh :: Parser OpType
parseTanh = Tanh <$> parseOp1 "tanh"

parseCopysign :: Parser OpType
parseCopysign = uncurry Copysign <$> parseOp2 "copysign"

parseRemainder :: Parser OpType
parseRemainder = uncurry Remainder <$> parseOp2 "remainder"

parseOp1 :: String -> Parser Expr
parseOp1 ident = stringP ident *> ws *> parseExpr

pair :: a -> b -> (a, b)
pair a b = (a, b)

parseOp2 :: String -> Parser (Expr, Expr)
parseOp2 ident = stringP ident *> ws *> tup
  where
    tup = pair <$> parseExpr <* ws <*> parseExpr

parseExpr :: Parser Expr
parseExpr = parseE <|> parsePi <|> parseFrac <|> parseNum <|> parseVar <|> parseOp

parseRule :: Parser Rule
parseRule = Rule <$> (charP '[' *> ws *> ident <* ws) <*> (parseExpr <* ws) <*> parseExpr <* ws <* charP ']'
  where
    ident = some $ parseIf "ident" (not . isSpace)

convertRule :: Rule -> String
convertRule (Rule name e1 e2) = "rewrite!(\"" <> name <> "\"; \"" <> convertExpr e1 <> "\" => \"" <> convertExpr e2 <> "\"),"

convertExpr :: Expr -> String
convertExpr (Num n) = show n
convertExpr (Var vName) = "?" <> vName
convertExpr (Frac n1 n2) = "(/ " <> show n1 <> " " <> show n2 <> ")"
convertExpr (Op op) = "(" <> convertOp op <> ")"
convertExpr E = "(E)"
convertExpr Pi = "(PI)"

convertOp :: OpType -> String
convertOp (Plus e1 e2) = convertOp2 "+" e1 e2
convertOp (Minus e1 e2) = convertOp2 "-" e1 e2
convertOp (Mul e1 e2) = convertOp2 "*" e1 e2
convertOp (Div e1 e2) = convertOp2 "/" e1 e2
convertOp (Pow e1 e2) = convertOp2 "pow" e1 e2
convertOp (Neg e1) = convertOp1 "neg" e1
convertOp (Sqrt e1) = convertOp1 "sqrt" e1
convertOp (Cbrt e1) = convertOp1 "cbrt" e1
convertOp (Fabs e1) = convertOp1 "fabs" e1
convertOp (FMin e1 e2) = convertOp2 "fmin" e1 e2
convertOp (FMax e1 e2) = convertOp2 "fmax" e1 e2
convertOp (Exp e1) = convertOp1 "exp" e1
convertOp (Log e1) = convertOp1 "log" e1
convertOp (Sin e1) = convertOp1 "sin" e1
convertOp (Cos e1) = convertOp1 "cos" e1
convertOp (Tan e1) = convertOp1 "tan" e1
convertOp (Asin e1) = convertOp1 "asin" e1
convertOp (Acos e1) = convertOp1 "acos" e1
convertOp (Atan e1) = convertOp1 "atan" e1
convertOp (Atan2 e1 e2) = convertOp2 "atan2" e1 e2
convertOp (Sinh e1) = convertOp1 "sinh" e1
convertOp (Cosh e1) = convertOp1 "cosh" e1
convertOp (Tanh e1) = convertOp1 "tanh" e1
convertOp (Asinh e1) = convertOp1 "asinh" e1
convertOp (Acosh e1) = convertOp1 "acosh" e1
convertOp (Atanh e1) = convertOp1 "atanh" e1
convertOp (Copysign e1 e2) = convertOp2 "copysign" e1 e2
convertOp (Remainder e1 e2) = convertOp2 "remainder" e1 e2

convertOp1 :: String -> Expr -> String
convertOp1 symbol e1 = symbol <> " " <> convertExpr e1

convertOp2 :: String -> Expr -> Expr -> String
convertOp2 symbol e1 e2 = symbol <> " " <> convertExpr e1 <> " " <> convertExpr e2

printConversion :: String -> IO ()
printConversion rule = case runParser parseRule $ Input 0 rule of
  Left err -> print err
  Right (_, res) -> putStr $ convertRule res <> "\n"

main :: IO ()
main = do
  content <- readFile "rules.rkt"
  mapM_ printConversion (lines content)
