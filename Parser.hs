module Parser where

import AST
import Control.Applicative
import Control.Monad
import Data.Char
import Debug.Trace

type Error = String

data OpPrec =
  OpPrec Op Int Fixity

data Fixity
  = R
  | L
  deriving (Eq, Show)

newtype Parser a =
  Parser
    { runParser :: String -> Either Error (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \s ->
      case p s of
        Left error -> Left error
        Right (s1, out) -> Right (s1, f out)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (s, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \s -> do
      (s1, f) <- p1 s
      (s2, x) <- p2 s1
      Right (s2, f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty"
  (Parser p1) <|> (Parser p2) =
    Parser $ \s ->
      case p1 s of
        Right (s, a) -> Right (s, a)
        Left _ -> p2 s

instance Monad Parser where
  return = pure
  (Parser p1) >>= f =
    Parser $ \s -> do
      (s1, tok) <- p1 s
      runParser (f tok) s1

parseChar :: Char -> Parser Char
parseChar y = Parser $ f
  where
    f (x:xs)
      | x == y = Right (xs, x)
    f [] = Left "Empty String"
    f _ = Left "Unable to match char"

parseString :: String -> Parser String
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs
parseString [] = pure []

parseSomeWs :: Parser [String]
parseSomeWs = some (parseString " ")

skipW :: Parser [String]
skipW = join <$> some ((some $ parseString " ") <|> (some $ parseString "\n"))

skipManyW :: Parser [String]
skipManyW =
  join <$> many ((some $ parseString " ") <|> (some $ parseString "\n"))

lookahead :: Parser a -> Parser a
lookahead (Parser p1) =
  Parser $ \s -> do
    (s1, tok) <- p1 s
    Right (s, tok)

option :: a -> Parser a -> Parser a
option x p1 = p1 <|> return x

parseSpan :: (Char -> Bool) -> Parser String
parseSpan fp =
  Parser $ \s ->
    let (tok, rest) = span fp s
     in if null tok
          then Left "Unable to match char"
          else Right (rest, tok)

parseOption :: Parser String -> Parser String -> Parser [String]
parseOption (Parser p1) (Parser p2) =
  Parser $ \s -> do
    (s1, tok1) <- p1 s
    case p2 s1 of
      Left _ -> Right (s1, [tok1])
      Right (s2, tok2) -> Right (s2, [tok1, tok2])

parseSC :: Parser String
parseSC = skipManyW *> parseString ";" <* skipManyW

parseIdentifier :: Parser String
parseIdentifier =
  concat <$> parseOption (parseSpan isAlpha) (parseSpan isAlphaNum)

parseType :: Parser Type
parseType = (TyInt <$ parseString "int" <|> TyBool <$ parseString "bool")

parseInt :: Parser Expr
parseInt = Literal <$> read <$> parseSpan isNumber

parseLiteral :: Parser Expr
parseLiteral = parseInt <* skipManyW

parseExpr :: Parser Expr
parseExpr = parseBinop <|> parseInt

parseStatement :: Parser Statement
parseStatement = parseReturn <|> parseIfStat

parseReturn :: Parser Statement
parseReturn = do
  parseString "return" <* skipW
  expr <- parseExpr <* parseSC
  return (Return expr)

parseNumOrOpenPar :: Int -> Parser Expr
parseNumOrOpenPar prec =
  ((parseString "(" *> skipManyW *> (parseLiteral >>= precClimbing (-1)) <*
    skipManyW <*
    parseString ")" <*
    skipManyW) <|>
   (parseLiteral >>= precClimbing (prec))) >>=
  precClimbing (prec)

precClimbing :: Int -> Expr -> Parser Expr
precClimbing minPrec left =
  option left $ do
    opType@(OpPrec op prec fix) <- lookahead parseOpPrec
    if prec < minPrec || prec == minPrec && fix == L
      then return left
      else do
        right <- parseOpPrec >> parseNumOrOpenPar prec
        precClimbing minPrec (Binop op left right)

parseBinop :: Parser Expr
parseBinop = parseNumOrOpenPar (-1)

parseOpPrec :: Parser OpPrec
parseOpPrec =
  ((OpPrec Mult 5 L <$ parseString "*") <|> (OpPrec Div 5 L <$ parseString "/") <|>
   (OpPrec Add 4 L <$ parseString "+") <|>
   (OpPrec Sub 4 L <$ parseString "-")) <*
  skipManyW

parseFunction :: Parser Function
parseFunction = do
  ty <- parseType
  skipW
  name <- parseIdentifier
  skipManyW
  parseString "()" <* skipManyW
  parseString "{" <* skipManyW
  body <- many (parseStatement)
  parseString "}" <* skipManyW
  return
    Function {typ = ty, name = name, formals = [], locals = [], body = body}

parseIfStat :: Parser Statement
parseIfStat = do
  parseString "if"
  skipManyW
  expr <-
    parseString "(" *> skipManyW *> parseExpr <* skipManyW <* parseString ")" <*
    skipManyW
  parseString "{" *> skipManyW
  stat1 <- many parseStatement <* skipManyW
  parseString "}" <* skipManyW
  return (If expr (Block stat1) (Block []))

parseProgram :: Parser Program
parseProgram = do
  funcs <- many parseFunction
  return $ Program [] [] funcs
