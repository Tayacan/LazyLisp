module LazyLisp.Parser (parseText, parseFile) where

import Text.Parsec
import Text.Parsec.Char

import Data.Char (isAlpha, isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor.Foldable hiding (Nil, Cons)

import LazyLisp.ADT

type Parser = Parsec T.Text ()

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile p = parse' p <$> T.readFile p

parseText :: T.Text -> Either ParseError Program
parseText t = parse' "" t

parse' :: String -> T.Text -> Either ParseError Program
parse' = parse (spaces >> many1 def <* eof)

def :: Parser Def
def = try valdef <|> fundef

valdef :: Parser Def
valdef = parens $ do
  keyword "val"
  n <- ident
  e <- expr
  pure $ ValDef n e

fundef :: Parser Def
fundef = parens $ do
  keyword "fun"
  fn <- ident
  tp <- funcType
  pn <- parens ident
  e  <- expr
  pure $ FuncDef fn tp pn e

expr :: Parser Expr
expr = try ((Fix . Ident) <$> ident) <|>
       try literal <|>
       try cons <|>
       try case_ <|>
       try print_ <|>
       try lambda <|>
       try call <|>
       parens expr

tok :: Parser a -> Parser a
tok = (<* spaces)

keyword :: String -> Parser String
keyword = tok . string

parens :: Parser a -> Parser a
parens p = tok (char '(') *> p <* tok (char ')')

keywords :: [T.Text]
keywords =
  [ "fun"
  , "val"
  , "case"
  , "nil"
  , "cons"
  , "true"
  , "false"
  , "print"
  , "lambda"
  ]

ident :: Parser T.Text
ident = tok (do
    s <- T.pack <$> many1 (satisfy isAlpha)
    if s `elem` keywords
          then parserZero
          else pure s
  <?> "identifier")

number :: Parser Int
number = read <$> tok (many1 $ satisfy isDigit) <?> "number literal"

boolean :: Parser Expr
boolean = (keyword "true"  >> pure (Fix TrueVal)) <|>
          (keyword "false" >> pure (Fix FalseVal)) <?> "boolean literal"

nil :: Parser Expr
nil = keyword "nil" >> pure (Fix Nil)

literal :: Parser Expr
literal = ((Fix . IntVal) <$> number <|> boolean <|> nil) <?> "literal"

cons :: Parser Expr
cons = parens $ do
  keyword "cons"
  x <- expr
  xs <- expr
  pure . Fix $ Cons x xs

call :: Parser Expr
call = parens $ do
  f <- expr
  xs <- many1 expr
  pure $ foldl ((Fix .) . FunCall) f xs

print_ :: Parser Expr
print_ = parens $ do
  keyword "print"
  a <- expr
  b <- expr
  pure . Fix $ Print a b

lambda :: Parser Expr
lambda = parens $ do
  keyword "lambda"
  tp <- funcType
  n <- parens ident
  e <- expr
  pure . Fix $ Lambda tp n e

type_ :: Parser Tp
type_ = (try (FTp <$> funcType)
     <|> try (BTp <$> basicType)
     <|> TypeVar <$> ident) <?> "type"

basicType :: Parser BasicTp
basicType = ((keyword "Int" >> pure IntTp)
         <|> (keyword "Bool" >> pure BoolTp)
         <|> (keyword "List" >> ListTp <$> type_)) <?> "basic type"

funcType :: Parser FuncTp
funcType = (parens $ do
  t1 <- type_
  keyword "->"
  t2 <- type_
  pure (t1, t2)) <?> "function type"

case_ :: Parser Expr
case_ = parens $ do
  keyword "case"
  x <- expr
  ps <- many1 patExp
  pure . Fix $ Case x ps

patExp :: Parser (PatternExpr Expr)
patExp = parens $ do
  p <- pattern
  e <- expr
  pure (p, e)

pattern :: Parser Pattern
pattern = try (namePat <|> nilPat <|> boolPat <|> intPat) <|> consPat <|> parens pattern

namePat :: Parser Pattern
namePat = try (NamePattern <$> ident)

consPat :: Parser Pattern
consPat = parens $ do
  keyword "cons"
  x <- pattern
  xs <- pattern
  pure (ConsPattern x xs)

intPat :: Parser Pattern
intPat = (IntPattern <$> number) <?> "number pattern"

nilPat :: Parser Pattern
nilPat = keyword "nil" >> pure NilPattern <?> "nil pattern"

boolPat :: Parser Pattern
boolPat = (keyword "true"  >> pure TruePattern) <|>
          (keyword "false" >> pure FalsePattern) <?> "boolean pattern"
