import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe


data Expression =
  EVar String |
  EPrefix Char Expression |
  EPostfix Char Expression |
  EInfix Char Expression Expression |
  ELiteral Literal 
 deriving Show

data Literal = 
  LitInt Integer |
  LitString String
 deriving Show

skipSpaces = optional spaces

oneOf' xs = do
  skipSpaces
  e <- oneOf xs
  skipSpaces 
  return e

manyOf' xs = do
  skipSpaces
  e <- many1 $ oneOf xs
  skipSpaces
  return e

parseVar :: Parser Expression
parseVar = do
  c <- manyOf' ['a'..'z']
  return $ EVar c

parseInfix :: Parser Expression
parseInfix = do
  char ','
  e1 <- parseExpression
  o <- oneOf' "!~+*/-"
  e2 <- parseExpression
  return $ EInfix o e1 e2

parsePrefix :: Parser Expression
parsePrefix = do
  o <- oneOf' "!~+"
  e1 <- parseExpression
  return $ EPrefix o e1

parsePostfix :: Parser Expression 
parsePostfix = do
  char '.'
  e1 <- parseExpression
  o <- oneOf' "!~+-"
  return $ EPostfix o e1

parseExpression :: Parser Expression
parseExpression = do
  skipSpaces
  e <- ((parseInfix) <|> (parsePrefix) <|> (parsePostfix) <|> (parseVar) <|> parseLiteral)
  return e

parseExpressions = many1 $ parseExpression

parseLiteral = parseNumber

parseNumber = parseIntVal

parseIntVal :: Parser Expression
parseIntVal = do
  n <- optionMaybe $ char '-'
  dg <- many1 $ oneOf ['0'..'9']
  let sign = fromMaybe '0' n
  optional spaces
  return $ ELiteral $ LitInt (read $ sign : dg)

runParserWithString p input = 
  case parse p "" input of
    Left err -> error $ show err
    Right q -> q

type GulfState = ([Literal],(M.Map String Literal))

type MonadGulf a = StateT GulfState IO a

run xs = do
 st <- evalStateT (eval xs) ([], M.fromList [])
 print st

push :: Literal -> MonadGulf ()
push x = do
  (stack, globals) <- get
  put (x : stack, globals)

eval :: Expression -> MonadGulf Literal
eval (EPrefix prefix (ELiteral lit)) = evalPrefix prefix lit
eval (EPrefix prefix e) = do 
  q <- eval e
  eval (EPrefix prefix (ELiteral q))
eval (EInfix inf (ELiteral lit1) (ELiteral lit2)) = evalInfix inf lit1 lit2
eval (EInfix inf e (ELiteral lit2)) = do 
  q <- eval e
  eval (EInfix inf (ELiteral q) (ELiteral lit2))
eval (EInfix inf (ELiteral lit1) e) = do 
  q <- eval e
  eval (EInfix inf (ELiteral lit1) (ELiteral q))
eval (EInfix inf c e) = do 
  q <- eval e
  z <- eval c
  eval (EInfix inf (ELiteral z) (ELiteral q))
eval (EPostfix postfix (ELiteral lit)) = evalPostfix postfix lit
eval (EPostfix postfix e) = do 
  q <- eval e
  eval (EPostfix postfix (ELiteral q))

errType xs = error $ "Wrong type for " ++ xs

evalPrefix '+' (LitInt i) = return $ (LitInt (i + 1))
evalPrefix '+' _ = errType "+"

{- START INFIX -}
evalInfix '+' (LitInt a) (LitInt b) = return $ (LitInt (a + b))

evalInfix '-' (LitInt a) (LitInt b) = return $ (LitInt (a - b))

evalInfix '*' (LitInt a) (LitInt b) = return $ (LitInt (a * b))

evalInfix '/' (LitInt a) (LitInt b) = return $ (LitInt (a `div` b))
{- END INFIX -}

evalPostfix '-' (LitInt i)
 |i <= 0 = return (LitInt $ i)
 |otherwise = return (LitInt $ i * (-1))
evalPostfix '-' _ = errType "+"
