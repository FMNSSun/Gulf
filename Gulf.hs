import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Char


data Expression =
  EVar String |
  EPrefix Char Expression |
  EPostfix Char Expression |
  EInfix Char Expression Expression |
  ELiteral Literal 
 deriving Show

data Literal = 
  LitInt Integer |
  LitString String |
  LitList [Literal] |
  LitBlock Expression

instance Show Literal where
  show (LitInt i) = show i
  show (LitString s) = s
  show (LitList xs) = "[" ++ intercalate " " (map show xs) ++ "]"

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

char' x = do
  skipSpaces
  e <- char x
  return e

parseVar :: Parser Expression
parseVar = do
  c <- manyOf' ['a'..'z']
  return $ EVar c

parseInfix :: Parser Expression
parseInfix = do
  char ','
  e1 <- parseExpression
  o <- oneOf' "!~+*/-:MIC"
  e2 <- parseExpression
  return $ EInfix o e1 e2

parsePrefix :: Parser Expression
parsePrefix = do
  o <- oneOf' "C!~+LU:\\"
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
  skipSpaces
  return e

parseExpressions = many1 $ parseExpression

parseLiteral = parseNumber <|> parseBlock <|> parseString

parseNumber = parseIntVal

parseString :: Parser Expression
parseString = do
  char' '\''
  str <- many $ noneOf "\"'"
  char' '\''
  optional spaces
  return $ ELiteral $ LitString $ unescape str
 where unescape ('\\' : '\\' : xs) = '\\' : unescape xs
       unescape ('\\' : 'D' : xs) = let str = decode xs in (chr (read str)) : unescape (drop (succ (length str)) xs)
       unescape (x : xs) = x : unescape xs
       unescape [] = []
       decode (x : xs)
         |x `elem` ['0'..'9'] = x : decode xs
         |x == ';' = []
         |otherwise = error "Invalid escape sequence!"
       decode [] = error "Invalid escape sequence!"

parseBlock = do
  char' '{'
  e <- parseExpression
  optional $ char' '}'
  return $ ELiteral $ LitBlock e

parseIntVal :: Parser Expression
parseIntVal = do
  n <- optionMaybe $ char '_'
  dg <- many1 $ oneOf ['0'..'9']
  let sign = if isJust n then '-' else '0'
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

pop :: MonadGulf Literal
pop = do
  ((x:xs), globals) <- get
  put (xs, globals)
  return x

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
eval (ELiteral lit) = return lit
eval (EVar "p") = do
 p <- pop
 return p
eval d = error $ show d

errType xs = error $ "Wrong type for " ++ xs

toStr (LitString s) = s
toStr (LitInt i) = show i

{- START PREFIX -}
evalPrefix '+' (LitInt i) = return $ (LitInt (i + 1))

evalPrefix 'L' (LitString s) = return $ LitList $ (map LitString) $ lines s

evalPrefix 'U' (LitList l) = return $ LitString $ unlines (map toStr l)

evalPrefix '~' (LitList l) = return $ LitList (reverse l)
evalPrefix '~' (LitString l) = return $ LitString (reverse l)

evalPrefix ':' (LitInt i) = return $ LitList $ map LitInt $ [1..i]

evalPrefix 'C' (LitList l) = return $ LitList $ concat' l
 where concat' [] = []
       concat' (LitList a : LitList b : xs) = (a ++ b) ++ concat' xs
       concat' (LitList a : b : xs) = (a ++ [b]) ++ concat' xs
       concat' (a : LitList b : xs) = ([a] ++ b) ++ concat' xs
       concat' (a : b : xs) = ([a] ++ [b]) ++ concat' xs
       concat' [(LitList a)] = a
       concat' [a] = [a]

evalPrefix '\\' (LitList l) = return $ LitString $ concat' l
 where concat' [] = ""
       concat' [LitString s] = s
       concat' (LitString s : xs) = s ++ concat' xs
{- END PREFIX -}

{- START INFIX -}
evalInfix '+' (LitInt a) (LitInt b) = return $ (LitInt (a + b))

evalInfix '-' (LitInt a) (LitInt b) = return $ (LitInt (a - b))

evalInfix '*' (LitInt a) (LitInt b) = return $ (LitInt (a * b))

evalInfix '/' (LitInt a) (LitInt b) = return $ (LitInt (a `div` b))

evalInfix ':' (LitInt a) (LitInt b) = return $ LitList $ map LitInt $ [a..b]

evalInfix 'M' (LitList a) (LitBlock b) = do
  result <- gulfMap b a
  return $ LitList result

evalInfix 'I' (LitList a) b = return $ LitList (intersperse b a)

evalInfix 'C' (LitList a) (LitInt b) = return $ LitList $ map LitList $ (chunksOf' b a)
evalInfix 'C' (LitString a) (LitInt b) = return $ LitList $ map LitString $ (chunksOf' b a)

{- END INFIX -}

evalPostfix '-' (LitInt i)
 |i <= 0 = return (LitInt $ i)
 |otherwise = return (LitInt $ i * (-1))
evalPostfix '-' _ = errType "+"

gulfMap exp [] = return []
gulfMap exp (x:xs) = do
  push x
  result <- eval exp
  rest <- gulfMap exp xs
  return $ result : rest

chunksOf' _ [] = []
chunksOf' n xs = genericTake n xs : chunksOf' n (genericDrop n xs)
