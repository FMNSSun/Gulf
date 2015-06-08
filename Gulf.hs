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
  EDyadic Char Expression Expression |
  ELiteral Literal 
 deriving Show

data Literal = 
  LitInt Integer |
  LitDouble Double | 
  LitString String |
  LitList [Literal] |
  LitBlock Expression

instance Show Literal where
  show (LitInt i) = show i
  show (LitDouble d) = show d
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
  
ops = "!+~?*/-:|\\" ++ ['A'..'Z']
sops = ['a'..'z']

parseVar :: Parser Expression
parseVar = (do
  a <- char 'x'
  c <- manyOf' ['a'..'z']
  return $ EVar $ a : c) <|> (do
  c <- oneOf' ['a'..'z']
  return $ EVar $ [c]
  )

parseInfix :: Parser Expression
parseInfix = (do
  char ','
  e1 <- parseExpression
  o <- oneOf' ops
  e2 <- parseExpression
  return $ EInfix o e1 e2)

parseDyadic :: Parser Expression
parseDyadic = try (do
  char '"'
  o <- oneOf' (ops ++ sops)
  e1 <- parseExpression
  e2 <- parseExpression
  return $ EDyadic o e1 e2) <|> (do
   char '"'
   e1 <- parseExpression
   o <- oneOf' (ops ++ sops)
   e2 <- parseExpression
   return $ EDyadic o e1 e2)

parsePrefix :: Parser Expression
parsePrefix = do
  o <- oneOf' ops
  e1 <- parseExpression
  return $ EPrefix o e1

parsePostfix :: Parser Expression 
parsePostfix = do
  char '.'
  e1 <- parseExpression
  o <- oneOf' ops
  return $ EPostfix o e1

parseExpression :: Parser Expression
parseExpression = do
  skipSpaces
  e <- ((parseInfix) <|> (parseDyadic) <|> (parsePrefix) <|> (parsePostfix) <|> (parseVar) <|> parseLiteral)
  skipSpaces
  return e

parseExpressions = many1 $ parseExpression

parseLiteral = parseNumber <|> parseBlock <|> parseString

parseNumber = (try parseDoubleVal) <|> parseIntVal

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
  
parseDoubleVal :: Parser Expression
parseDoubleVal = do
  n <- optionMaybe $ char '_'
  dg <- many1 $ oneOf ['0'..'9']
  char '.'
  dg2 <- many1 $ oneOf ['0'..'9']
  let sign = if isJust n then '-' else '0'
  optional spaces
  return $ ELiteral $ LitDouble (read $ [sign] ++ dg ++ "." ++ dg2)

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
eval (EDyadic dy (ELiteral lit1) (ELiteral lit2)) = evalDyadic dy lit1 lit2
eval (EDyadic dy e (ELiteral lit2)) = do 
  q <- eval e
  eval (EDyadic dy (ELiteral q) (ELiteral lit2))
eval (EDyadic dy (ELiteral lit1) e) = do 
  q <- eval e
  eval (EDyadic dy (ELiteral lit1) (ELiteral q))
eval (EDyadic dy c e) = do 
  q <- eval e
  z <- eval c
  eval (EDyadic dy (ELiteral z) (ELiteral q))
eval d = error $ show d

errType xs = error $ "Wrong type for " ++ xs

toStr (LitString s) = s
toStr (LitInt i) = show i

{- START PREFIX -}
evalPrefix '+' (LitInt i) = return $ (LitInt (i + 1))
evalPrefix '+' (LitDouble d) = return $ (LitDouble (d + 1))

evalPrefix '|' (LitInt i) = return $ (LitInt $ abs i)
evalPrefix '|' (LitDouble d) = return $ (LitDouble $ abs d)

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

evalPrefix o _ = error $ "Unknown prefix operator " ++ (show o)
{- END PREFIX -}

{- START DYADIC -}
evalDyadic 'M' (LitInt a) (LitInt b) = return $ (LitInt (max a b))
evalDyadic 'M' (LitDouble a) (LitInt b) = return $ (LitDouble (max a (fromIntegral b)))
evalDyadic 'M' (LitInt a) (LitDouble b) = return $ (LitDouble (max (fromIntegral a) b))
evalDyadic 'M' (LitDouble a) (LitDouble b) = return $ (LitDouble (max a b))

evalDyadic 'm' (LitInt a) (LitInt b) = return $ (LitInt (min a b))
evalDyadic 'm' (LitDouble a) (LitInt b) = return $ (LitDouble (min a (fromIntegral b)))
evalDyadic 'm' (LitInt a) (LitDouble b) = return $ (LitDouble (min (fromIntegral a) b))
evalDyadic 'm' (LitDouble a) (LitDouble b) = return $ (LitDouble (min a b))

evalDyadic o _ _ = error $ "Unknown dyadic_II operator " ++ (show o)

{- END DYADIC -}

{- START INFIX -}
evalInfix '+' (LitInt a) (LitInt b) = return $ (LitInt (a + b))
evalInfix '+' (LitInt a) (LitDouble b) = return $ (LitDouble $ (fromIntegral a) + b)
evalInfix '+' (LitDouble a) (LitInt b) = return $ (LitDouble $ a + (fromIntegral b))
evalInfix '+' (LitDouble a) (LitDouble b) = return $ LitDouble (a + b)

evalInfix '-' (LitInt a) (LitInt b) = return $ (LitInt (a - b))
evalInfix '-' (LitInt a) (LitDouble b) = return $ (LitDouble $ (fromIntegral a) - b)
evalInfix '-' (LitDouble a) (LitInt b) = return $ (LitDouble $ a - (fromIntegral b))
evalInfix '-' (LitDouble a) (LitDouble b) = return $ LitDouble (a - b)

evalInfix '*' (LitInt a) (LitInt b) = return $ (LitInt (a * b))

evalInfix '/' (LitInt a) (LitInt b) = return $ (LitInt (a `div` b))

evalInfix ':' (LitInt a) (LitInt b) = return $ LitList $ map LitInt $ [a..b]

evalInfix 'M' (LitList a) (LitBlock b) = do
  result <- gulfMap b a
  return $ LitList result

evalInfix 'R' (LitList a) (LitBlock b) = do
  result <- gulfReduce b a
  return $ result

evalInfix 'I' (LitList a) b = return $ LitList (intersperse b a)

evalInfix 'C' (LitList a) (LitInt b) = return $ LitList $ map LitList $ (chunksOf' b a)
evalInfix 'C' (LitString a) (LitInt b) = return $ LitList $ map LitString $ (chunksOf' b a)

evalInfix o _ _ = error $ "Unknown dyadic_I operator " ++ (show o)

{- END INFIX -}

evalPostfix '-' (LitInt i)
 |i <= 0 = return (LitInt $ i)
 |otherwise = return (LitInt $ i * (-1))
evalPostfix '-' (LitDouble d)
 |d <= 0 = return (LitDouble $ d)
 |otherwise = return (LitDouble $ d * (-1))
 
evalPostfix '+' (LitInt i) = return (LitInt (i * (-1)))
evalPostfix '+' (LitDouble d) = return (LitDouble (d * (-1)))

evalPostfix '*' (LitInt i) = return (LitDouble (1.0 / (fromIntegral i)))
evalPostfix '*' (LitDouble d) = return (LitDouble (1.0 / d))

evalPostfix o _ = error $ "Unknown postfix operator " ++ (show o)


gulfMap exp [] = return []
gulfMap exp (x:xs) = do
  push x
  result <- eval exp
  rest <- gulfMap exp xs
  return $ result : rest

gulfReduce exp [a] = return a
gulfReduce exp (a:b:xs) = do
  push a
  push b
  result <- eval exp
  gulfReduce exp (result : xs)

chunksOf' _ [] = []
chunksOf' n xs = genericTake n xs : chunksOf' n (genericDrop n xs)
