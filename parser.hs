import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- standard
-- parseString :: Parser LispVal
-- parseString = do
--     char '"'
--     x <- many $ noneOf "\""
--     char '"'
--     return $ String x

-- exercise 1.2
-- parseString :: Parser LispVal
-- parseString = do
--     char '"'
--     x <- many $ escapedQuote <|> noneOf "\""
--     char '"'
--     return $ String x

-- escapedQuote :: Parser Char
-- escapedQuote = char '\\' >> char '"'

-- exercise 1.3
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChar <|> noneOf "\""
                 char '"'
                 return $ String x

escapedChar :: Parser Char
escapedChar = do char '\\'
                 x <- oneOf "\\\"ntr"
                 return $ getEscapedChar x

getEscapedChar :: Char -> Char
getEscapedChar c | c == '\\' = '\\'
                 | c ==  '"' = '"'
                 | c ==  'n' = '\n'
                 | c ==  't' = '\t'
                 | c ==  'r' = '\r'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               -- exercise 1.4
               '#':'x':_ -> Number $ convertNumber rest
               '#':'b':_ -> Number $ convertNumber rest
               '#':'o':_ -> Number $ convertNumber rest
               '#':'d':_ -> Number $ convertNumber rest
               _    -> Atom atom

convertNumber :: String -> Integer
convertNumber (x:xs) = case x of
                         'x' -> fst $ head $ readHex xs
                         'd' -> read xs
                         'o' -> fst $ head $ readOct xs
                         --                         'b' ->   TODO !!!!!!!!!!!!!

-- standard
-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- exercise 1.1.1
-- parseNumber :: Parser LispVal
-- parseNumber = do
--     digits <- many1 digit
--     return $ (Number . read) digits

-- exercise 1.1.2
parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

showVal :: LispVal -> String
showVal (String contents) = "String: \"" ++ contents ++ "\""
showVal (Atom name) = "Atom: " ++ name
showVal (Number contents) = "Number: " ++ show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (expr)
    putStrLn (readExpr expr)
