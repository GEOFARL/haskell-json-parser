module Main where
import Control.Applicative (Alternative (empty, many))
import GHC.Base (Alternative((<|>)))
import Data.Char (isDigit, isSpace)

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser (p1) <*> Parser (p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
                              in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull p = Parser $ \input -> do
  (input', xs) <- runParser p input
  if null xs
    then Nothing
    else Just (input', xs)

stringLiteral :: Parser String
stringLiteral = (charP '"' *> spanP (/= '"') <* charP '"')

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' 
            *> ws *>
            sepBy (ws *> charP ',' <* ws) pair
            <* ws
            <* charP '}')
  where pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull(spanP isDigit) 
  where
    f digits = JsonNumber $ read digits

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined

charP :: Char -> Parser Char
charP x = Parser f
  where
    f(y:ys)
        | y == x = Just(ys, x)
        | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP input = sequenceA $ map charP input

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = undefined