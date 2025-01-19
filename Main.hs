module Main where

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


jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser f
  where
    f(y:ys)
        | y == x = Just(ys, x)
        | otherwise = Nothing
    f [] = Nothing

-- stringP :: String -> Parser String
-- stringP input = sequenceA $ map charP input

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined