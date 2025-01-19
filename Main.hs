module Main where
import Control.Applicative (Alternative (empty))
import GHC.Base (Alternative((<|>)))

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
jsonValue = undefined

main :: IO ()
main = undefined