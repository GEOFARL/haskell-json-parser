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

jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser f
  where
    f(y:ys)
        | y == x = Just(ys, x)
        | otherwise = Nothing
    f [] = Nothing

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined