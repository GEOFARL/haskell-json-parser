module Main where

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

type Parser a = String -> Maybe (String, a)

main :: IO ()
main = undefined