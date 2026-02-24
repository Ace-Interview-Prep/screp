module Parsers (parsers) where

import Text.Parsec
import Text.Parsec.String
import Data.Map (Map)
import qualified Data.Map as Map

parsers :: Map String (Parser String)
parsers = Map.fromList
  [ ("email", email)
  , ("phone", phone)
  ]

email :: Parser String
email = do
  user <- many1 alphaNum
  char '@'
  domain <- many1 alphaNum
  char '.'
  tld <- many1 letter
  pure $ user ++ "@" ++ domain ++ "." ++ tld

phone :: Parser String
phone = do
  a <- count 3 digit
  char '-'
  b <- count 3 digit
  char '-'
  c <- count 4 digit
  pure $ a ++ "-" ++ b ++ "-" ++ c
