module Scrappy.Grep.DSL.Parser
  ( parseExpr
  , ParseError
  ) where

import Scrappy.Grep.DSL

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>), many)
import Control.Monad (void)

-- | Parse a DSL string into an AST
parseExpr :: String -> Either ParseError ParserExpr
parseExpr = parse (spaces *> exprParser <* eof) "parsec-grep DSL"

-- | Main expression parser - handles combinators with proper precedence
-- Precedence (lowest to highest): <|>, >> / <+>, modifiers, atoms
exprParser :: Parser ParserExpr
exprParser = altExpr

-- | Alternative: p1 <|> p2
altExpr :: Parser ParserExpr
altExpr = chainl1 seqExpr altOp
  where
    altOp = PAlt <$ try (spaces *> string "<|>" <* spaces)

-- | Sequence: p1 >> p2 or p1 <+> p2
seqExpr :: Parser ParserExpr
seqExpr = chainl1 termExpr seqOp
  where
    seqOp = try (PSeqConcat <$ try (spaces *> string "<+>" <* spaces))
        <|> (PSeq <$ try (spaces *> string ">>" <* spaces))

-- | Term: modified expression or atom
termExpr :: Parser ParserExpr
termExpr = modifiedExpr <|> atomExpr

-- | Modifiers: many, some, optional, try
modifiedExpr :: Parser ParserExpr
modifiedExpr = do
  modifier <- try $ do
    m <- choice
      [ PMany <$ try (string "many")
      , PSome <$ try (string "some")
      , POptional <$ try (string "optional")
      , PTry <$ try (string "try")
      ]
    spaces1
    pure m
  inner <- termExpr
  pure $ modifier inner

-- | At least one space
spaces1 :: Parser ()
spaces1 = void $ many1 (oneOf " \t\n")

-- | Atom: primitive, reference, or parenthesized expression
atomExpr :: Parser ParserExpr
atomExpr = choice
  [ parenExpr
  , primitiveExpr
  , refExpr
  ]

-- | Parenthesized expression
parenExpr :: Parser ParserExpr
parenExpr = between (char '(' <* spaces) (spaces *> char ')') exprParser

-- | Reference to named parser: ref "name"
refExpr :: Parser ParserExpr
refExpr = do
  try $ string "ref"
  spaces
  PRef <$> stringLiteral

-- | Primitive parsers
primitiveExpr :: Parser ParserExpr
primitiveExpr = choice
  [ try manyTillExpr
  , try betweenExpr
  , try countExpr
  , try charExpr
  , try stringExpr
  , try oneOfExpr
  , try noneOfExpr
  , try $ PAlphaNum <$ string "alphaNum"  -- must come before 'alpha' prefix match
  , try $ PAnyChar <$ string "anyChar"
  , try $ PDigit <$ string "digit"
  , try $ PLetter <$ string "letter"
  , try $ PSpaces <$ string "spaces"      -- must come before 'space'
  , try $ PSpace <$ string "space"
  , try $ PNewline <$ string "newline"
  ]

-- | char 'x'
charExpr :: Parser ParserExpr
charExpr = do
  string "char"
  spaces
  PChar <$> charLiteral

-- | string "abc"
stringExpr :: Parser ParserExpr
stringExpr = do
  string "string"
  spaces
  PString <$> stringLiteral

-- | oneOf "abc"
oneOfExpr :: Parser ParserExpr
oneOfExpr = do
  string "oneOf"
  spaces
  POneOf <$> stringLiteral

-- | noneOf "abc"
noneOfExpr :: Parser ParserExpr
noneOfExpr = do
  string "noneOf"
  spaces
  PNoneOf <$> stringLiteral

-- | between '(' ')' expr
betweenExpr :: Parser ParserExpr
betweenExpr = do
  string "between"
  spaces
  open <- charLiteral
  spaces
  close <- charLiteral
  spaces
  inner <- termExpr
  pure $ PBetween open close inner

-- | count 3 expr
countExpr :: Parser ParserExpr
countExpr = do
  _ <- string "count"
  spaces
  n <- read <$> many1 digit
  spaces
  inner <- termExpr
  pure $ PCount n inner

-- | manyTill p end - match p until end, non-greedy
manyTillExpr :: Parser ParserExpr
manyTillExpr = do
  _ <- string "manyTill"
  spaces
  p <- termExpr
  spaces
  end <- termExpr
  pure $ PManyTill p end

-- | Character literal: 'x' or '\n' etc
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') charContent
  where
    charContent = escapedChar <|> noneOf "'\\"

-- | String literal: "abc" with escape support
stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') (many stringChar)
  where
    stringChar = escapedChar <|> noneOf "\"\\"

-- | Escape sequences
escapedChar :: Parser Char
escapedChar = char '\\' >> choice
  [ '\n' <$ char 'n'
  , '\t' <$ char 't'
  , '\r' <$ char 'r'
  , '\\' <$ char '\\'
  , '\'' <$ char '\''
  , '"' <$ char '"'
  ]
