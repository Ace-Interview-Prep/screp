{-# LANGUAGE FlexibleContexts #-}

module Scrappy.Grep.DSL.Interpreter
  ( interpret
  , interpretWithRefs
  , InterpreterError(..)
  ) where

import Scrappy.Grep.DSL
import Scrappy.Scrape (ScraperT)

import Text.Parsec hiding ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))

data InterpreterError
  = UnknownRef String
  deriving (Show, Eq)

-- | Interpret AST without named references (refs will fail)
interpret :: ParserExpr -> Either InterpreterError (ScraperT String)
interpret = interpretWithRefs Map.empty

-- | Interpret AST with named parser references
-- The Map contains pre-interpreted parsers for each ref name
interpretWithRefs :: Map String (ScraperT String) -> ParserExpr -> Either InterpreterError (ScraperT String)
interpretWithRefs refs = go
  where
    go :: ParserExpr -> Either InterpreterError (ScraperT String)
    go expr = case expr of
      -- Primitives (return matched text as String)
      PChar c     -> Right $ (:[]) <$> char c
      PString s   -> Right $ string s
      PAnyChar    -> Right $ (:[]) <$> anyChar
      PDigit      -> Right $ (:[]) <$> digit
      PLetter     -> Right $ (:[]) <$> letter
      PAlphaNum   -> Right $ (:[]) <$> alphaNum
      PSpace      -> Right $ (:[]) <$> space
      PSpaces     -> Right $ many space
      PNewline    -> Right $ (:[]) <$> newline
      POneOf cs   -> Right $ (:[]) <$> oneOf cs
      PNoneOf cs  -> Right $ (:[]) <$> noneOf cs

      -- Sequence: run p1 then p2, return p2's result
      PSeq p1 p2 -> do
        parser1 <- go p1
        parser2 <- go p2
        Right $ parser1 *> parser2

      -- Sequence with concatenation: run both, concatenate results
      PSeqConcat p1 p2 -> do
        parser1 <- go p1
        parser2 <- go p2
        Right $ (++) <$> parser1 <*> parser2

      -- Alternative: try p1, if fails try p2
      PAlt p1 p2 -> do
        parser1 <- go p1
        parser2 <- go p2
        Right $ try parser1 <|> parser2

      -- Many: zero or more
      PMany p -> do
        parser <- go p
        Right $ concat <$> many (try parser)

      -- Some: one or more
      PSome p -> do
        parser <- go p
        Right $ concat <$> many1 (try parser)

      -- Optional: zero or one
      POptional p -> do
        parser <- go p
        Right $ option "" (try parser)

      -- Try: backtracking
      PTry p -> do
        parser <- go p
        Right $ try parser

      -- Between: parse between delimiters
      PBetween open close p -> do
        parser <- go p
        Right $ do
          o <- char open
          result <- parser
          c <- char close
          pure $ [o] ++ result ++ [c]

      -- Count: exactly n repetitions
      PCount n p -> do
        parser <- go p
        Right $ concat <$> count n parser

      -- ManyTill: non-greedy match until end
      PManyTill p end -> do
        parserP <- go p
        parserEnd <- go end
        Right $ manyTillConcat parserP parserEnd

      -- Reference: lookup in refs map
      PRef name -> case Map.lookup name refs of
        Nothing -> Left $ UnknownRef name
        Just parser -> Right parser

-- | manyTill that concatenates all matched strings including the end
manyTillConcat :: ScraperT String -> ScraperT String -> ScraperT String
manyTillConcat p end = go
  where
    go = (try end) <|> do
      x <- p
      xs <- go
      pure (x ++ xs)
