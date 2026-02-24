{-# LANGUAGE DeriveGeneric #-}

module Scrappy.Grep.DSL
  ( ParserExpr(..)
  , MatchResult(..)
  ) where

import GHC.Generics (Generic)

-- | AST representation of the parsec-grep DSL
data ParserExpr
  -- Primitives
  = PChar Char              -- char 'x'
  | PString String          -- string "abc"
  | PAnyChar                -- anyChar
  | PDigit                  -- digit
  | PLetter                 -- letter
  | PAlphaNum               -- alphaNum
  | PSpace                  -- space
  | PSpaces                 -- spaces
  | PNewline                -- newline
  | POneOf String           -- oneOf "abc"
  | PNoneOf String          -- noneOf "abc"

  -- Combinators
  | PSeq ParserExpr ParserExpr       -- p1 >> p2 (sequence, return second)
  | PSeqConcat ParserExpr ParserExpr -- p1 <+> p2 (sequence, concatenate results)
  | PAlt ParserExpr ParserExpr       -- p1 <|> p2 (alternative)
  | PMany ParserExpr                 -- many p (zero or more)
  | PSome ParserExpr                 -- some p (one or more)
  | POptional ParserExpr             -- optional p (zero or one)
  | PTry ParserExpr                  -- try p (backtracking)
  | PBetween Char Char ParserExpr    -- between '(' ')' p
  | PCount Int ParserExpr            -- count 3 p
  | PManyTill ParserExpr ParserExpr  -- manyTill p end (non-greedy)

  -- References to named parsers (from config file)
  | PRef String                      -- ref "email"
  deriving (Show, Eq, Generic)

-- | A match result with position information
data MatchResult = MatchResult
  { mrFilePath  :: FilePath
  , mrLine      :: Int       -- Starting line (1-indexed)
  , mrCol       :: Int       -- Starting column (1-indexed)
  , mrMatchText :: String    -- The matched text (may contain newlines)
  } deriving (Show, Eq, Generic)
