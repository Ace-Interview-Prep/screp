{-# LANGUAGE DeriveGeneric #-}

module Scrappy.Grep.DSL
  ( ParserExpr(..)
  , MatchResult(..)
  , MatchContext(..)
  , containsRef
  , extractRefs
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
  , mrContext   :: Maybe MatchContext  -- Optional context lines
  } deriving (Show, Eq, Generic)

-- | Context lines around a match
data MatchContext = MatchContext
  { mcBefore :: [String]    -- Lines before the match
  , mcAfter  :: [String]    -- Lines after the match
  } deriving (Show, Eq, Generic)

-- | Check if a ParserExpr contains any PRef nodes
containsRef :: ParserExpr -> Bool
containsRef expr = case expr of
  PRef _           -> True
  PSeq a b         -> containsRef a || containsRef b
  PSeqConcat a b   -> containsRef a || containsRef b
  PAlt a b         -> containsRef a || containsRef b
  PMany a          -> containsRef a
  PSome a          -> containsRef a
  POptional a      -> containsRef a
  PTry a           -> containsRef a
  PBetween _ _ a   -> containsRef a
  PCount _ a       -> containsRef a
  PManyTill a b    -> containsRef a || containsRef b
  _                -> False

-- | Extract all ref names from a ParserExpr
extractRefs :: ParserExpr -> [String]
extractRefs expr = case expr of
  PRef name        -> [name]
  PSeq a b         -> extractRefs a ++ extractRefs b
  PSeqConcat a b   -> extractRefs a ++ extractRefs b
  PAlt a b         -> extractRefs a ++ extractRefs b
  PMany a          -> extractRefs a
  PSome a          -> extractRefs a
  POptional a      -> extractRefs a
  PTry a           -> extractRefs a
  PBetween _ _ a   -> extractRefs a
  PCount _ a       -> extractRefs a
  PManyTill a b    -> extractRefs a ++ extractRefs b
  _                -> []
