module Scrappy.Grep.Output
  ( formatResult
  , formatResults
  , OutputFormat(..)
  ) where

import Scrappy.Grep.DSL (MatchResult(..))
import Data.List (intercalate)

data OutputFormat
  = FormatGrep      -- file:line:col:match
  | FormatVerbose   -- file:line:col: match (with context)
  | FormatJSON      -- JSON output
  | FormatCount     -- Just count of matches
  deriving (Show, Eq)

-- | Format a single match result
formatResult :: OutputFormat -> MatchResult -> String
formatResult fmt mr = case fmt of
  FormatGrep ->
    mrFilePath mr ++ ":" ++
    show (mrLine mr) ++ ":" ++
    show (mrCol mr) ++ ":" ++
    escapeNewlines (mrMatchText mr)

  FormatVerbose ->
    mrFilePath mr ++ ":" ++
    show (mrLine mr) ++ ":" ++
    show (mrCol mr) ++ ": " ++
    mrMatchText mr

  FormatJSON ->
    "{\"file\":\"" ++ escapeJSON (mrFilePath mr) ++
    "\",\"line\":" ++ show (mrLine mr) ++
    ",\"col\":" ++ show (mrCol mr) ++
    ",\"match\":\"" ++ escapeJSON (mrMatchText mr) ++ "\"}"

  FormatCount -> ""

-- | Format multiple results
formatResults :: OutputFormat -> [MatchResult] -> String
formatResults FormatCount results =
  show (length results) ++ " match" ++ (if length results == 1 then "" else "es")
formatResults FormatJSON results =
  "[" ++ intercalate "," (map (formatResult FormatJSON) results) ++ "]"
formatResults fmt results =
  unlines $ map (formatResult fmt) results

-- | Escape newlines for single-line output
escapeNewlines :: String -> String
escapeNewlines = concatMap escape
  where
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape c = [c]

-- | Escape for JSON strings
escapeJSON :: String -> String
escapeJSON = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape c = [c]
