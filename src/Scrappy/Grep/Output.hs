module Scrappy.Grep.Output
  ( formatResult
  , formatResults
  , formatResultsWithOpts
  , formatResultsGrouped
  , OutputFormat(..)
  , OutputOptions(..)
  , defaultOutputOptions
  , ColorMode(..)
  ) where

import Scrappy.Grep.DSL (MatchResult(..), MatchContext(..))
import Data.List (intercalate, groupBy)
import Data.Function (on)

data OutputFormat
  = FormatGrep         -- file:line:col:match
  | FormatVerbose      -- file:line:col: match (with context)
  | FormatJSON         -- JSON output
  | FormatCount        -- Just count of matches
  | FormatFilesOnly    -- -l: just filenames with matches
  | FormatFilesWithout -- -L: just filenames without matches
  deriving (Show, Eq)

data ColorMode = ColorNever | ColorAlways | ColorAuto
  deriving (Show, Eq)

data OutputOptions = OutputOptions
  { ooColor      :: ColorMode
  , ooNoFilename :: Bool      -- -h: suppress filename
  } deriving (Show, Eq)

defaultOutputOptions :: OutputOptions
defaultOutputOptions = OutputOptions
  { ooColor = ColorNever
  , ooNoFilename = False
  }

-- ANSI color codes
colorRed, colorGreen, colorCyan, colorMagenta, colorReset :: String
colorRed     = "\ESC[1;31m"
colorGreen   = "\ESC[1;32m"
colorCyan    = "\ESC[1;36m"
colorMagenta = "\ESC[1;35m"
colorReset   = "\ESC[0m"

-- | Format a single match result
formatResult :: OutputFormat -> MatchResult -> String
formatResult = formatResultWithOpts defaultOutputOptions

-- | Format a single match result with options
formatResultWithOpts :: OutputOptions -> OutputFormat -> MatchResult -> String
formatResultWithOpts opts fmt mr = case fmt of
  FormatGrep ->
    let useColor = ooColor opts == ColorAlways
        file = if ooNoFilename opts then "" else colorIf useColor colorMagenta (mrFilePath mr) ++ ":"
        lineNum = colorIf useColor colorGreen (show (mrLine mr)) ++ ":"
        colNum = show (mrCol mr) ++ ":"
        match = colorIf useColor colorRed (escapeNewlines (mrMatchText mr))
    in file ++ lineNum ++ colNum ++ match

  FormatVerbose ->
    let useColor = ooColor opts == ColorAlways
        file = if ooNoFilename opts then "" else colorIf useColor colorMagenta (mrFilePath mr) ++ ":"
        lineNum = colorIf useColor colorGreen (show (mrLine mr)) ++ ":"
        colNum = show (mrCol mr) ++ ": "
        match = colorIf useColor colorRed (mrMatchText mr)
        ctx = formatContext useColor mr
    in ctx ++ file ++ lineNum ++ colNum ++ match

  FormatJSON ->
    "{\"file\":\"" ++ escapeJSON (mrFilePath mr) ++
    "\",\"line\":" ++ show (mrLine mr) ++
    ",\"col\":" ++ show (mrCol mr) ++
    ",\"match\":\"" ++ escapeJSON (mrMatchText mr) ++ "\"}"

  FormatCount -> ""
  FormatFilesOnly -> mrFilePath mr
  FormatFilesWithout -> mrFilePath mr

-- | Conditionally apply color
colorIf :: Bool -> String -> String -> String
colorIf False _ s = s
colorIf True color s = color ++ s ++ colorReset

-- | Format context lines
formatContext :: Bool -> MatchResult -> String
formatContext useColor mr = case mrContext mr of
  Nothing -> ""
  Just ctx ->
    let beforeStr = unlines $ map (\l -> colorIf useColor colorCyan "-" ++ l) (mcBefore ctx)
    in beforeStr

-- | Format multiple results
formatResults :: OutputFormat -> [MatchResult] -> String
formatResults = formatResultsWithOpts defaultOutputOptions

-- | Format multiple results with options
formatResultsWithOpts :: OutputOptions -> OutputFormat -> [MatchResult] -> String
formatResultsWithOpts _ FormatCount results =
  show (length results) ++ " match" ++ (if length results == 1 then "" else "es") ++ "\n"
formatResultsWithOpts _ FormatJSON results =
  "[" ++ intercalate "," (map (formatResult FormatJSON) results) ++ "]\n"
formatResultsWithOpts _ FormatFilesOnly results =
  unlines $ unique $ map mrFilePath results
formatResultsWithOpts _ FormatFilesWithout _ = ""  -- Handled specially in Main
formatResultsWithOpts opts fmt results =
  unlines $ map (formatResultWithOpts opts fmt) results

-- | Format results grouped by file with context (for -A/-B/-C)
formatResultsGrouped :: OutputOptions -> OutputFormat -> [MatchResult] -> String
formatResultsGrouped opts fmt results =
  let grouped = groupBy ((==) `on` mrFilePath) results
      useColor = ooColor opts == ColorAlways
      separator = if useColor then colorCyan ++ "--" ++ colorReset else "--"
  in intercalate (separator ++ "\n") $ map (formatGroup opts fmt) grouped

formatGroup :: OutputOptions -> OutputFormat -> [MatchResult] -> String
formatGroup opts fmt mrs =
  let useColor = ooColor opts == ColorAlways
  in concatMap (formatWithContext opts useColor fmt) mrs

formatWithContext :: OutputOptions -> Bool -> OutputFormat -> MatchResult -> String
formatWithContext opts useColor fmt mr =
  let beforeCtx = case mrContext mr of
        Nothing -> ""
        Just ctx -> unlines $ zipWith (formatCtxLine opts useColor (mrFilePath mr) (mrLine mr))
                              [mrLine mr - length (mcBefore ctx)..]
                              (mcBefore ctx)
      mainLine = formatResultWithOpts opts fmt mr ++ "\n"
      afterCtx = case mrContext mr of
        Nothing -> ""
        Just ctx -> unlines $ zipWith (formatCtxLine opts useColor (mrFilePath mr) (mrLine mr))
                              [mrLine mr + 1..]
                              (mcAfter ctx)
  in beforeCtx ++ mainLine ++ afterCtx

formatCtxLine :: OutputOptions -> Bool -> FilePath -> Int -> Int -> String -> String
formatCtxLine opts useColor fp _ lineNum content =
  let file = if ooNoFilename opts then "" else colorIf useColor colorMagenta fp ++ "-"
      num = colorIf useColor colorGreen (show lineNum) ++ "-"
  in file ++ num ++ content

-- | Get unique elements preserving order
unique :: Eq a => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

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
