{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Scrappy.Grep.Search
  ( searchFile
  , searchFiles
  , searchFileWithOpts
  , searchFilesWithOpts
  , searchText
  , searchTextWithContext
  , offsetToLineCol
  , SearchOptions(..)
  , defaultSearchOptions
  ) where

import Scrappy.Grep.DSL (MatchResult(..), MatchContext(..))
import Scrappy.Scrape (ScraperT)

import Text.Parsec (parse, getPosition, sourceColumn, sourceLine, try, anyChar, (<|>), eof)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Data.Char (toLower)

-- | Search options
data SearchOptions = SearchOptions
  { soIgnoreCase    :: Bool   -- -i
  , soContextBefore :: Int    -- -B
  , soContextAfter  :: Int    -- -C
  } deriving (Show, Eq)

defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
  { soIgnoreCase = False
  , soContextBefore = 0
  , soContextAfter = 0
  }

-- | Search a file for all matches, returning results with positions
-- Silently skips binary files and files that can't be read
searchFile :: ScraperT String -> FilePath -> IO [MatchResult]
searchFile = searchFileWithOpts defaultSearchOptions

-- | Search a file with options
searchFileWithOpts :: SearchOptions -> ScraperT String -> FilePath -> IO [MatchResult]
searchFileWithOpts opts parser fp = do
  exists <- doesFileExist fp
  if not exists
    then pure []
    else readAndSearch `catch` handleError
  where
    readAndSearch = do
      content <- readFile fp
      -- Force evaluation to catch encoding errors early
      let !_ = length content
      pure $ searchTextWithContext opts fp parser content

    handleError :: IOException -> IO [MatchResult]
    handleError _ = pure []  -- Skip files we can't read (binary, permissions, etc.)

-- | Search multiple files
searchFiles :: ScraperT String -> [FilePath] -> IO [MatchResult]
searchFiles = searchFilesWithOpts defaultSearchOptions

-- | Search multiple files with options
searchFilesWithOpts :: SearchOptions -> ScraperT String -> [FilePath] -> IO [MatchResult]
searchFilesWithOpts opts parser fps = concat <$> mapM (searchFileWithOpts opts parser) fps

-- | Search text content and return matches with positions (no context)
searchText :: FilePath -> ScraperT String -> String -> [MatchResult]
searchText = searchTextWithContext defaultSearchOptions

-- | Search text content with context support
searchTextWithContext :: SearchOptions -> FilePath -> ScraperT String -> String -> [MatchResult]
searchTextWithContext opts fp parser content =
  let contentToSearch = if soIgnoreCase opts then map toLower content else content
      contentLines = lines content
  in case parse (findAllWithPos parser) "" contentToSearch of
       Left _ -> []
       Right matches -> map (toMatchResultWithContext opts contentLines fp) matches

-- | Find all matches with their positions
findAllWithPos :: ScraperT String -> ScraperT [(Int, Int, String)]
findAllWithPos parser = go
  where
    go = do
      atEnd <- (True <$ eof) <|> pure False
      if atEnd
        then pure []
        else tryMatch <|> skipAndContinue

    tryMatch = do
      pos <- getPosition
      let line = sourceLine pos
          col = sourceColumn pos
      matched <- try parser
      rest <- go
      pure $ (line, col, matched) : rest

    skipAndContinue = do
      _ <- anyChar
      go

-- | Convert (line, col, match) to MatchResult with optional context
toMatchResultWithContext :: SearchOptions -> [String] -> FilePath -> (Int, Int, String) -> MatchResult
toMatchResultWithContext opts contentLines fp (line, col, matched) =
  let ctx = if soContextBefore opts > 0 || soContextAfter opts > 0
            then Just $ getContext opts contentLines line
            else Nothing
  in MatchResult
       { mrFilePath = fp
       , mrLine = line
       , mrCol = col
       , mrMatchText = matched
       , mrContext = ctx
       }

-- | Get context lines around a match
getContext :: SearchOptions -> [String] -> Int -> MatchContext
getContext opts contentLines matchLine =
  let beforeStart = max 0 (matchLine - 1 - soContextBefore opts)
      beforeEnd = matchLine - 1
      afterStart = matchLine  -- 0-indexed: line after match
      afterEnd = min (length contentLines) (matchLine + soContextAfter opts)
      beforeLines = take (beforeEnd - beforeStart) $ drop beforeStart contentLines
      afterLines = take (afterEnd - afterStart) $ drop afterStart contentLines
  in MatchContext
       { mcBefore = beforeLines
       , mcAfter = afterLines
       }

-- | Convert a byte offset in text to (line, col) - 1-indexed
offsetToLineCol :: String -> Int -> (Int, Int)
offsetToLineCol content offset =
  let prefix = take offset content
      lineNum = 1 + length (filter (== '\n') prefix)
      colNum = 1 + length (takeWhile (/= '\n') (reverse prefix))
  in (lineNum, colNum)
