{-# LANGUAGE FlexibleContexts #-}

module Scrappy.Grep.Search
  ( searchFile
  , searchFiles
  , searchText
  , offsetToLineCol
  ) where

import Scrappy.Grep.DSL (MatchResult(..))
import Scrappy.Scrape (ScraperT)

import Text.Parsec (parse, getPosition, sourceColumn, sourceLine, try, anyChar, (<|>), eof)
import System.Directory (doesFileExist)

-- | Search a file for all matches, returning results with positions
searchFile :: ScraperT String -> FilePath -> IO [MatchResult]
searchFile parser fp = do
  exists <- doesFileExist fp
  if not exists
    then pure []
    else do
      content <- readFile fp
      pure $ searchText fp parser content

-- | Search multiple files
searchFiles :: ScraperT String -> [FilePath] -> IO [MatchResult]
searchFiles parser fps = concat <$> mapM (searchFile parser) fps

-- | Search text content and return matches with positions
searchText :: FilePath -> ScraperT String -> String -> [MatchResult]
searchText fp parser content =
  case parse (findAllWithPos parser) "" content of
    Left _ -> []
    Right matches -> map (toMatchResult fp) matches

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

-- | Convert (line, col, match) to MatchResult
toMatchResult :: FilePath -> (Int, Int, String) -> MatchResult
toMatchResult fp (line, col, matched) = MatchResult
  { mrFilePath = fp
  , mrLine = line
  , mrCol = col
  , mrMatchText = matched
  }

-- | Convert a byte offset in text to (line, col) - 1-indexed
offsetToLineCol :: String -> Int -> (Int, Int)
offsetToLineCol content offset =
  let prefix = take offset content
      lineNum = 1 + length (filter (== '\n') prefix)
      colNum = 1 + length (takeWhile (/= '\n') (reverse prefix))
  in (lineNum, colNum)
