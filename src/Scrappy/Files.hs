{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Files where

import Scrappy.Types
import Scrappy.Scrape
import Text.Parsec
import Scrappy.Elem
import qualified Data.Map.Strict as Map
import Control.Exception
import Control.Monad
import Data.Map.Strict (Map,keys, toList)
import Data.List (foldl')
import System.FilePath
import System.Directory


-- | Recursively lists all files in a directory, returning absolute file paths.
-- Silently skips directories that cannot be read (permission denied, etc.)
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = go dir `catch` (\(_ :: IOException) -> pure [])
  where
    go d = do
        contents <- listDirectory d
        paths <- forM contents $ \name -> do
            let fullPath = d </> name
            isDir <- doesDirectoryExist fullPath
            if isDir
                then listFilesRecursive fullPath
                else do
                    absPath <- makeAbsolute fullPath
                    return [absPath]
        return (concat paths)


searchFile :: ScraperT a -> FilePath -> IO Bool
searchFile p fp = do
  str <- readFile fp
  pure $ exists p str

searchStrFile :: String -> FilePath -> IO Bool
searchStrFile s fp = searchFile (string "s") fp

searchManyFile :: [String] -> FilePath -> IO (Map String Int)
searchManyFile strs fp = flip catch (\(_ :: IOException) -> pure mempty) $ do
  print fp
  file <- readFile fp
  case scrape (buildElemsOpts strs) file of
    Nothing -> pure mempty
    Just results -> pure $ countOccurrences results

-- | Function to count occurrences of each unique string in a list
countOccurrences :: [String] -> Map String Int
countOccurrences = foldl' (\acc word -> Map.insertWith (+) word 1 acc) Map.empty



areFilesUsed :: FilePath -> FilePath -> IO ()
areFilesUsed sourceDir usageDir = do
  sources <- listFilesRecursive sourceDir
  searchFiles <- listFilesRecursive usageDir
  let sources' = takeFileName <$> sources
  maps <- mapM (\x -> searchManyFile sources' x) searchFiles
  let mapped = mconcat maps
  print mapped

  print "---"

  print $ filter (\s -> not $ elem s (keys mapped)) sources'

areFilesUsed' :: FilePath -> [FilePath] -> IO [String]
areFilesUsed' sourceDir usageDirs = do
  sources <- listFilesRecursive sourceDir
  searchFiles <- mconcat <$> mapM listFilesRecursive usageDirs
  putStr "Number of files: "
  print $ length searchFiles
  let sources' = takeFileName <$> sources
  maps <- mapM (\x -> searchManyFile sources' x) searchFiles
  let mapped = mconcat maps
  mapM_ print $ toList mapped

  print "---"

  let unused = filter (\s -> s `notElem` keys mapped) sources'
  mapM_ print unused
  pure unused
