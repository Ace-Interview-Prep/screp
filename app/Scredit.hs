{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret, InterpreterError(..))
import Scrappy.Find (streamEdit)
import Scrappy.Files (listFilesRecursive)
import qualified Scrappy.Scrape

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, hGetContents)
import qualified System.Directory
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName, takeDirectory, (</>))
import Data.List (isSuffixOf, isPrefixOf)
import Control.Exception (catch, IOException)
import Control.Monad (forM, when)

data Options = Options
  { optPattern      :: String
  , optReplacement  :: String
  , optTargets      :: [FilePath]
  , optRecursive    :: Bool
  , optExtensions   :: [String]
  , optInclude      :: [String]
  , optExclude      :: [String]
  , optExcludeDir   :: [String]
  , optInPlace      :: Bool
  , optVerbose      :: Bool
  , optDryRun       :: Bool
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      ( metavar "PATTERN"
     <> help "Parsec DSL pattern (e.g., 'some digit', 'string \"TODO\"')"
      )
  <*> strOption
      ( long "replace"
     <> short 'r'
     <> metavar "STRING"
     <> help "Replacement string"
      )
  <*> many (strArgument
      ( metavar "FILE..."
     <> help "Files or directories to edit (use - for stdin)"
      ))
  <*> switch
      ( long "recursive"
     <> short 'R'
     <> help "Recursively search directories"
      )
  <*> many (strOption
      ( long "extension"
     <> short 'e'
     <> metavar "EXT"
     <> help "Only edit files with extension (e.g., .hs, .txt)"
      ))
  <*> many (strOption
      ( long "include"
     <> metavar "GLOB"
     <> help "Only edit files matching GLOB pattern"
      ))
  <*> many (strOption
      ( long "exclude"
     <> metavar "GLOB"
     <> help "Skip files matching GLOB pattern"
      ))
  <*> many (strOption
      ( long "exclude-dir"
     <> metavar "GLOB"
     <> help "Skip directories matching GLOB pattern"
      ))
  <*> switch
      ( long "in-place"
     <> short 'i'
     <> help "Edit files in place (instead of printing to stdout)"
      )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Show verbose output"
      )
  <*> switch
      ( long "dry-run"
     <> short 'n'
     <> help "Show what would be changed without making changes"
      )

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Find and replace using Parsec parser patterns"
   <> header "scredit - sed with parser combinators"
    )

  let targets = if null (optTargets opts) then ["-"] else optTargets opts
      opts' = opts { optTargets = targets }

  case parseExpr (optPattern opts') of
    Left err -> do
      hPutStrLn stderr $ "Pattern parse error: " ++ show err
      exitFailure
    Right ast -> do
      if containsRef ast
        then do
          hPutStrLn stderr "Error: scredit does not yet support 'ref' patterns"
          hPutStrLn stderr "Use inline patterns only (e.g., 'some digit', 'string \"TODO\"')"
          exitFailure
        else runEdit opts' ast

runEdit :: Options -> ParserExpr -> IO ()
runEdit opts ast = do
  case interpret ast of
    Left (UnknownRef name) -> do
      hPutStrLn stderr $ "Error: Unknown reference '" ++ name ++ "'"
      exitFailure
    Right parser -> do
      if "-" `elem` optTargets opts
        then do
          content <- hGetContents stdin
          let edited = streamEdit parser (const $ optReplacement opts) content
          putStr edited
        else do
          files <- gatherFiles opts (optTargets opts)
          editFiles opts parser files

editFiles :: Options -> Scrappy.Scrape.ScraperT String -> [FilePath] -> IO ()
editFiles opts parser files = do
  forM_ files $ \fp -> editFile opts parser fp
  where
    forM_ xs f = mapM_ f xs

editFile :: Options -> Scrappy.Scrape.ScraperT String -> FilePath -> IO ()
editFile opts parser fp = do
  result <- (Just <$> readFile fp) `catch` (\(_ :: IOException) -> return Nothing)
  case result of
    Nothing -> return ()
    Just content -> do
      let !_ = length content  -- Force to catch encoding errors
          replacement = optReplacement opts
          edited = streamEdit parser (const replacement) content
          changed = content /= edited

      when (optVerbose opts && changed) $
        hPutStrLn stderr $ "Editing: " ++ fp

      if optInPlace opts
        then do
          when changed $ do
            if optDryRun opts
              then hPutStrLn stderr $ "Would edit: " ++ fp
              else writeFile fp edited
        else do
          when changed $ putStr edited

-- | Gather all files to edit based on options
gatherFiles :: Options -> [FilePath] -> IO [FilePath]
gatherFiles opts targets = do
  let fileTargets = filter (/= "-") targets
  allFiles <- concat <$> mapM (expandTarget opts) fileTargets
  let filtered = filterByExtension (optExtensions opts) allFiles
  return $ filterExcludes opts $ filterIncludes opts filtered

filterIncludes :: Options -> [FilePath] -> [FilePath]
filterIncludes opts files =
  case optInclude opts of
    [] -> files
    patterns -> filter (matchesAnyGlob patterns . takeFileName) files

filterExcludes :: Options -> [FilePath] -> [FilePath]
filterExcludes opts files =
  let excludeFile = not . matchesAnyGlob (optExclude opts) . takeFileName
      excludeDir = not . anyParentMatches (optExcludeDir opts)
  in filter (\f -> excludeFile f && excludeDir f) files

anyParentMatches :: [String] -> FilePath -> Bool
anyParentMatches patterns fp =
  let dirs = splitPath fp
  in any (matchesAnyGlob patterns) dirs
  where
    splitPath p = case takeDirectory p of
      "." -> []
      "/" -> []
      parent -> takeFileName parent : splitPath parent

matchesAnyGlob :: [String] -> String -> Bool
matchesAnyGlob patterns str = any (`simpleGlobMatch` str) patterns

simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch [] [] = True
simpleGlobMatch [] _ = False
simpleGlobMatch ('*':rest) str = any (simpleGlobMatch rest) (tails str)
simpleGlobMatch ('?':rest) (_:str) = simpleGlobMatch rest str
simpleGlobMatch ('?':_) [] = False
simpleGlobMatch (c:rest) (s:str) = c == s && simpleGlobMatch rest str
simpleGlobMatch _ [] = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:rest) = xs : tails rest

expandTarget :: Options -> FilePath -> IO [FilePath]
expandTarget opts target = do
  isDir <- doesDirectoryExist target
  isFile <- doesFileExist target
  case (isDir, isFile) of
    (True, _) ->
      if optRecursive opts
        then listFilesRecursiveFiltered (optExcludeDir opts) target
        else do
          hPutStrLn stderr $ "Warning: " ++ target ++ " is a directory. Use -R to edit recursively."
          return []
    (_, True) -> return [target]
    _ -> do
      hPutStrLn stderr $ "Warning: " ++ target ++ " not found"
      return []

listFilesRecursiveFiltered :: [String] -> FilePath -> IO [FilePath]
listFilesRecursiveFiltered excludeDirs dir = go dir `catch` (\(_ :: IOException) -> return [])
  where
    go d = do
      contents <- System.Directory.listDirectory d
      paths <- forM contents $ \name -> do
        let fullPath = d </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
          then if matchesAnyGlob excludeDirs name
               then return []
               else listFilesRecursiveFiltered excludeDirs fullPath
          else do
            absPath <- System.Directory.makeAbsolute fullPath
            return [absPath]
      return (concat paths)

filterByExtension :: [String] -> [FilePath] -> [FilePath]
filterByExtension [] files = files
filterByExtension exts files = filter hasExt files
  where
    hasExt fp = any (`isSuffixOf` fp) normalizedExts
    normalizedExts = map ensureDot exts
    ensureDot ext = if "." `isPrefixOf` ext then ext else "." ++ ext
