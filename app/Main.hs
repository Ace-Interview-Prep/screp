module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret, InterpreterError(..))
import Scrappy.Grep.Search (searchFile, searchFiles)
import Scrappy.Grep.Output (formatResults, OutputFormat(..))
import Scrappy.Grep.Config (runParserViaGhc, ConfigError(..))
import Scrappy.Files (listFilesRecursive)

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesDirectoryExist, doesFileExist)
import Data.List (isSuffixOf, nub)

data Options = Options
  { optPattern    :: String
  , optTargets    :: [FilePath]
  , optRecursive  :: Bool
  , optExtensions :: [String]
  , optVerbose    :: Bool
  , optCount      :: Bool
  , optQuiet      :: Bool
  , optMaxResults :: Maybe Int
  , optJSON       :: Bool
  , optImport     :: Maybe FilePath
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      ( metavar "PATTERN"
     <> help "Parsec DSL pattern (e.g., 'some digit', 'ref \"email\"')"
      )
  <*> some (strArgument
      ( metavar "FILE..."
     <> help "Files or directories to search"
      ))
  <*> switch
      ( long "recursive"
     <> short 'r'
     <> help "Search directories recursively"
      )
  <*> many (strOption
      ( long "extension"
     <> short 'e'
     <> metavar "EXT"
     <> help "Only search files with extension (e.g., .hs, .txt)"
      ))
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output with full match text"
      )
  <*> switch
      ( long "count"
     <> short 'c'
     <> help "Only print count of matches"
      )
  <*> switch
      ( long "quiet"
     <> short 'q'
     <> help "Quiet mode (exit code only)"
      )
  <*> optional (option auto
      ( long "max-results"
     <> short 'm'
     <> metavar "N"
     <> help "Maximum number of results to show"
      ))
  <*> switch
      ( long "json"
     <> help "Output results as JSON"
      )
  <*> optional (strOption
      ( long "import"
     <> short 'i'
     <> metavar "FILE"
     <> help "Haskell file with named parsers (for use with 'ref \"name\"')"
      ))

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Search files using Parsec parser patterns"
   <> header "screp - grep with parser combinators"
    )

  -- Parse the DSL pattern
  case parseExpr (optPattern opts) of
    Left err -> do
      hPutStrLn stderr $ "Pattern parse error: " ++ show err
      exitFailure
    Right ast -> do
      -- Check if pattern uses refs
      if containsRef ast
        then runWithRefs opts ast
        else runWithoutRefs opts ast

-- | Run search using inline DSL only (no refs)
runWithoutRefs :: Options -> ParserExpr -> IO ()
runWithoutRefs opts ast = do
  case interpret ast of
    Left (UnknownRef name) -> do
      hPutStrLn stderr $ "Unknown parser reference: " ++ name
      hPutStrLn stderr "Use --import to provide a Haskell file with named parsers."
      exitFailure
    Right parser -> do
      files <- gatherFiles opts (optTargets opts)
      allResults <- searchFiles parser files
      outputResults opts allResults

-- | Run search using external parsers via runghc
runWithRefs :: Options -> ParserExpr -> IO ()
runWithRefs opts ast = do
  case optImport opts of
    Nothing -> do
      hPutStrLn stderr "Pattern uses 'ref' but no --import file specified."
      hPutStrLn stderr "Usage: screp --import Parsers.hs 'ref \"email\"' file.txt"
      exitFailure
    Just importPath -> do
      -- Extract the ref names (for now, only support single ref patterns)
      let refs = nub $ extractRefs ast
      case refs of
        [refName] -> do
          files <- gatherFiles opts (optTargets opts)
          allResults <- searchFilesWithRef importPath refName files
          outputResults opts allResults
        [] -> do
          -- No refs found, shouldn't happen since containsRef was true
          hPutStrLn stderr "Internal error: no refs found"
          exitFailure
        _ -> do
          hPutStrLn stderr "Currently only single ref patterns are supported."
          hPutStrLn stderr $ "Found refs: " ++ show refs
          exitFailure

-- | Search files using external parser via runghc
searchFilesWithRef :: FilePath -> String -> [FilePath] -> IO [MatchResult]
searchFilesWithRef importPath refName files = do
  results <- mapM searchOne files
  pure $ concat results
  where
    searchOne fp = do
      content <- readFile fp
      result <- runParserViaGhc importPath refName content
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error searching " ++ fp ++ ": " ++ showError err
          pure []
        Right matches -> pure $ map (toMatchResult fp) matches

    toMatchResult fp (line, col, matchText) = MatchResult
      { mrFilePath = fp
      , mrLine = line
      , mrCol = col
      , mrMatchText = matchText
      }

    showError (ConfigFileNotFound path) = "Import file not found: " ++ path
    showError (GhcRunFailed msg) = "GHC error: " ++ msg
    showError (ParseResultFailed msg) = "Parse error: " ++ msg

-- | Output results based on options
outputResults :: Options -> [MatchResult] -> IO ()
outputResults opts allResults = do
  let results = case optMaxResults opts of
        Nothing -> allResults
        Just n -> take n allResults

  if optQuiet opts
    then if null results then exitFailure else exitSuccess
    else do
      let fmt | optCount opts = FormatCount
              | optJSON opts = FormatJSON
              | optVerbose opts = FormatVerbose
              | otherwise = FormatGrep
      putStr $ formatResults fmt results
      if null results then exitFailure else exitSuccess

-- | Gather all files to search based on options
gatherFiles :: Options -> [FilePath] -> IO [FilePath]
gatherFiles opts targets = do
  allFiles <- concat <$> mapM (expandTarget opts) targets
  pure $ filterByExtension (optExtensions opts) allFiles

-- | Expand a target (file or directory) to a list of files
expandTarget :: Options -> FilePath -> IO [FilePath]
expandTarget opts target = do
  isDir <- doesDirectoryExist target
  isFile <- doesFileExist target
  case (isDir, isFile) of
    (True, _) ->
      if optRecursive opts
        then listFilesRecursive target
        else do
          hPutStrLn stderr $ "Warning: " ++ target ++ " is a directory. Use -r to search recursively."
          pure []
    (_, True) -> pure [target]
    _ -> do
      hPutStrLn stderr $ "Warning: " ++ target ++ " not found"
      pure []

-- | Filter files by extension
filterByExtension :: [String] -> [FilePath] -> [FilePath]
filterByExtension [] files = files
filterByExtension exts files = filter hasExt files
  where
    hasExt fp = any (`isSuffixOf` fp) normalizedExts
    normalizedExts = map ensureDot exts
    ensureDot ext = if "." `isPrefixOf` ext then ext else "." ++ ext
    isPrefixOf prefix str = take (length prefix) str == prefix
