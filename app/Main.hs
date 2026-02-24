module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret, InterpreterError(..))
import Scrappy.Grep.Search (searchFile, searchFiles, searchText)
import Scrappy.Grep.Output (formatResults, OutputFormat(..))
import Scrappy.Files (listFilesRecursive)

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeExtension)
import Control.Monad (filterM, when)
import Data.List (isSuffixOf)

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
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      ( metavar "PATTERN"
     <> help "Parsec DSL pattern (e.g., 'some digit >> char '@' >> some letter')"
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

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Search files using Parsec parser patterns"
   <> header "pgrep - grep with parser combinators"
    )

  -- Parse the DSL pattern
  case parseExpr (optPattern opts) of
    Left err -> do
      hPutStrLn stderr $ "Pattern parse error: " ++ show err
      exitFailure
    Right ast -> do
      -- Interpret the AST into a parser
      case interpret ast of
        Left (UnknownRef name) -> do
          hPutStrLn stderr $ "Unknown parser reference: " ++ name
          hPutStrLn stderr "Note: Config-based refs not yet implemented. Use inline DSL only."
          exitFailure
        Right parser -> do
          -- Gather all files to search
          files <- gatherFiles opts (optTargets opts)

          -- Search all files
          allResults <- searchFiles parser files

          -- Apply max results limit
          let results = case optMaxResults opts of
                Nothing -> allResults
                Just n -> take n allResults

          -- Output based on options
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
