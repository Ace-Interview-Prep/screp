{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret, InterpreterError(..))
import Scrappy.Grep.Search (searchFilesWithOpts, SearchOptions(..), defaultSearchOptions)
import Scrappy.Grep.Output (formatResultsWithOpts, formatResultsGrouped, OutputFormat(..), OutputOptions(..), ColorMode(..))
import Scrappy.Grep.Config (runParserViaGhc, ConfigError(..))
import Scrappy.Files (listFilesRecursive)
import qualified Scrappy.Scrape
import qualified Text.Parsec

import Options.Applicative hiding ((<|>))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, hGetContents, hIsTerminalDevice, stdout)
import qualified System.Directory
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName, takeDirectory, (</>))
import Data.List (isSuffixOf, nub, isPrefixOf)
import Data.Char (toLower)
import Control.Exception (catch, IOException)
import Control.Monad (forM)
import Control.Applicative ((<|>), (<$))

data Options = Options
  { optPattern      :: String
  , optTargets      :: [FilePath]
  , optRecursive    :: Bool
  , optExtensions   :: [String]
  , optInclude      :: [String]      -- --include
  , optExclude      :: [String]      -- --exclude
  , optExcludeDir   :: [String]      -- --exclude-dir
  , optIgnoreCase   :: Bool          -- -i
  , optVerbose      :: Bool
  , optCount        :: Bool
  , optQuiet        :: Bool
  , optMaxResults   :: Maybe Int
  , optJSON         :: Bool
  , optImport       :: Maybe FilePath
  , optFilesOnly    :: Bool          -- -l
  , optFilesWithout :: Bool          -- -L
  , optNoFilename   :: Bool          -- -h
  , optContextBefore :: Int          -- -B
  , optContextAfter  :: Int          -- -A
  , optContext       :: Int          -- -C (both)
  , optColor        :: Maybe String  -- --color
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> strArgument
      ( metavar "PATTERN"
     <> help "Parsec DSL pattern (e.g., 'some digit', 'ref \"email\"')"
      )
  <*> many (strArgument
      ( metavar "FILE..."
     <> help "Files or directories to search (use - for stdin)"
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
  <*> many (strOption
      ( long "include"
     <> metavar "GLOB"
     <> help "Only search files matching GLOB pattern (e.g., *.hs, src/*.txt)"
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
      ( long "ignore-case"
     <> short 'i'
     <> help "Ignore case distinctions in pattern"
      )
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
     <> metavar "FILE"
     <> help "Haskell file with named parsers (for use with 'ref \"name\"')"
      ))
  <*> switch
      ( long "files-with-matches"
     <> short 'l'
     <> help "Print only names of files with matches"
      )
  <*> switch
      ( long "files-without-match"
     <> short 'L'
     <> help "Print only names of files without matches"
      )
  <*> switch
      ( long "no-filename"
     <> short 'h'
     <> help "Suppress the file name prefix on output"
      )
  <*> option auto
      ( long "before-context"
     <> short 'B'
     <> metavar "NUM"
     <> value 0
     <> help "Print NUM lines of leading context"
      )
  <*> option auto
      ( long "after-context"
     <> short 'A'
     <> metavar "NUM"
     <> value 0
     <> help "Print NUM lines of trailing context"
      )
  <*> option auto
      ( long "context"
     <> short 'C'
     <> metavar "NUM"
     <> value 0
     <> help "Print NUM lines of context"
      )
  <*> optional (strOption
      ( long "color"
     <> long "colour"
     <> metavar "WHEN"
     <> help "Use color: always, never, or auto"
      ))

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Search files using Parsec parser patterns"
   <> header "screp - grep with parser combinators"
    )

  -- Handle stdin if no targets or "-" specified
  let targets = if null (optTargets opts) then ["-"] else optTargets opts
      opts' = opts { optTargets = targets }

  -- Parse the DSL pattern (with case folding if -i)
  let patternStr = if optIgnoreCase opts'
                   then map toLower (optPattern opts')
                   else optPattern opts'

  case parseExpr patternStr of
    Left err -> do
      hPutStrLn stderr $ "Pattern parse error: " ++ show err
      exitFailure
    Right ast -> do
      -- Check if pattern uses refs
      if containsRef ast
        then runWithRefs opts' ast
        else runWithoutRefs opts' ast

-- | Build SearchOptions from CLI Options
mkSearchOptions :: Options -> SearchOptions
mkSearchOptions opts = SearchOptions
  { soIgnoreCase = optIgnoreCase opts
  , soContextBefore = if optContext opts > 0 then optContext opts else optContextBefore opts
  , soContextAfter = if optContext opts > 0 then optContext opts else optContextAfter opts
  }

-- | Build OutputOptions from CLI Options
mkOutputOptions :: Options -> IO OutputOptions
mkOutputOptions opts = do
  colorMode <- case optColor opts of
    Just "always" -> pure ColorAlways
    Just "never"  -> pure ColorNever
    Just "auto"   -> do
      isTerm <- hIsTerminalDevice stdout
      pure $ if isTerm then ColorAlways else ColorNever
    Nothing -> do
      isTerm <- hIsTerminalDevice stdout
      pure $ if isTerm then ColorAlways else ColorNever
    Just other -> do
      hPutStrLn stderr $ "Invalid --color value: " ++ other ++ " (use always, never, or auto)"
      pure ColorNever
  pure OutputOptions
    { ooColor = colorMode
    , ooNoFilename = optNoFilename opts
    }

-- | Run search using inline DSL only (no refs)
runWithoutRefs :: Options -> ParserExpr -> IO ()
runWithoutRefs opts ast = do
  case interpret ast of
    Left (UnknownRef name) -> do
      hPutStrLn stderr $ "Unknown parser reference: " ++ name
      hPutStrLn stderr "Use --import to provide a Haskell file with named parsers."
      exitFailure
    Right parser -> do
      -- Handle stdin
      if "-" `elem` optTargets opts
        then do
          content <- hGetContents stdin
          let searchOpts = mkSearchOptions opts
              results = searchTextWithOpts searchOpts "(stdin)" parser content
          outputResults opts results
        else do
          files <- gatherFiles opts (optTargets opts)
          let searchOpts = mkSearchOptions opts
          allResults <- searchFilesWithOpts searchOpts parser files
          outputResults opts allResults

-- | Search text with options (helper for stdin)
searchTextWithOpts :: SearchOptions -> FilePath -> Scrappy.Scrape.ScraperT String -> String -> [MatchResult]
searchTextWithOpts opts fp parser content =
  let contentToSearch = if soIgnoreCase opts then map toLower content else content
      contentLines = lines content
  in case Text.Parsec.parse (findAllWithPos parser) "" contentToSearch of
       Left _ -> []
       Right matches -> map (toMatchResultWithContext opts contentLines fp) matches
  where
    findAllWithPos p = go
      where
        go = do
          atEnd <- (True <$ Text.Parsec.eof) <|> pure False
          if atEnd
            then pure []
            else tryMatch <|> skipAndContinue

        tryMatch = do
          pos <- Text.Parsec.getPosition
          let line = Text.Parsec.sourceLine pos
              col = Text.Parsec.sourceColumn pos
          matched <- Text.Parsec.try p
          rest <- go
          pure $ (line, col, matched) : rest

        skipAndContinue = do
          _ <- Text.Parsec.anyChar
          go

    toMatchResultWithContext sopts contentLines fp' (line, col, matched) =
      let ctx = if soContextBefore sopts > 0 || soContextAfter sopts > 0
                then Just $ getContext sopts contentLines line
                else Nothing
      in MatchResult
           { mrFilePath = fp'
           , mrLine = line
           , mrCol = col
           , mrMatchText = matched
           , mrContext = ctx
           }

    getContext sopts contentLines matchLine =
      let beforeStart = max 0 (matchLine - 1 - soContextBefore sopts)
          beforeEnd = matchLine - 1
          afterStart = matchLine
          afterEnd = min (length contentLines) (matchLine + soContextAfter sopts)
          beforeLines = take (beforeEnd - beforeStart) $ drop beforeStart contentLines
          afterLines = take (afterEnd - afterStart) $ drop afterStart contentLines
      in MatchContext { mcBefore = beforeLines, mcAfter = afterLines }

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
          allResults <- searchFilesWithRefOpts opts importPath refName files
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
searchFilesWithRefOpts :: Options -> FilePath -> String -> [FilePath] -> IO [MatchResult]
searchFilesWithRefOpts opts importPath refName files = do
  results <- mapM searchOne files
  pure $ concat results
  where
    searchOne fp = readAndSearch `catch` handleIOError
      where
        readAndSearch = do
          content <- readFile fp
          -- Force evaluation to catch encoding errors early
          let !_ = length content
              contentToSearch = if optIgnoreCase opts then map toLower content else content
          result <- runParserViaGhc importPath refName contentToSearch
          case result of
            Left err -> do
              hPutStrLn stderr $ "Error searching " ++ fp ++ ": " ++ showError err
              pure []
            Right matches -> pure $ map (toMatchResult fp) matches

        handleIOError :: IOException -> IO [MatchResult]
        handleIOError _ = pure []  -- Skip files we can't read (binary, permissions, etc.)

    toMatchResult fp (line, col, matchText) = MatchResult
      { mrFilePath = fp
      , mrLine = line
      , mrCol = col
      , mrMatchText = matchText
      , mrContext = Nothing  -- TODO: add context support for refs
      }

    showError (ConfigFileNotFound path) = "Import file not found: " ++ path
    showError (GhcRunFailed msg) = "GHC error: " ++ msg
    showError (ParseResultFailed msg) = "Parse error: " ++ msg

-- | Output results based on options
outputResults :: Options -> [MatchResult] -> IO ()
outputResults opts allResults = do
  outOpts <- mkOutputOptions opts

  -- Handle -L (files without matches)
  if optFilesWithout opts
    then do
      files <- gatherFiles opts (optTargets opts)
      let filesWithMatches = nub $ map mrFilePath allResults
          filesWithoutMatches = filter (`notElem` filesWithMatches) files
      mapM_ putStrLn filesWithoutMatches
      if null filesWithoutMatches then exitFailure else exitSuccess
    else do
      let results = case optMaxResults opts of
            Nothing -> allResults
            Just n -> take n allResults

      if optQuiet opts
        then if null results then exitFailure else exitSuccess
        else do
          let fmt | optCount opts = FormatCount
                  | optJSON opts = FormatJSON
                  | optFilesOnly opts = FormatFilesOnly
                  | optVerbose opts = FormatVerbose
                  | otherwise = FormatGrep
              hasContext = optContextBefore opts > 0 || optContextAfter opts > 0 || optContext opts > 0
              output = if hasContext && fmt == FormatGrep
                       then formatResultsGrouped outOpts fmt results
                       else formatResultsWithOpts outOpts fmt results
          putStr output
          if null results then exitFailure else exitSuccess

-- | Gather all files to search based on options
gatherFiles :: Options -> [FilePath] -> IO [FilePath]
gatherFiles opts targets = do
  -- Filter out stdin marker
  let fileTargets = filter (/= "-") targets
  allFiles <- concat <$> mapM (expandTarget opts) fileTargets
  let filtered = filterByExtension (optExtensions opts) allFiles
  pure $ filterExcludes opts $ filterIncludes opts filtered

-- | Filter files by include patterns (if any specified)
filterIncludes :: Options -> [FilePath] -> [FilePath]
filterIncludes opts files =
  case optInclude opts of
    [] -> files  -- No include patterns = include all
    patterns -> filter (matchesAnyGlob patterns . takeFileName) files

-- | Filter files by exclude patterns
filterExcludes :: Options -> [FilePath] -> [FilePath]
filterExcludes opts files =
  let excludeFile = not . matchesAnyGlob (optExclude opts) . takeFileName
      excludeDir = not . anyParentMatches (optExcludeDir opts)
  in filter (\f -> excludeFile f && excludeDir f) files

-- | Check if any parent directory matches exclude patterns
anyParentMatches :: [String] -> FilePath -> Bool
anyParentMatches patterns fp =
  let dirs = splitPath fp
  in any (matchesAnyGlob patterns) dirs
  where
    splitPath p = case takeDirectory p of
      "." -> []
      "/" -> []
      parent -> takeFileName parent : splitPath parent

-- | Check if a string matches any glob pattern
matchesAnyGlob :: [String] -> String -> Bool
matchesAnyGlob patterns str = any (`simpleGlobMatch` str) patterns

-- | Simple glob matching (supports * and ?)
simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch [] [] = True
simpleGlobMatch [] _ = False
simpleGlobMatch ('*':rest) str = any (simpleGlobMatch rest) (tails str)
simpleGlobMatch ('?':rest) (_:str) = simpleGlobMatch rest str
simpleGlobMatch ('?':_) [] = False
simpleGlobMatch (c:rest) (s:str) = c == s && simpleGlobMatch rest str
simpleGlobMatch _ [] = False

-- | Get all tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:rest) = xs : tails rest

-- | Expand a target (file or directory) to a list of files
expandTarget :: Options -> FilePath -> IO [FilePath]
expandTarget opts target = do
  isDir <- doesDirectoryExist target
  isFile <- doesFileExist target
  case (isDir, isFile) of
    (True, _) ->
      if optRecursive opts
        then listFilesRecursiveFiltered (optExcludeDir opts) target
        else do
          hPutStrLn stderr $ "Warning: " ++ target ++ " is a directory. Use -r to search recursively."
          pure []
    (_, True) -> pure [target]
    _ -> do
      hPutStrLn stderr $ "Warning: " ++ target ++ " not found"
      pure []

-- | List files recursively, filtering out excluded directories
listFilesRecursiveFiltered :: [String] -> FilePath -> IO [FilePath]
listFilesRecursiveFiltered excludeDirs dir = go dir `catch` (\(_ :: IOException) -> pure [])
  where
    go d = do
      contents <- System.Directory.listDirectory d
      paths <- forM contents $ \name -> do
        let fullPath = d </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
          then if matchesAnyGlob excludeDirs name
               then pure []
               else listFilesRecursiveFiltered excludeDirs fullPath
          else do
            absPath <- System.Directory.makeAbsolute fullPath
            return [absPath]
      return (concat paths)

-- | Filter files by extension
filterByExtension :: [String] -> [FilePath] -> [FilePath]
filterByExtension [] files = files
filterByExtension exts files = filter hasExt files
  where
    hasExt fp = any (`isSuffixOf` fp) normalizedExts
    normalizedExts = map ensureDot exts
    ensureDot ext = if "." `isPrefixOf` ext then ext else "." ++ ext
