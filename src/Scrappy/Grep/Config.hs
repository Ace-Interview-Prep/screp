{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Grep.Config
  ( runParserViaGhc
  , ConfigError(..)
  ) where

import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>), takeDirectory, takeBaseName)
import System.Exit (ExitCode(..))
import Control.Exception (try, catch, SomeException)

data ConfigError
  = ConfigFileNotFound FilePath
  | GhcRunFailed String
  | ParseResultFailed String
  deriving (Show, Eq)

-- | Run a named parser from the import file via runghc
-- Returns the matches as a list of (line, col, matchText)
runParserViaGhc
  :: FilePath           -- ^ Import file path (Parsers.hs)
  -> String             -- ^ Parser name (e.g., "email")
  -> String             -- ^ Content to search
  -> IO (Either ConfigError [(Int, Int, String)])
runParserViaGhc importPath parserName content = do
  exists <- doesFileExist importPath
  if not exists
    then pure $ Left $ ConfigFileNotFound importPath
    else do
      tmpDir <- getTemporaryDirectory
      let runnerPath = tmpDir </> "PgrepRunner.hs"
          moduleName = takeBaseName importPath
          importDir = takeDirectory importPath

      -- Write the runner script
      writeFile runnerPath (generateRunner moduleName)

      -- Run it (need to expose parsec and containers packages via --ghc-arg)
      result <- try $ readProcessWithExitCode
        "runghc"
        [ "--ghc-arg=-package", "--ghc-arg=parsec"
        , "--ghc-arg=-package", "--ghc-arg=containers"
        , "--ghc-arg=-i" ++ importDir
        , runnerPath
        , parserName
        ]
        content

      -- Clean up
      removeFile runnerPath `catch` (\(_ :: SomeException) -> pure ())

      case result of
        Left (e :: SomeException) ->
          pure $ Left $ GhcRunFailed (show e)
        Right (ExitSuccess, stdout, _) ->
          pure $ Right $ parseOutput stdout
        Right (ExitFailure _, _, stderr) ->
          pure $ Left $ GhcRunFailed stderr

-- | Generate the runner Haskell script
generateRunner :: String -> String
generateRunner moduleName = unlines
  [ "module Main where"
  , ""
  , "import " ++ moduleName ++ " (parsers)"
  , "import Text.Parsec"
  , "import Text.Parsec.String"
  , "import qualified Data.Map as Map"
  , "import System.Environment (getArgs)"
  , "import System.Exit (exitFailure)"
  , "import System.IO (hPutStrLn, stderr)"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  args <- getArgs"
  , "  case args of"
  , "    [parserName] -> do"
  , "      content <- getContents"
  , "      case Map.lookup parserName parsers of"
  , "        Nothing -> do"
  , "          hPutStrLn stderr $ \"Unknown parser: \" ++ parserName"
  , "          exitFailure"
  , "        Just p -> mapM_ printMatch (findAll p content)"
  , "    _ -> do"
  , "      hPutStrLn stderr \"Usage: runner <parserName>\""
  , "      exitFailure"
  , ""
  , "findAll :: Parser String -> String -> [(Int, Int, String)]"
  , "findAll p = go 1 1"
  , "  where"
  , "    go _ _ [] = []"
  , "    go line col input@(c:cs) ="
  , "      case parse p \"\" input of"
  , "        Right match ->"
  , "          let len = length match"
  , "              newlines = length (filter (=='\\n') match)"
  , "              (newLine, newCol) ="
  , "                if newlines > 0"
  , "                then (line + newlines, length (takeWhile (/='\\n') (reverse match)) + 1)"
  , "                else (line, col + len)"
  , "          in (line, col, match) : go newLine newCol (drop len input)"
  , "        Left _ ->"
  , "          if c == '\\n'"
  , "          then go (line + 1) 1 cs"
  , "          else go line (col + 1) cs"
  , ""
  , "printMatch :: (Int, Int, String) -> IO ()"
  , "printMatch (l, c, m) = putStrLn $ show l ++ \":\" ++ show c ++ \":\" ++ escape m"
  , "  where"
  , "    escape = concatMap (\\x -> if x == '\\n' then \"\\\\n\" else [x])"
  ]

-- | Parse the output from runghc (LINE:COL:MATCH per line)
parseOutput :: String -> [(Int, Int, String)]
parseOutput = map parseLine . filter (not . null) . lines
  where
    parseLine s =
      let (lineStr, rest1) = break (== ':') s
          (colStr, rest2) = break (== ':') (drop 1 rest1)
          matchText = unescape (drop 1 rest2)
      in (read lineStr, read colStr, matchText)

    unescape [] = []
    unescape ('\\':'n':rest) = '\n' : unescape rest
    unescape (c:rest) = c : unescape rest
