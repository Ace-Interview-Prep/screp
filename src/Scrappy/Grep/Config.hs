{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Grep.Config
  ( runParserViaGhc
  , defaultConfigPath
  , ConfigError(..)
  ) where

import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)

data ConfigError
  = ConfigFileNotFound FilePath
  | GhcRunFailed String
  | ParseResultFailed String
  deriving (Show, Eq)

-- | Default config file path: ~/.config/parsec-grep/Parsers.hs
defaultConfigPath :: IO FilePath
defaultConfigPath = do
  home <- getHomeDirectory
  let dir = home </> ".config" </> "parsec-grep"
  createDirectoryIfMissing True dir
  pure $ dir </> "Parsers.hs"

-- | Run a named parser from the config file via runghc
-- Returns the matches as a list of (line, col, matchText)
runParserViaGhc
  :: FilePath           -- ^ Config file path (Parsers.hs)
  -> String             -- ^ Parser name (e.g., "email")
  -> String             -- ^ Content to search
  -> IO (Either ConfigError [(Int, Int, String)])
runParserViaGhc configPath parserName content = do
  exists <- doesFileExist configPath
  if not exists
    then pure $ Left $ ConfigFileNotFound configPath
    else do
      -- Generate a wrapper script that imports the config and runs the parser
      let wrapperScript = generateWrapper configPath parserName
      result <- try $ readProcessWithExitCode "runghc" ["-e", wrapperScript] content
      case result of
        Left (e :: SomeException) -> pure $ Left $ GhcRunFailed (show e)
        Right (ExitSuccess, stdout, _) ->
          case parseOutput stdout of
            Nothing -> pure $ Left $ ParseResultFailed stdout
            Just matches -> pure $ Right matches
        Right (ExitFailure code, _, stderr) ->
          pure $ Left $ GhcRunFailed $ "Exit code " ++ show code ++ ": " ++ stderr

-- | Generate a Haskell expression that runs the parser and outputs results
generateWrapper :: FilePath -> String -> String
generateWrapper configPath parserName = unlines
  [ ":set -i" ++ takeDirectory configPath
  , ":load " ++ configPath
  , "import Parsers"
  , "import Text.Parsec"
  , "import Data.Map (lookup)"
  , "import Data.Maybe (fromMaybe)"
  , "import Scrappy.Find (findNaive)"
  , ""
  , "main = do"
  , "  content <- getContents"
  , "  let mParser = Data.Map.lookup \"" ++ parserName ++ "\" parsers"
  , "  case mParser of"
  , "    Nothing -> putStrLn \"ERROR:UnknownParser:" ++ parserName ++ "\""
  , "    Just p -> do"
  , "      case parse (findNaiveWithPos p) \"\" content of"
  , "        Left err -> putStrLn $ \"ERROR:ParseFailed:\" ++ show err"
  , "        Right Nothing -> putStrLn \"MATCHES:\""
  , "        Right (Just ms) -> putStrLn $ \"MATCHES:\" ++ show ms"
  ]
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

-- | Parse the output from runghc
parseOutput :: String -> Maybe [(Int, Int, String)]
parseOutput output
  | "MATCHES:" `isPrefixOf` output =
      let rest = drop 8 output
      in if null rest || rest == "\n"
         then Just []
         else readMaybe (takeWhile (/= '\n') rest)
  | otherwise = Nothing
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- Note: This is a simplified implementation. A more robust version would:
-- 1. Use a temp file for the wrapper script
-- 2. Handle GHC package dependencies properly
-- 3. Cache compiled parsers for performance
--
-- For now, we'll implement a simpler version that works for basic cases
-- and can be enhanced later.

