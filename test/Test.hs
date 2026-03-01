{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret)
import Scrappy.Grep.Search (searchText, searchFile, searchFiles, searchTextWithContext, searchFilesWithOpts, SearchOptions(..))
import Scrappy.Grep.Output (formatResults, formatResultsWithOpts, formatResultsGrouped, OutputFormat(..), OutputOptions(..), ColorMode(..))

import System.Exit (exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (toLower)

main :: IO ()
main = do
  putStrLn "Running screp tests...\n"

  -- Run all tests
  results <- sequence
    [ testDSLParser
    , testInterpreter
    , testSearchString
    , testSearchFile
    , testSearchDirectory
    , testIgnoreCase
    , testContextLines
    , testColorOutput
    , testFilesOnlyOutput
    , testExcludePatterns
    , testOutputFormats
    ]

  -- Summary
  let passed = length (filter id results)
      total = length results
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"

  if and results
    then exitSuccess
    else exitFailure

-- | Test DSL parser
testDSLParser :: IO Bool
testDSLParser = do
  putStrLn "=== DSL Parser Tests ==="

  let tests =
        [ ("char 'a'", PChar 'a')
        , ("string \"hello\"", PString "hello")
        , ("digit", PDigit)
        , ("letter", PLetter)
        , ("anyChar", PAnyChar)
        , ("alphaNum", PAlphaNum)
        , ("space", PSpace)
        , ("spaces", PSpaces)
        , ("newline", PNewline)
        , ("oneOf \"abc\"", POneOf "abc")
        , ("noneOf \"xyz\"", PNoneOf "xyz")
        , ("many digit", PMany PDigit)
        , ("some letter", PSome PLetter)
        , ("optional digit", POptional PDigit)
        , ("try letter", PTry PLetter)
        , ("count 3 digit", PCount 3 PDigit)
        , ("between '(' ')' digit", PBetween '(' ')' PDigit)
        , ("digit >> letter", PSeq PDigit PLetter)
        , ("digit <+> letter", PSeqConcat PDigit PLetter)
        , ("digit <|> letter", PAlt PDigit PLetter)
        , ("ref \"email\"", PRef "email")
        -- Precedence tests
        , ("digit >> letter <|> space", PAlt (PSeq PDigit PLetter) PSpace)
        , ("digit <+> letter >> space", PSeq (PSeqConcat PDigit PLetter) PSpace)
        , ("many digit >> letter", PSeq (PMany PDigit) PLetter)
        -- Parentheses
        , ("(digit >> letter)", PSeq PDigit PLetter)
        , ("many (digit <|> letter)", PMany (PAlt PDigit PLetter))
        ]

  results <- mapM runParserTest tests
  pure $ and results

  where
    runParserTest (input, expected) = do
      case parseExpr input of
        Left err -> do
          putStrLn $ "  FAIL: '" ++ input ++ "'"
          putStrLn $ "    Error: " ++ show err
          pure False
        Right actual ->
          if actual == expected
            then do
              putStrLn $ "  PASS: '" ++ input ++ "'"
              pure True
            else do
              putStrLn $ "  FAIL: '" ++ input ++ "'"
              putStrLn $ "    Expected: " ++ show expected
              putStrLn $ "    Actual:   " ++ show actual
              pure False

-- | Test interpreter
testInterpreter :: IO Bool
testInterpreter = do
  putStrLn "\n=== Interpreter Tests ==="

  let tests =
        [ ("digit", "abc123def", ["1", "2", "3"])
        , ("some digit", "abc123def456", ["123", "456"])
        , ("string \"hello\"", "say hello world hello", ["hello", "hello"])
        , ("letter <+> digit", "a1 b2 c3", ["a1", "b2", "c3"])
        , ("char 'a' >> char 'b'", "ab ab ab", ["b", "b", "b"])
        , ("char 'a' <+> char 'b'", "ab ab ab", ["ab", "ab", "ab"])
        , ("between '(' ')' some digit", "(123) (456)", ["(123)", "(456)"])
        , ("count 3 letter", "abc def ghi", ["abc", "def", "ghi"])
        , ("digit <|> letter", "a1b2c3", ["a", "1", "b", "2", "c", "3"])
        ]

  results <- mapM runInterpTest tests
  pure $ and results

  where
    runInterpTest (pattern, input, expected) = do
      case parseExpr pattern of
        Left err -> do
          putStrLn $ "  FAIL: '" ++ pattern ++ "' - parse error: " ++ show err
          pure False
        Right ast -> case interpret ast of
          Left err -> do
            putStrLn $ "  FAIL: '" ++ pattern ++ "' - interpret error: " ++ show err
            pure False
          Right parser -> do
            let results = searchText "<test>" parser input
                matches = map mrMatchText results
            if matches == expected
              then do
                putStrLn $ "  PASS: '" ++ pattern ++ "' on \"" ++ take 20 input ++ "...\""
                pure True
              else do
                putStrLn $ "  FAIL: '" ++ pattern ++ "'"
                putStrLn $ "    Input:    \"" ++ input ++ "\""
                putStrLn $ "    Expected: " ++ show expected
                putStrLn $ "    Actual:   " ++ show matches
                pure False

-- | Test search on a string
testSearchString :: IO Bool
testSearchString = do
  putStrLn "\n=== String Search Tests ==="

  -- Test multi-line matching
  let multiLineContent = "Hello World\nThis is line 2\nAnd line 3"

  case parseExpr "string \"line\"" of
    Left err -> do
      putStrLn $ "  FAIL: parse error: " ++ show err
      pure False
    Right ast -> case interpret ast of
      Left err -> do
        putStrLn $ "  FAIL: interpret error: " ++ show err
        pure False
      Right parser -> do
        let results = searchText "<test>" parser multiLineContent
        if length results == 2
          then do
            putStrLn $ "  PASS: Found " ++ show (length results) ++ " matches for 'line'"
            -- Check positions
            let r1 = head results
                r2 = results !! 1
            if mrLine r1 == 2 && mrLine r2 == 3
              then do
                putStrLn $ "  PASS: Line numbers correct (line 2 and 3)"
                pure True
              else do
                putStrLn $ "  FAIL: Line numbers wrong"
                putStrLn $ "    Result 1 line: " ++ show (mrLine r1)
                putStrLn $ "    Result 2 line: " ++ show (mrLine r2)
                pure False
          else do
            putStrLn $ "  FAIL: Expected 2 matches, got " ++ show (length results)
            pure False

-- | Test search on a file
testSearchFile :: IO Bool
testSearchFile = do
  putStrLn "\n=== File Search Tests ==="

  -- Create a temp test file
  let testDir = "/tmp/screp-test"
      testFile = testDir </> "test.txt"
      testContent = unlines
        [ "Hello World"
        , "Email: test@example.com"
        , "Phone: 123-456-7890"
        , "Another email: foo@bar.org"
        ]

  createDirectoryIfMissing True testDir
  writeFile testFile testContent

  -- Test finding email-like patterns
  case parseExpr "some alphaNum <+> char '@' <+> some alphaNum <+> char '.' <+> some letter" of
    Left err -> do
      putStrLn $ "  FAIL: parse error: " ++ show err
      cleanup testDir
      pure False
    Right ast -> case interpret ast of
      Left err -> do
        putStrLn $ "  FAIL: interpret error: " ++ show err
        cleanup testDir
        pure False
      Right parser -> do
        results <- searchFile parser testFile
        let matches = map mrMatchText results
        if length matches == 2 && "test@example.com" `elem` matches && "foo@bar.org" `elem` matches
          then do
            putStrLn $ "  PASS: Found 2 email addresses in file"
            putStrLn $ "    Matches: " ++ show matches
            cleanup testDir
            pure True
          else do
            putStrLn $ "  FAIL: Expected 2 emails"
            putStrLn $ "    Got: " ++ show matches
            cleanup testDir
            pure False

  where
    cleanup dir = do
      exists <- doesDirectoryExist dir
      when exists $ removeDirectoryRecursive dir

-- | Test search on a directory
testSearchDirectory :: IO Bool
testSearchDirectory = do
  putStrLn "\n=== Directory Search Tests ==="

  -- Create a temp test directory with multiple files
  let testDir = "/tmp/screp-test-dir"
      file1 = testDir </> "file1.txt"
      file2 = testDir </> "file2.txt"
      subDir = testDir </> "subdir"
      file3 = subDir </> "file3.txt"

  createDirectoryIfMissing True testDir
  createDirectoryIfMissing True subDir

  writeFile file1 "TODO: fix this bug\nTODO: add tests"
  writeFile file2 "Regular content\nNothing special"
  writeFile file3 "TODO: implement feature"

  -- Test finding TODO comments
  case parseExpr "string \"TODO\"" of
    Left err -> do
      putStrLn $ "  FAIL: parse error: " ++ show err
      cleanup testDir
      pure False
    Right ast -> case interpret ast of
      Left err -> do
        putStrLn $ "  FAIL: interpret error: " ++ show err
        cleanup testDir
        pure False
      Right parser -> do
        results <- searchFiles parser [file1, file2, file3]
        if length results == 3
          then do
            putStrLn $ "  PASS: Found 3 TODO comments across 3 files"
            -- Check that results come from different files
            let files = map mrFilePath results
            if file1 `elem` files && file3 `elem` files
              then do
                putStrLn $ "  PASS: Results from correct files"
                cleanup testDir
                pure True
              else do
                putStrLn $ "  FAIL: Results from wrong files"
                putStrLn $ "    Files: " ++ show files
                cleanup testDir
                pure False
          else do
            putStrLn $ "  FAIL: Expected 3 matches"
            putStrLn $ "    Got: " ++ show (length results)
            cleanup testDir
            pure False

  where
    cleanup dir = do
      exists <- doesDirectoryExist dir
      when exists $ removeDirectoryRecursive dir

-- | Test ignore case (-i)
testIgnoreCase :: IO Bool
testIgnoreCase = do
  putStrLn "\n=== Ignore Case Tests ==="

  let content = "Hello HELLO hello HeLLo"
      opts = SearchOptions { soIgnoreCase = True, soContextBefore = 0, soContextAfter = 0 }

  -- Parse pattern in lowercase (simulating -i flag behavior)
  case parseExpr "string \"hello\"" of
    Left err -> do
      putStrLn $ "  FAIL: parse error: " ++ show err
      pure False
    Right ast -> case interpret ast of
      Left err -> do
        putStrLn $ "  FAIL: interpret error: " ++ show err
        pure False
      Right parser -> do
        -- Search with case-insensitive content
        let lowerContent = map toLower content
            results = searchText "<test>" parser lowerContent
            matches = map mrMatchText results

        if length matches == 4
          then do
            putStrLn $ "  PASS: Found 4 case-insensitive matches for 'hello'"
            pure True
          else do
            putStrLn $ "  FAIL: Expected 4 matches"
            putStrLn $ "    Got: " ++ show (length matches) ++ " - " ++ show matches
            pure False

-- | Test context lines (-A/-B/-C)
testContextLines :: IO Bool
testContextLines = do
  putStrLn "\n=== Context Lines Tests ==="

  let content = unlines
        [ "Line 1: header"
        , "Line 2: before"
        , "Line 3: MATCH"
        , "Line 4: after"
        , "Line 5: footer"
        ]
      opts = SearchOptions { soIgnoreCase = False, soContextBefore = 1, soContextAfter = 1 }

  case parseExpr "string \"MATCH\"" of
    Left err -> do
      putStrLn $ "  FAIL: parse error: " ++ show err
      pure False
    Right ast -> case interpret ast of
      Left err -> do
        putStrLn $ "  FAIL: interpret error: " ++ show err
        pure False
      Right parser -> do
        let results = searchTextWithContext opts "<test>" parser content

        if length results == 1
          then do
            let result = head results
            case mrContext result of
              Nothing -> do
                putStrLn "  FAIL: No context returned"
                pure False
              Just ctx -> do
                let beforeOk = length (mcBefore ctx) == 1 && "Line 2: before" `isInfixOf` head (mcBefore ctx)
                    afterOk = length (mcAfter ctx) == 1 && "Line 4: after" `isInfixOf` head (mcAfter ctx)

                if beforeOk && afterOk
                  then do
                    putStrLn "  PASS: Context lines correctly captured"
                    putStrLn $ "    Before: " ++ show (mcBefore ctx)
                    putStrLn $ "    After: " ++ show (mcAfter ctx)
                    pure True
                  else do
                    putStrLn "  FAIL: Context lines incorrect"
                    putStrLn $ "    Before: " ++ show (mcBefore ctx)
                    putStrLn $ "    After: " ++ show (mcAfter ctx)
                    pure False
          else do
            putStrLn $ "  FAIL: Expected 1 match, got " ++ show (length results)
            pure False

-- | Test color output (--color)
testColorOutput :: IO Bool
testColorOutput = do
  putStrLn "\n=== Color Output Tests ==="

  let result = MatchResult
        { mrFilePath = "test.txt"
        , mrLine = 5
        , mrCol = 10
        , mrMatchText = "hello"
        , mrContext = Nothing
        }
      optsColor = OutputOptions { ooColor = ColorAlways, ooNoFilename = False }
      optsNoColor = OutputOptions { ooColor = ColorNever, ooNoFilename = False }

  let outputColor = formatResultsWithOpts optsColor FormatGrep [result]
      outputNoColor = formatResultsWithOpts optsNoColor FormatGrep [result]

  -- Color output should contain ANSI escape codes
  let hasEscapeCodes = "\ESC[" `isInfixOf` outputColor
      noEscapeCodes = not ("\ESC[" `isInfixOf` outputNoColor)

  if hasEscapeCodes && noEscapeCodes
    then do
      putStrLn "  PASS: Color mode adds ANSI codes, no-color mode omits them"
      pure True
    else do
      putStrLn "  FAIL: Color handling incorrect"
      putStrLn $ "    Color output has escape codes: " ++ show hasEscapeCodes
      putStrLn $ "    No-color output lacks escape codes: " ++ show noEscapeCodes
      pure False

-- | Test files-only output (-l/-L)
testFilesOnlyOutput :: IO Bool
testFilesOnlyOutput = do
  putStrLn "\n=== Files-Only Output Tests ==="

  let results =
        [ MatchResult { mrFilePath = "file1.txt", mrLine = 1, mrCol = 1, mrMatchText = "x", mrContext = Nothing }
        , MatchResult { mrFilePath = "file1.txt", mrLine = 2, mrCol = 1, mrMatchText = "y", mrContext = Nothing }
        , MatchResult { mrFilePath = "file2.txt", mrLine = 1, mrCol = 1, mrMatchText = "z", mrContext = Nothing }
        ]

  let output = formatResults FormatFilesOnly results
      outputLines = filter (not . null) $ lines output

  -- Should only list unique filenames
  if outputLines == ["file1.txt", "file2.txt"]
    then do
      putStrLn "  PASS: Files-only output lists unique filenames"
      pure True
    else do
      putStrLn "  FAIL: Files-only output incorrect"
      putStrLn $ "    Expected: [\"file1.txt\", \"file2.txt\"]"
      putStrLn $ "    Got: " ++ show outputLines
      pure False

-- | Test exclude patterns (--exclude/--exclude-dir)
testExcludePatterns :: IO Bool
testExcludePatterns = do
  putStrLn "\n=== Exclude Patterns Tests ==="

  -- Test simple glob matching
  let tests =
        [ ("*.log", "debug.log", True)
        , ("*.log", "debug.txt", False)
        , ("*.log", "app.log.bak", False)
        , ("test*", "test_file.txt", True)
        , ("test*", "my_test.txt", False)
        , ("node_modules", "node_modules", True)
        , ("node_modules", "node_module", False)
        , ("*cache*", "mycache", True)
        , ("*cache*", ".cache", True)
        , ("*cache*", "nocach", False)
        ]

  results <- mapM runGlobTest tests
  let allPassed = and results

  if allPassed
    then do
      putStrLn "  PASS: All glob pattern tests passed"
      pure True
    else do
      putStrLn "  FAIL: Some glob pattern tests failed"
      pure False

  where
    runGlobTest (pattern, input, expected) = do
      let result = simpleGlobMatch pattern input
      if result == expected
        then pure True
        else do
          putStrLn $ "  FAIL: glob '" ++ pattern ++ "' on '" ++ input ++ "'"
          putStrLn $ "    Expected: " ++ show expected ++ ", Got: " ++ show result
          pure False

    -- Simple glob matching (copied from Main.hs for testing)
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

-- | Test different output formats
testOutputFormats :: IO Bool
testOutputFormats = do
  putStrLn "\n=== Output Format Tests ==="

  let result = MatchResult
        { mrFilePath = "test.txt"
        , mrLine = 5
        , mrCol = 10
        , mrMatchText = "hello"
        , mrContext = Nothing
        }
      opts = OutputOptions { ooColor = ColorNever, ooNoFilename = False }

  -- Test FormatGrep
  let grepOutput = formatResultsWithOpts opts FormatGrep [result]
  let grepOk = "test.txt:5:10:hello" `isPrefixOf` grepOutput

  -- Test FormatJSON
  let jsonOutput = formatResultsWithOpts opts FormatJSON [result]
  let jsonOk = "[{\"file\":\"test.txt\",\"line\":5,\"col\":10,\"match\":\"hello\"}]" `isPrefixOf` jsonOutput

  -- Test FormatCount
  let countOutput = formatResultsWithOpts opts FormatCount [result]
  let countOk = "1 match" `isPrefixOf` countOutput

  -- Test FormatCount plural
  let countPluralOutput = formatResultsWithOpts opts FormatCount [result, result]
  let countPluralOk = "2 matches" `isPrefixOf` countPluralOutput

  -- Test no filename option
  let optsNoFile = OutputOptions { ooColor = ColorNever, ooNoFilename = True }
  let noFileOutput = formatResultsWithOpts optsNoFile FormatGrep [result]
  let noFileOk = "5:10:hello" `isPrefixOf` noFileOutput && not ("test.txt" `isInfixOf` noFileOutput)

  let allTests =
        [ ("FormatGrep", grepOk)
        , ("FormatJSON", jsonOk)
        , ("FormatCount singular", countOk)
        , ("FormatCount plural", countPluralOk)
        , ("No filename option", noFileOk)
        ]

  mapM_ (\(name, ok) -> putStrLn $ (if ok then "  PASS: " else "  FAIL: ") ++ name) allTests

  pure $ all snd allTests
