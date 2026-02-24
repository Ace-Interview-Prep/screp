module Main where

import Scrappy.Grep.DSL
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret)
import Scrappy.Grep.Search (searchText, searchFile, searchFiles)
import Scrappy.Grep.Output (formatResults, OutputFormat(..))

import System.Exit (exitFailure, exitSuccess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Running pgrep tests...\n"

  -- Run all tests
  results <- sequence
    [ testDSLParser
    , testInterpreter
    , testSearchString
    , testSearchFile
    , testSearchDirectory
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
  let testDir = "/tmp/parsec-grep-test"
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
  let testDir = "/tmp/parsec-grep-test-dir"
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
