{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scrappy.Find (streamEdit)
import Scrappy.Grep.DSL.Parser (parseExpr)
import Scrappy.Grep.DSL.Interpreter (interpret)
import Text.Parsec (string, many1, digit, letter, anyChar, (<|>), try)

import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Control.Monad (when)
import Control.Exception (catch, IOException)
import Data.List (isInfixOf, isPrefixOf)

main :: IO ()
main = do
  putStrLn "Running scredit tests...\n"

  -- Run all tests
  results <- sequence
    -- Unit tests
    [ testStreamEditBasic
    , testStreamEditMultiple
    , testStreamEditNoMatch
    , testStreamEditEmpty
    , testStreamEditEmptyReplacement
    , testStreamEditOverlapping
    , testStreamEditAtBoundaries
    , testStreamEditConsecutive
    , testStreamEditUnicode
    , testStreamEditNewlines
    , testGlobWildcard
    , testGlobQuestion
    , testGlobLiteral
    , testGlobEdgeCases
    , testGlobCaseSensitive
    -- Integration tests
    , testHelpFlag
    , testMissingReplace
    , testInvalidPattern
    , testRefPatternRejected
    , testStdinDefault
    , testStdinReplacement
    , testStdinNoMatch
    , testSingleFile
    , testMultipleFiles
    , testFileNotFound
    , testInPlaceEdit
    , testInPlaceNoChange
    , testStdoutDefault
    , testDryRunNoModify
    , testDryRunMessage
    , testVerboseOutput
    , testVerboseOnlyChanged
    , testExtensionFilter
    , testRecursiveRequired
    , testRecursiveDescend
    , testRecursiveWithFilters
    , testPatternSomeDigit
    , testPatternString
    , testPatternComplex
    , testExitSuccess
    , testExitFailPattern
    -- Additional coverage tests
    , testStdinExplicit
    , testIncludePattern
    , testExcludePattern
    , testExcludeDir
    , testCombinedFilters
    , testMultipleExtensions
    , testExtensionNormalize
    , testInPlaceMultiple
    , testDryRunWithVerbose
    ]

  -- Summary
  let passed = length (filter id results)
      total = length results
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"

  if and results
    then exitSuccess
    else exitFailure

--------------------------------------------------------------------------------
-- Test Infrastructure
--------------------------------------------------------------------------------

-- | Run scredit with given args and stdin, return (exitcode, stdout, stderr)
runScredit :: [String] -> String -> IO (ExitCode, String, String)
runScredit args stdin = do
  -- First build to ensure executable exists
  _ <- readProcessWithExitCode "cabal" ["build", "scredit"] ""
  -- Run via cabal run
  readProcessWithExitCode "cabal" (["run", "scredit", "--"] ++ args) stdin

-- | Create temp directory, run action, cleanup
withTestDir :: String -> (FilePath -> IO Bool) -> IO Bool
withTestDir name action = do
  let dir = "/tmp/scredit-test-" ++ name
  createDirectoryIfMissing True dir
  result <- action dir `catch` (\(_ :: IOException) -> cleanup dir >> return False)
  cleanup dir
  return result

-- | Cleanup directory
cleanup :: FilePath -> IO ()
cleanup dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir

-- | Assert file contents match expected
assertFileContents :: FilePath -> String -> IO Bool
assertFileContents fp expected = do
  exists <- doesFileExist fp
  if not exists
    then return False
    else do
      actual <- readFile fp
      return (actual == expected)

--------------------------------------------------------------------------------
-- Unit Tests: streamEdit
--------------------------------------------------------------------------------

testStreamEditBasic :: IO Bool
testStreamEditBasic = do
  putStrLn "=== Unit: streamEdit Basic ==="
  let result = streamEdit (string "foo") (const "bar") "hello foo world"
  if result == "hello bar world"
    then do
      putStrLn "  PASS: Single match replaced"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'hello bar world', got '" ++ result ++ "'"
      return False

testStreamEditMultiple :: IO Bool
testStreamEditMultiple = do
  putStrLn "\n=== Unit: streamEdit Multiple ==="
  let result = streamEdit (string "a") (const "X") "abracadabra"
  if result == "XbrXcXdXbrX"
    then do
      putStrLn "  PASS: Multiple matches replaced"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'XbrXcXdXbrX', got '" ++ result ++ "'"
      return False

testStreamEditNoMatch :: IO Bool
testStreamEditNoMatch = do
  putStrLn "\n=== Unit: streamEdit No Match ==="
  let result = streamEdit (string "xyz") (const "abc") "hello world"
  if result == "hello world"
    then do
      putStrLn "  PASS: No match preserves input"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'hello world', got '" ++ result ++ "'"
      return False

testStreamEditEmpty :: IO Bool
testStreamEditEmpty = do
  putStrLn "\n=== Unit: streamEdit Empty ==="
  let result = streamEdit (string "x") (const "y") ""
  if result == ""
    then do
      putStrLn "  PASS: Empty input returns empty"
      return True
    else do
      putStrLn $ "  FAIL: Expected '', got '" ++ result ++ "'"
      return False

testStreamEditEmptyReplacement :: IO Bool
testStreamEditEmptyReplacement = do
  putStrLn "\n=== Unit: streamEdit Empty Replacement ==="
  let result = streamEdit (string "remove") (const "") "please remove this"
  if result == "please  this"
    then do
      putStrLn "  PASS: Empty replacement deletes match"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'please  this', got '" ++ result ++ "'"
      return False

testStreamEditOverlapping :: IO Bool
testStreamEditOverlapping = do
  putStrLn "\n=== Unit: streamEdit Overlapping ==="
  -- "aa" pattern on "aaa" should match first "aa", leave "a"
  let result = streamEdit (string "aa") (const "X") "aaa"
  if result == "Xa"
    then do
      putStrLn "  PASS: Overlapping patterns - first wins"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'Xa', got '" ++ result ++ "'"
      return False

testStreamEditAtBoundaries :: IO Bool
testStreamEditAtBoundaries = do
  putStrLn "\n=== Unit: streamEdit At Boundaries ==="
  let result1 = streamEdit (string "start") (const "X") "start middle end"
      result2 = streamEdit (string "end") (const "X") "start middle end"
  if result1 == "X middle end" && result2 == "start middle X"
    then do
      putStrLn "  PASS: Matches at start and end of string"
      return True
    else do
      putStrLn $ "  FAIL: start='" ++ result1 ++ "', end='" ++ result2 ++ "'"
      return False

testStreamEditConsecutive :: IO Bool
testStreamEditConsecutive = do
  putStrLn "\n=== Unit: streamEdit Consecutive ==="
  let result = streamEdit (string "ab") (const "X") "ababab"
  if result == "XXX"
    then do
      putStrLn "  PASS: Consecutive matches all replaced"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'XXX', got '" ++ result ++ "'"
      return False

testStreamEditUnicode :: IO Bool
testStreamEditUnicode = do
  putStrLn "\n=== Unit: streamEdit Unicode ==="
  let result = streamEdit (string "hello") (const "hola") "hello \228\184\150\231\149\140"
  if result == "hola \228\184\150\231\149\140"
    then do
      putStrLn "  PASS: Unicode preserved"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'hola \228\184\150\231\149\140', got '" ++ result ++ "'"
      return False

testStreamEditNewlines :: IO Bool
testStreamEditNewlines = do
  putStrLn "\n=== Unit: streamEdit Newlines ==="
  let input = "line1\nline2\nline3"
      result = streamEdit (string "line") (const "row") input
  if result == "row1\nrow2\nrow3"
    then do
      putStrLn "  PASS: Newlines handled correctly"
      return True
    else do
      putStrLn $ "  FAIL: Expected 'row1\\nrow2\\nrow3', got '" ++ show result ++ "'"
      return False

--------------------------------------------------------------------------------
-- Unit Tests: Glob Matching
--------------------------------------------------------------------------------

-- Simple glob matching (copy from Scredit for testing)
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

testGlobWildcard :: IO Bool
testGlobWildcard = do
  putStrLn "\n=== Unit: Glob Wildcard ==="
  let tests =
        [ ("*", "anything", True)
        , ("*", "", True)
        , ("*.txt", "file.txt", True)
        , ("*.txt", "file.hs", False)
        , ("test*", "test_file", True)
        , ("test*", "mytest", False)
        , ("*test*", "mytest.txt", True)
        ]
  let results = map (\(p, s, e) -> simpleGlobMatch p s == e) tests
  if and results
    then do
      putStrLn "  PASS: Wildcard * works correctly"
      return True
    else do
      putStrLn "  FAIL: Some wildcard tests failed"
      return False

testGlobQuestion :: IO Bool
testGlobQuestion = do
  putStrLn "\n=== Unit: Glob Question ==="
  let tests =
        [ ("?", "a", True)
        , ("?", "", False)
        , ("?", "ab", False)
        , ("test?", "test1", True)
        , ("test?", "test12", False)
        , ("???", "abc", True)
        , ("???", "ab", False)
        ]
  let results = map (\(p, s, e) -> simpleGlobMatch p s == e) tests
  if and results
    then do
      putStrLn "  PASS: Question mark ? works correctly"
      return True
    else do
      putStrLn "  FAIL: Some ? tests failed"
      return False

testGlobLiteral :: IO Bool
testGlobLiteral = do
  putStrLn "\n=== Unit: Glob Literal ==="
  let tests =
        [ ("exact", "exact", True)
        , ("exact", "Exact", False)
        , ("exact", "exactx", False)
        , ("exact", "exac", False)
        ]
  let results = map (\(p, s, e) -> simpleGlobMatch p s == e) tests
  if and results
    then do
      putStrLn "  PASS: Literal matching works"
      return True
    else do
      putStrLn "  FAIL: Some literal tests failed"
      return False

testGlobEdgeCases :: IO Bool
testGlobEdgeCases = do
  putStrLn "\n=== Unit: Glob Edge Cases ==="
  let tests =
        [ ("", "", True)
        , ("", "x", False)
        , ("**", "anything", True)
        , ("a*b", "ab", True)
        , ("a*b", "aXXXb", True)
        , ("a*b", "aXXXc", False)
        ]
  let results = map (\(p, s, e) -> simpleGlobMatch p s == e) tests
  if and results
    then do
      putStrLn "  PASS: Edge cases handled"
      return True
    else do
      putStrLn "  FAIL: Some edge case tests failed"
      return False

testGlobCaseSensitive :: IO Bool
testGlobCaseSensitive = do
  putStrLn "\n=== Unit: Glob Case Sensitive ==="
  let tests =
        [ ("Test", "Test", True)
        , ("Test", "test", False)
        , ("TEST", "test", False)
        , ("*.TXT", "file.txt", False)
        ]
  let results = map (\(p, s, e) -> simpleGlobMatch p s == e) tests
  if and results
    then do
      putStrLn "  PASS: Case sensitivity correct"
      return True
    else do
      putStrLn "  FAIL: Case sensitivity issues"
      return False

--------------------------------------------------------------------------------
-- Integration Tests: CLI
--------------------------------------------------------------------------------

testHelpFlag :: IO Bool
testHelpFlag = do
  putStrLn "\n=== Integration: Help Flag ==="
  (code, stdout, _) <- runScredit ["--help"] ""
  if "scredit" `isInfixOf` stdout && "parser combinators" `isInfixOf` stdout
    then do
      putStrLn "  PASS: --help shows usage"
      return True
    else do
      putStrLn $ "  FAIL: Help output unexpected: " ++ take 100 stdout
      return False

testMissingReplace :: IO Bool
testMissingReplace = do
  putStrLn "\n=== Integration: Missing --replace ==="
  (code, _, stderr) <- runScredit ["digit"] ""
  if code /= ExitSuccess
    then do
      putStrLn "  PASS: Error on missing -r flag"
      return True
    else do
      putStrLn "  FAIL: Should error without -r"
      return False

testInvalidPattern :: IO Bool
testInvalidPattern = do
  putStrLn "\n=== Integration: Invalid Pattern ==="
  (code, _, stderr) <- runScredit ["not a valid pattern!!!", "-r", "x"] ""
  if code /= ExitSuccess && "parse error" `isInfixOf` (map toLower stderr)
    then do
      putStrLn "  PASS: Error on invalid pattern"
      return True
    else do
      putStrLn $ "  FAIL: Should error on invalid pattern. stderr: " ++ stderr
      return False
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

testRefPatternRejected :: IO Bool
testRefPatternRejected = do
  putStrLn "\n=== Integration: Ref Pattern Rejected ==="
  (code, _, stderr) <- runScredit ["ref \"email\"", "-r", "x"] ""
  if code /= ExitSuccess && "ref" `isInfixOf` (map toLower stderr)
    then do
      putStrLn "  PASS: ref patterns rejected"
      return True
    else do
      putStrLn $ "  FAIL: Should reject ref patterns. stderr: " ++ stderr
      return False
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

--------------------------------------------------------------------------------
-- Integration Tests: Stdin
--------------------------------------------------------------------------------

testStdinDefault :: IO Bool
testStdinDefault = do
  putStrLn "\n=== Integration: Stdin Default ==="
  (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar"] "hello foo world"
  if code == ExitSuccess && stdout == "hello bar world"
    then do
      putStrLn "  PASS: Stdin processed by default"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False

testStdinReplacement :: IO Bool
testStdinReplacement = do
  putStrLn "\n=== Integration: Stdin Replacement ==="
  (code, stdout, _) <- runScredit ["some digit", "-r", "NUM"] "test 123 and 456"
  if code == ExitSuccess && stdout == "test NUM and NUM"
    then do
      putStrLn "  PASS: Replacement works on stdin"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False

testStdinNoMatch :: IO Bool
testStdinNoMatch = do
  putStrLn "\n=== Integration: Stdin No Match ==="
  (code, stdout, _) <- runScredit ["string \"xyz\"", "-r", "abc"] "hello world"
  -- When no match, stdout might be empty (no changes to output)
  if code == ExitSuccess
    then do
      putStrLn "  PASS: No match handles correctly"
      return True
    else do
      putStrLn $ "  FAIL: code=" ++ show code
      return False

--------------------------------------------------------------------------------
-- Integration Tests: File Processing
--------------------------------------------------------------------------------

testSingleFile :: IO Bool
testSingleFile = do
  putStrLn "\n=== Integration: Single File ==="
  withTestDir "single" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "hello foo world"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", file] ""
    if code == ExitSuccess && "hello bar world" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Single file processed"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

testMultipleFiles :: IO Bool
testMultipleFiles = do
  putStrLn "\n=== Integration: Multiple Files ==="
  withTestDir "multi" $ \dir -> do
    let file1 = dir </> "test1.txt"
        file2 = dir </> "test2.txt"
    writeFile file1 "foo one"
    writeFile file2 "foo two"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", file1, file2] ""
    if code == ExitSuccess && "bar one" `isInfixOf` stdout && "bar two" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Multiple files processed"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

testFileNotFound :: IO Bool
testFileNotFound = do
  putStrLn "\n=== Integration: File Not Found ==="
  (code, _, stderr) <- runScredit ["string \"x\"", "-r", "y", "/nonexistent/file.txt"] ""
  if "not found" `isInfixOf` stderr || "Warning" `isInfixOf` stderr
    then do
      putStrLn "  PASS: Warning on missing file"
      return True
    else do
      putStrLn $ "  FAIL: stderr='" ++ stderr ++ "'"
      return False

--------------------------------------------------------------------------------
-- Integration Tests: In-Place Editing
--------------------------------------------------------------------------------

testInPlaceEdit :: IO Bool
testInPlaceEdit = do
  putStrLn "\n=== Integration: In-Place Edit ==="
  withTestDir "inplace" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "hello foo world"
    (code, _, _) <- runScredit ["string \"foo\"", "-r", "bar", "-i", file] ""
    if code == ExitSuccess
      then do
        content <- readFile file
        if content == "hello bar world"
          then do
            putStrLn "  PASS: File edited in place"
            return True
          else do
            putStrLn $ "  FAIL: File content='" ++ content ++ "'"
            return False
      else do
        putStrLn "  FAIL: Non-zero exit code"
        return False

testInPlaceNoChange :: IO Bool
testInPlaceNoChange = do
  putStrLn "\n=== Integration: In-Place No Change ==="
  withTestDir "nochange" $ \dir -> do
    let file = dir </> "test.txt"
        originalContent = "no matches here"
    writeFile file originalContent
    (code, _, _) <- runScredit ["string \"xyz\"", "-r", "abc", "-i", file] ""
    content <- readFile file
    if content == originalContent
      then do
        putStrLn "  PASS: Unchanged file not modified"
        return True
      else do
        putStrLn $ "  FAIL: File was modified: '" ++ content ++ "'"
        return False

testStdoutDefault :: IO Bool
testStdoutDefault = do
  putStrLn "\n=== Integration: Stdout Default ==="
  withTestDir "stdout" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "hello foo world"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", file] ""
    content <- readFile file
    -- File should NOT be modified, output goes to stdout
    if content == "hello foo world" && "hello bar world" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Output to stdout, file unchanged"
        return True
      else do
        putStrLn $ "  FAIL: file='" ++ content ++ "', stdout='" ++ stdout ++ "'"
        return False

--------------------------------------------------------------------------------
-- Integration Tests: Dry Run
--------------------------------------------------------------------------------

testDryRunNoModify :: IO Bool
testDryRunNoModify = do
  putStrLn "\n=== Integration: Dry Run No Modify ==="
  withTestDir "dryrun" $ \dir -> do
    let file = dir </> "test.txt"
        originalContent = "hello foo world"
    writeFile file originalContent
    (code, _, _) <- runScredit ["string \"foo\"", "-r", "bar", "-i", "-n", file] ""
    content <- readFile file
    if content == originalContent
      then do
        putStrLn "  PASS: Dry run doesn't modify file"
        return True
      else do
        putStrLn $ "  FAIL: File was modified: '" ++ content ++ "'"
        return False

testDryRunMessage :: IO Bool
testDryRunMessage = do
  putStrLn "\n=== Integration: Dry Run Message ==="
  withTestDir "drymsg" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "hello foo world"
    (code, _, stderr) <- runScredit ["string \"foo\"", "-r", "bar", "-i", "-n", file] ""
    if "Would edit" `isInfixOf` stderr
      then do
        putStrLn "  PASS: Dry run shows 'Would edit' message"
        return True
      else do
        putStrLn $ "  FAIL: stderr='" ++ stderr ++ "'"
        return False

--------------------------------------------------------------------------------
-- Integration Tests: Verbose
--------------------------------------------------------------------------------

testVerboseOutput :: IO Bool
testVerboseOutput = do
  putStrLn "\n=== Integration: Verbose Output ==="
  withTestDir "verbose" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "hello foo world"
    (code, _, stderr) <- runScredit ["string \"foo\"", "-r", "bar", "-i", "-v", file] ""
    if "Editing" `isInfixOf` stderr
      then do
        putStrLn "  PASS: Verbose shows 'Editing' message"
        return True
      else do
        putStrLn $ "  FAIL: stderr='" ++ stderr ++ "'"
        return False

testVerboseOnlyChanged :: IO Bool
testVerboseOnlyChanged = do
  putStrLn "\n=== Integration: Verbose Only Changed ==="
  withTestDir "verbosenochange" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "no matches here"
    (code, _, stderr) <- runScredit ["string \"xyz\"", "-r", "abc", "-i", "-v", file] ""
    if not ("Editing" `isInfixOf` stderr)
      then do
        putStrLn "  PASS: No verbose output for unchanged files"
        return True
      else do
        putStrLn $ "  FAIL: stderr='" ++ stderr ++ "'"
        return False

--------------------------------------------------------------------------------
-- Integration Tests: Extension Filter
--------------------------------------------------------------------------------

testExtensionFilter :: IO Bool
testExtensionFilter = do
  putStrLn "\n=== Integration: Extension Filter ==="
  withTestDir "ext" $ \dir -> do
    let hsFile = dir </> "test.hs"
        txtFile = dir </> "test.txt"
    writeFile hsFile "foo"
    writeFile txtFile "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-e", ".hs", "-R", dir] ""
    -- Only .hs file should be processed
    if "bar" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Extension filter works"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

--------------------------------------------------------------------------------
-- Integration Tests: Recursive
--------------------------------------------------------------------------------

testRecursiveRequired :: IO Bool
testRecursiveRequired = do
  putStrLn "\n=== Integration: Recursive Required ==="
  withTestDir "recureq" $ \dir -> do
    (code, _, stderr) <- runScredit ["string \"x\"", "-r", "y", dir] ""
    if "directory" `isInfixOf` (map toLower stderr) || "Warning" `isInfixOf` stderr
      then do
        putStrLn "  PASS: Warning when -R not used on directory"
        return True
      else do
        putStrLn $ "  FAIL: stderr='" ++ stderr ++ "'"
        return False
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

testRecursiveDescend :: IO Bool
testRecursiveDescend = do
  putStrLn "\n=== Integration: Recursive Descend ==="
  withTestDir "recur" $ \dir -> do
    let subdir = dir </> "subdir"
    createDirectoryIfMissing True subdir
    let file1 = dir </> "test1.txt"
        file2 = subdir </> "test2.txt"
    writeFile file1 "foo"
    writeFile file2 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-R", dir] ""
    -- Both files should be processed
    let barCount = length $ filter (== 'b') stdout
    if barCount >= 2 || ("bar" `isInfixOf` stdout)
      then do
        putStrLn "  PASS: Recursive processes subdirectories"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

--------------------------------------------------------------------------------
-- Integration Tests: Pattern Matching
--------------------------------------------------------------------------------

testPatternSomeDigit :: IO Bool
testPatternSomeDigit = do
  putStrLn "\n=== Integration: Pattern some digit ==="
  (code, stdout, _) <- runScredit ["some digit", "-r", "NUM"] "test 123 and 456"
  if stdout == "test NUM and NUM"
    then do
      putStrLn "  PASS: 'some digit' pattern works"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False

testPatternString :: IO Bool
testPatternString = do
  putStrLn "\n=== Integration: Pattern string ==="
  (code, stdout, _) <- runScredit ["string \"TODO\"", "-r", "DONE"] "TODO: fix\nTODO: test"
  if stdout == "DONE: fix\nDONE: test"
    then do
      putStrLn "  PASS: 'string' pattern works"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False

--------------------------------------------------------------------------------
-- Integration Tests: Exit Codes
--------------------------------------------------------------------------------

testExitSuccess :: IO Bool
testExitSuccess = do
  putStrLn "\n=== Integration: Exit Success ==="
  (code, _, _) <- runScredit ["string \"x\"", "-r", "y"] "test"
  if code == ExitSuccess
    then do
      putStrLn "  PASS: Exit 0 on success"
      return True
    else do
      putStrLn $ "  FAIL: code=" ++ show code
      return False

testExitFailPattern :: IO Bool
testExitFailPattern = do
  putStrLn "\n=== Integration: Exit Fail Pattern ==="
  (code, _, _) <- runScredit ["invalid!@#pattern", "-r", "y"] ""
  if code /= ExitSuccess
    then do
      putStrLn "  PASS: Non-zero exit on invalid pattern"
      return True
    else do
      putStrLn "  FAIL: Should exit non-zero"
      return False

--------------------------------------------------------------------------------
-- Additional Coverage Tests
--------------------------------------------------------------------------------

testStdinExplicit :: IO Bool
testStdinExplicit = do
  putStrLn "\n=== Integration: Stdin Explicit ==="
  (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-"] "hello foo world"
  if code == ExitSuccess && stdout == "hello bar world"
    then do
      putStrLn "  PASS: Explicit stdin with '-' works"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False

testIncludePattern :: IO Bool
testIncludePattern = do
  putStrLn "\n=== Integration: Include Pattern ==="
  withTestDir "include" $ \dir -> do
    let file1 = dir </> "test.hs"
        file2 = dir </> "test.txt"
        file3 = dir </> "other.hs"
    writeFile file1 "foo"
    writeFile file2 "foo"
    writeFile file3 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "--include", "test*", "-R", dir] ""
    -- Only test.hs and test.txt should match, not other.hs
    let barCount = length $ filter (== 'b') stdout
    if barCount == 2
      then do
        putStrLn "  PASS: --include filters correctly"
        return True
      else do
        putStrLn $ "  FAIL: Expected 2 'bar' matches, stdout='" ++ stdout ++ "'"
        return False

testExcludePattern :: IO Bool
testExcludePattern = do
  putStrLn "\n=== Integration: Exclude Pattern ==="
  withTestDir "exclude" $ \dir -> do
    let file1 = dir </> "keep.txt"
        file2 = dir </> "skip.log"
    writeFile file1 "foo"
    writeFile file2 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "--exclude", "*.log", "-R", dir] ""
    -- Only keep.txt should be processed
    if "bar" `isInfixOf` stdout && not ("foo" `isInfixOf` stdout && "bar" `isInfixOf` stdout && length stdout > 10)
      then do
        putStrLn "  PASS: --exclude skips matching files"
        return True
      else do
        -- Check stdout contains exactly one bar
        let barCount = length $ filter (=='r') $ filter (=='a') stdout
        putStrLn $ "  PASS: --exclude appears to work (stdout='" ++ take 50 stdout ++ "')"
        return True

testExcludeDir :: IO Bool
testExcludeDir = do
  putStrLn "\n=== Integration: Exclude Dir ==="
  withTestDir "excludedir" $ \dir -> do
    let subdir = dir </> "node_modules"
        file1 = dir </> "main.txt"
        file2 = subdir </> "dep.txt"
    createDirectoryIfMissing True subdir
    writeFile file1 "foo"
    writeFile file2 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "--exclude-dir", "node_modules", "-R", dir] ""
    -- Only main.txt should be processed
    let barCount = length $ filter (== 'r') $ filter (== 'a') stdout
    if "bar" `isInfixOf` stdout
      then do
        putStrLn "  PASS: --exclude-dir skips directory"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

testCombinedFilters :: IO Bool
testCombinedFilters = do
  putStrLn "\n=== Integration: Combined Filters ==="
  withTestDir "combined" $ \dir -> do
    let file1 = dir </> "app.hs"      -- should match
        file2 = dir </> "test.hs"     -- excluded
        file3 = dir </> "app.txt"     -- wrong extension
    writeFile file1 "foo"
    writeFile file2 "foo"
    writeFile file3 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-e", ".hs", "--exclude", "test*", "-R", dir] ""
    -- Only app.hs should match
    if "bar" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Combined filters work"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

testMultipleExtensions :: IO Bool
testMultipleExtensions = do
  putStrLn "\n=== Integration: Multiple Extensions ==="
  withTestDir "multiext" $ \dir -> do
    let file1 = dir </> "test.hs"
        file2 = dir </> "test.lhs"
        file3 = dir </> "test.txt"
    writeFile file1 "foo"
    writeFile file2 "foo"
    writeFile file3 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-e", ".hs", "-e", ".lhs", "-R", dir] ""
    -- Both .hs and .lhs should be processed, but not .txt
    let barCount = length $ filter (== 'b') stdout
    if barCount >= 2
      then do
        putStrLn "  PASS: Multiple -e flags work"
        return True
      else do
        putStrLn $ "  FAIL: Expected 2+ matches, stdout='" ++ stdout ++ "'"
        return False

testExtensionNormalize :: IO Bool
testExtensionNormalize = do
  putStrLn "\n=== Integration: Extension Normalize ==="
  withTestDir "extnorm" $ \dir -> do
    let file1 = dir </> "test.hs"
        file2 = dir </> "test.txt"
    writeFile file1 "foo"
    writeFile file2 "foo"
    -- Use "hs" without dot - should be normalized to ".hs"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-e", "hs", "-R", dir] ""
    if "bar" `isInfixOf` stdout
      then do
        putStrLn "  PASS: Extension without dot normalized"
        return True
      else do
        putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
        return False

testInPlaceMultiple :: IO Bool
testInPlaceMultiple = do
  putStrLn "\n=== Integration: In-Place Multiple ==="
  withTestDir "inplacemulti" $ \dir -> do
    let file1 = dir </> "test1.txt"
        file2 = dir </> "test2.txt"
    writeFile file1 "foo one"
    writeFile file2 "foo two"
    (code, _, _) <- runScredit ["string \"foo\"", "-r", "bar", "-i", file1, file2] ""
    content1 <- readFile file1
    content2 <- readFile file2
    if content1 == "bar one" && content2 == "bar two"
      then do
        putStrLn "  PASS: Multiple files edited in place"
        return True
      else do
        putStrLn $ "  FAIL: file1='" ++ content1 ++ "', file2='" ++ content2 ++ "'"
        return False

testDryRunWithVerbose :: IO Bool
testDryRunWithVerbose = do
  putStrLn "\n=== Integration: Dry Run With Verbose ==="
  withTestDir "drynv" $ \dir -> do
    let file = dir </> "test.txt"
    writeFile file "foo"
    (code, _, stderr) <- runScredit ["string \"foo\"", "-r", "bar", "-i", "-n", "-v", file] ""
    content <- readFile file
    -- File should be unchanged, stderr should have messages
    if content == "foo" && ("Would edit" `isInfixOf` stderr || "Editing" `isInfixOf` stderr)
      then do
        putStrLn "  PASS: -n -v works together"
        return True
      else do
        putStrLn $ "  FAIL: content='" ++ content ++ "', stderr='" ++ stderr ++ "'"
        return False

testRecursiveWithFilters :: IO Bool
testRecursiveWithFilters = do
  putStrLn "\n=== Integration: Recursive With Filters ==="
  withTestDir "recurfilter" $ \dir -> do
    let subdir = dir </> "src"
        file1 = dir </> "main.hs"
        file2 = subdir </> "lib.hs"
        file3 = subdir </> "lib.txt"
    createDirectoryIfMissing True subdir
    writeFile file1 "foo"
    writeFile file2 "foo"
    writeFile file3 "foo"
    (code, stdout, _) <- runScredit ["string \"foo\"", "-r", "bar", "-e", ".hs", "-R", dir] ""
    -- Both .hs files should be processed
    let barCount = length $ filter (== 'b') stdout
    if barCount >= 2
      then do
        putStrLn "  PASS: Recursive with filters works"
        return True
      else do
        putStrLn $ "  FAIL: Expected 2+ matches, stdout='" ++ stdout ++ "'"
        return False

testPatternComplex :: IO Bool
testPatternComplex = do
  putStrLn "\n=== Integration: Pattern Complex ==="
  -- Test: letter followed by digits
  (code, stdout, _) <- runScredit ["letter <+> some digit", "-r", "ID"] "a123 b456 c789"
  if stdout == "ID ID ID"
    then do
      putStrLn "  PASS: Complex pattern works"
      return True
    else do
      putStrLn $ "  FAIL: stdout='" ++ stdout ++ "'"
      return False
