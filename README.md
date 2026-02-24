# scrappy-file

File system utilities for the scrappy web scraping library, plus **pgrep** - a grep-like CLI that uses Parsec parsers instead of regex.

## pgrep

Search files using Parsec parser patterns instead of regular expressions.

### Installation

```bash
cabal build pgrep
cabal install pgrep
```

Make sure `~/.local/bin` is in your PATH:
```bash
export PATH="$HOME/.local/bin:$PATH"
```

### Usage

```bash
pgrep PATTERN FILE...
pgrep [OPTIONS] PATTERN FILE...
```

### DSL Primitives

| Primitive | Example | Description |
|-----------|---------|-------------|
| `char` | `char 'x'` | Single character |
| `string` | `string "abc"` | Literal string |
| `anyChar` | `anyChar` | Any character |
| `digit` | `digit` | 0-9 |
| `letter` | `letter` | a-z, A-Z |
| `alphaNum` | `alphaNum` | Letter or digit |
| `space` | `space` | Single whitespace |
| `spaces` | `spaces` | Zero or more whitespace |
| `newline` | `newline` | Newline character |
| `oneOf` | `oneOf "abc"` | Match one of chars |
| `noneOf` | `noneOf "xyz"` | Match none of chars |

### DSL Combinators

| Combinator | Syntax | Description |
|------------|--------|-------------|
| Sequence | `p1 >> p2` | Run p1 then p2, return p2's result |
| Concat | `p1 <+> p2` | Run both, concatenate results |
| Alternative | `p1 <\|> p2` | Try p1, else p2 |
| Many | `many p` | Zero or more |
| Some | `some p` | One or more |
| Optional | `optional p` | Zero or one |
| Try | `try p` | Backtracking |
| Between | `between '(' ')' p` | Parse between delimiters |
| Count | `count 3 p` | Exactly n repetitions |
| ManyTill | `manyTill p end` | Non-greedy: match p until end |

### Examples

```bash
# Find digits
pgrep 'some digit' file.txt

# Find email patterns
pgrep 'some alphaNum <+> char '\''@'\'' <+> some alphaNum <+> char '\''.'\'' <+> some letter' contacts.txt

# Find TODO comments
pgrep 'string "TODO"' -r ./src/

# Search recursively with extension filter
pgrep -r -e .hs 'string "import"' ./src/

# Count matches
pgrep -c 'digit' data.txt

# JSON output
pgrep --json 'some digit' file.txt

# Non-greedy matching: find everything between X and Y
pgrep 'string "START" <+> manyTill anyChar (string "END")' file.txt
```

### Options

```
-r, --recursive      Search directories recursively
-e, --extension EXT  Only search files with extension (e.g., .hs, .txt)
-v, --verbose        Verbose output with full match text
-c, --count          Only print count of matches
-q, --quiet          Quiet mode (exit code only)
-m, --max-results N  Maximum number of results to show
--json               Output results as JSON
```

### Output Format

Default output is grep-like:
```
file.txt:1:28:123
file.txt:2:5:test@example.com
```

Format: `filepath:line:column:matched_text`

## Library API

The `Scrappy.Grep.*` modules expose:

- `Scrappy.Grep.DSL` - AST types (`ParserExpr`, `MatchResult`)
- `Scrappy.Grep.DSL.Parser` - Parse DSL strings to AST
- `Scrappy.Grep.DSL.Interpreter` - Convert AST to Parsec parsers
- `Scrappy.Grep.Search` - File/directory search functions
- `Scrappy.Grep.Output` - Result formatting
