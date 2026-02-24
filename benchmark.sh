#!/usr/bin/env bash

# Benchmark: pgrep vs grep
# Run from scrappy-file directory inside nix-shell

set -e

# Create test data
TEST_DIR="/tmp/pgrep-benchmark"
rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

echo "=== Generating test data ==="

# Generate a large file with emails, numbers, and text
for i in $(seq 1 1000); do
    echo "Line $i: This is some text with email user${i}@example.com and number $RANDOM"
    echo "More text here without patterns just filler content to make it realistic"
    echo "TODO: fix bug #$i in module foo_${i}.hs"
    echo "Contact: admin@company${i}.org or support@site${i}.net"
done > "$TEST_DIR/large.txt"

# Generate multiple files
for i in $(seq 1 100); do
    echo "File $i content with email test${i}@domain.com" > "$TEST_DIR/file_${i}.txt"
    echo "import Data.Map" >> "$TEST_DIR/file_${i}.txt"
    echo "import Control.Monad" >> "$TEST_DIR/file_${i}.txt"
    echo "TODO: implement feature $i" >> "$TEST_DIR/file_${i}.txt"
done

echo "Generated: $(wc -l < "$TEST_DIR/large.txt") lines in large.txt"
echo "Generated: 100 small files"
echo ""

# Get pgrep path
PGREP="$HOME/.local/bin/pgrep"
if [ ! -f "$PGREP" ]; then
    PGREP="$(cabal list-bin pgrep 2>/dev/null || echo "")"
fi

if [ -z "$PGREP" ] || [ ! -f "$PGREP" ]; then
    echo "Building pgrep..."
    cabal build pgrep
    PGREP="$(cabal list-bin pgrep)"
fi

echo "Using pgrep at: $PGREP"
echo ""

echo "=== Benchmark 1: Simple string match (single large file) ==="
echo "Pattern: 'TODO'"
echo ""

echo "grep:"
time grep -c "TODO" "$TEST_DIR/large.txt"
echo ""

echo "pgrep:"
time "$PGREP" -c 'string "TODO"' "$TEST_DIR/large.txt"
echo ""

echo "=== Benchmark 2: Digit sequences (single large file) ==="
echo "Pattern: one or more digits"
echo ""

echo "grep -E '[0-9]+':"
time grep -oE '[0-9]+' "$TEST_DIR/large.txt" | wc -l
echo ""

echo "pgrep 'some digit':"
time "$PGREP" -c 'some digit' "$TEST_DIR/large.txt"
echo ""

echo "=== Benchmark 3: Email-like pattern (single large file) ==="
echo "Pattern: word@word.word"
echo ""

echo "grep -E '[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z]+':"
time grep -oE '[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-zA-Z]+' "$TEST_DIR/large.txt" | wc -l
echo ""

echo "pgrep email pattern:"
time "$PGREP" -c 'some alphaNum <+> char '\''@'\'' <+> some alphaNum <+> char '\''.'\'' <+> some letter' "$TEST_DIR/large.txt"
echo ""

echo "=== Benchmark 4: Multiple files (100 files) ==="
echo "Pattern: 'import'"
echo ""

echo "grep -r 'import':"
time grep -r "import" "$TEST_DIR"/*.txt | wc -l
echo ""

echo "pgrep 'string \"import\"':"
time "$PGREP" 'string "import"' "$TEST_DIR"/*.txt | wc -l
echo ""

echo "=== Benchmark 5: Non-greedy match (pgrep only) ==="
echo "Pattern: 'Line' ... 'email' (manyTill)"
echo ""

echo "pgrep manyTill:"
time "$PGREP" -c 'string "Line" <+> manyTill anyChar (string "email")' "$TEST_DIR/large.txt"
echo ""
echo "(grep cannot easily do non-greedy across arbitrary content)"

echo ""
echo "=== Cleanup ==="
rm -rf "$TEST_DIR"
echo "Done!"
