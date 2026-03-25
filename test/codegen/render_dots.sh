#!/usr/bin/env bash
# Extract DOT graphs from expect test files and render them as SVG images.
#
# Usage: ./render_dots.sh [output_dir]
#   output_dir defaults to ./dot_images

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUT_DIR="${1:-$SCRIPT_DIR/dot_images}"

mkdir -p "$OUT_DIR"

count=0

for ml_file in "$SCRIPT_DIR"/test_gen.ml "$SCRIPT_DIR"/test_realistic.ml; do
  [ -f "$ml_file" ] || continue
  base="$(basename "$ml_file" .ml)"

  # State machine: scan for test names and DOT blocks
  test_name=""
  in_dot=false
  dot_buf=""

  while IFS= read -r line; do
    # Detect test name from let%expect_test "..."
    if [[ "$line" =~ let%expect_test\ \"(.+)\" ]]; then
      test_name="${BASH_REMATCH[1]}"
      continue
    fi

    # Detect start of DOT block
    if [[ "$in_dot" == false && "$line" =~ ^[[:space:]]*DOT: ]]; then
      in_dot=true
      dot_buf=""
      continue
    fi

    # Detect end of DOT block (CODE: line or closing |})
    if [[ "$in_dot" == true ]]; then
      if [[ "$line" =~ ^[[:space:]]*CODE: ]] || [[ "$line" =~ ^\|?\} ]]; then
        in_dot=false
        if [ -n "$dot_buf" ] && [ -n "$test_name" ]; then
          # Sanitize test name for filename
          safe_name="$(echo "$test_name" | sed 's/[^A-Za-z0-9_-]/_/g')"
          out_file="$OUT_DIR/${base}__${safe_name}.svg"
          echo "$dot_buf" | dot -Tsvg -o "$out_file" 2>/dev/null && {
            echo "  $out_file"
            count=$((count + 1))
          } || echo "  WARN: failed to render $safe_name" >&2
        fi
        dot_buf=""
        continue
      fi
      dot_buf="$dot_buf
$line"
    fi
  done < "$ml_file"
done

echo "Rendered $count images in $OUT_DIR"
