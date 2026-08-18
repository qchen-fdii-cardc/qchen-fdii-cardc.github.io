#!/usr/bin/env bash
set -u

root="${1:-.}"

if ! command -v convert >/dev/null 2>&1; then
  echo "Error: 'convert' is not installed or not in PATH." >&2
  echo "Install ImageMagick first: sudo apt install imagemagick" >&2
  exit 1
fi

if [[ ! -d "$root" ]]; then
  echo "Error: directory not found: $root" >&2
  exit 1
fi

count=0
while IFS= read -r -d '' file; do
  out="${file%.ppm}.png"
#   skipping the check for existing files to allow overwriting
#   if [[ -f "$out" ]]; then
#     echo "Skipping existing: $out"
#     continue
#   fi

  echo "Converting: $file -> $out"
  convert "$file" "$out"
  count=$((count + 1))
done < <(find "$root" -type f -iname '*.ppm' -print0)

echo "Done. Converted $count PPM file(s) to PNG."
