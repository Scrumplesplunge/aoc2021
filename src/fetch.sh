#!/bin/bash

day="${1?}"
if ! [[ -f .cookie ]]; then
  >&2 echo 'no cookie'
exit 1
fi
cookie="Cookie: $(cat .cookie)"
directory="$(printf 'puzzles/day%02d' "$day")"
file="$directory/$USER.input"
mkdir -p "$directory"
curl -H "$cookie" "https://adventofcode.com/2021/day/$day/input" > "$file"
touch "$directory/$USER.output"
