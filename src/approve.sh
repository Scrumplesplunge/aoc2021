#!/bin/bash

day="${1?}"
from="$(printf 'build/day%02d.%s.output' "$day" "$USER")"
to="$(printf 'puzzles/day%02d/%s.output' "$day" "$USER")"
if ! [[ -f "$from" ]]; then
  >&2 echo "No such file: $from"
  exit 1
fi
cp "$from" "$to"
