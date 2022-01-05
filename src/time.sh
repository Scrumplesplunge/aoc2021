#!/bin/bash

gcc src/microtime.c -o build/microtime
for day in day{01..25}; do
  printf "%s: " "$day"
  build/microtime "build/$day" "puzzles/$day/joe.input" >/dev/null
done
