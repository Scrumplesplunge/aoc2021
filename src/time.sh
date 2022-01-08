#!/bin/bash

# Start some background work to force the CPU to clock up to full speed.
dd if=/dev/zero of=/dev/null &
pid="$!"
function cleanup {
  kill "$pid"
}
trap cleanup EXIT

echo 'Warming up...'
sleep 0.5s

# Time the puzzles.
gcc src/microtime.c -o build/microtime
for day in day{01..25}; do
  printf "%s: " "$day"
  build/microtime "build/$day" "puzzles/$day/joe.input" >/dev/null
done
