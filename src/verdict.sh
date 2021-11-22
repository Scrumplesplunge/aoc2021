#!/bin/bash

expected_output="${1?}"
actual_output="${2?}"

solver="$(dirname "${expected_output/puzzles\//}")"
input="$(basename "${expected_output}" .output)"
printf "$(basename "$solver")($(basename -s .input "$input")).. "
diff="$(colordiff "$expected_output" "$actual_output")"
if [[ $? -eq 0 ]]; then
  printf " \x1b[32mPASSED\x1b[0m\n"
else
  printf " \x1b[31mFAILED\x1b[0m: (\x1b[31mwant\x1b[0m, \x1b[32mgot\x1b[0m)\n"
  echo "$diff"
fi
