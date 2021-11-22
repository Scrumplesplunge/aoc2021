#!/bin/bash

# Build the compiler.
if ! (cd lang && cmake -B ../build && make -C ../build); then
  >&2 echo 'Failed to build the compiler'
  exit 1
fi

# Run unit tests.
for unit_test in build/*_test; do
  echo "$unit_test"
  "$unit_test"
done

# Run E2E tests.
r='\x1b[31m'
g='\x1b[32m'
y='\x1b[33m'
reset='\x1b[0m'
for e2e_test_file in lang/test_data/*.aoc; do
  e2e_test="$(basename -s .aoc "$e2e_test_file")"
  if ! build/compiler "lang/test_data/$e2e_test.aoc" > "build/$e2e_test.s"; then
    echo -e "${r}FAILED${reset} (compilation) $e2e_test"
    continue
  fi
  if ! as "build/$e2e_test.s" -o "build/$e2e_test.o"; then
    echo -e "${r}FAILED${reset} (assembly) $e2e_test"
    continue
  fi
  if ! ld -T lang/link.ld "build/$e2e_test.o" -o "build/$e2e_test"; then
    echo -e "${r}FAILED${reset} (linking) $e2e_test"
    continue
  fi
  llvm-strip --strip-all "build/$e2e_test"
  "build/$e2e_test" <"lang/test_data/$e2e_test.input"  \
                    >"build/$e2e_test.stdout"  \
                    2>"build/$e2e_test.stderr"
  exit_code="$?"
  cat > "build/$e2e_test.output" <<EOF
stdout:
$(cat "build/$e2e_test.stdout")
stderr:
$(cat "build/$e2e_test.stderr")
exit code: $exit_code
EOF
  if ! colordiff --color=always  \
                 "lang/test_data/$e2e_test.output"  \
                 "build/$e2e_test.output" >"build/$e2e_test.diff"; then
    echo -e "${r}FAILED${reset} (diff) $e2e_test"  \
            "(${r}want${reset}, ${g}got${reset})"
    cat "build/$e2e_test.diff"
    continue
  fi
  echo -e "${g}PASSED${reset} $e2e_test"
done
