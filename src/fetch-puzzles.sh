#!/bin/bash

users=(
  agata-borkowska-clark:aoc2021:.
  JoBoCl:aoc2021:.
  Scrumplesplunge:aoc2021:.
)

function fetch {
  local user="${1?}"
  local repository="${2?}"
  if [[ -d "/tmp/$user-aoc2021" ]]; then
    # Try `git pull` instead, since it may be faster.
    (cd "/tmp/$user-aoc2021"; git pull) && return 0;
    # If that fails, start from scratch.
    rm -rf "/tmp/$user-aoc2021"
  fi
  git clone "git@github.com:$user/$repository.git" "/tmp/$user-aoc2021"
}

function copy {
  local user="${1?}"
  local directory="${2?}"
  rsync --filter='. src/puzzles.filter'  \
        -rv "/tmp/$user-aoc2021/$directory/puzzles/" "puzzles/"
}

for info in "${users[@]}"; do
  IFS=: read user repository directory <<< "$info"
  echo "git@github.com:$user/$repository/$directory/puzzles"
  fetch "$user" "$repository" && copy "$user" "$directory"
done

# Remove trailing whitespace and ensure that the file ends with a newline.
sed -i 's/\s*$//;a\' puzzles/*/*
