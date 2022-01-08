# Advent of Code 2021

This repository contains my solutions to Advent of Code 2021. For 2020, I solved
them all in [plain C with no libraries, not even libc][1]. This time, I
implemented my own C-like language and solved them all in that. The `/lang`
subdirectory contains the source code for a compiler, written in C++, and the
Advent of Code puzzles are solved in that language. The compiler outputs x86-64
assembly code, and the compiled binaries are dependency-free, small, and fast.

[1]: https://github.com/Scrumplesplunge/aoc2020

## Binary Size

Each of the solvers compiles to a native linux executable with zero
dependencies. With a custom linker script for good measure, this results in very
small binaries:

    $ ls -l build/day{01..25}
    -rwxr-xr-x 1 joe joe 2499 Jan  5 17:39 build/day01
    -rwxr-xr-x 1 joe joe 3239 Jan  5 17:39 build/day02
    -rwxr-xr-x 1 joe joe 3531 Jan  5 17:39 build/day03
    -rwxr-xr-x 1 joe joe 4298 Jan  5 17:39 build/day04
    -rwxr-xr-x 1 joe joe 4418 Jan  5 17:39 build/day05
    -rwxr-xr-x 1 joe joe 2945 Jan  5 17:39 build/day06
    -rwxr-xr-x 1 joe joe 4210 Jan  5 17:39 build/day07
    -rwxr-xr-x 1 joe joe 5103 Jan  5 17:39 build/day08
    -rwxr-xr-x 1 joe joe 4569 Jan  5 17:39 build/day09
    -rwxr-xr-x 1 joe joe 4234 Jan  5 17:39 build/day10
    -rwxr-xr-x 1 joe joe 3527 Jan  5 17:39 build/day11
    -rwxr-xr-x 1 joe joe 3711 Jan  5 17:39 build/day12
    -rwxr-xr-x 1 joe joe 4567 Jan  5 17:39 build/day13
    -rwxr-xr-x 1 joe joe 5164 Jan  5 17:39 build/day14
    -rwxr-xr-x 1 joe joe 4084 Jan  5 17:39 build/day15
    -rwxr-xr-x 1 joe joe 5554 Jan  5 17:39 build/day16
    -rwxr-xr-x 1 joe joe 3615 Jan  5 17:39 build/day17
    -rwxr-xr-x 1 joe joe 6708 Jan  5 17:39 build/day18
    -rwxr-xr-x 1 joe joe 6676 Jan  5 17:39 build/day19
    -rwxr-xr-x 1 joe joe 3920 Jan  5 17:39 build/day20
    -rwxr-xr-x 1 joe joe 4402 Jan  5 17:39 build/day21
    -rwxr-xr-x 1 joe joe 5673 Jan  5 17:39 build/day22
    -rwxr-xr-x 1 joe joe 8755 Jan  5 18:29 build/day23
    -rwxr-xr-x 1 joe joe 4071 Jan  5 17:39 build/day24
    -rwxr-xr-x 1 joe joe 3635 Jan  5 17:39 build/day25

## Performance

    $ src/time.sh
    Warming up...
    day01: Ran 3709 times, average 134us
    day02: Ran 4592 times, average 108us
    day03: Ran 1637 times, average 305us
    day04: Ran 629 times, average 795us
    day05: Ran 331 times, average 1512us
    day06: Ran 10190 times, average 49us
    day07: Ran 1470 times, average 340us
    day08: Ran 2175 times, average 229us
    day09: Ran 880 times, average 568us
    day10: Ran 3409 times, average 146us
    day11: Ran 631 times, average 793us
    day12: Ran 2436 times, average 205us
    day13: Ran 912 times, average 548us
    day14: Ran 2022 times, average 247us
    day15: Ran 36 times, average 14187us
    day16: Ran 6388 times, average 78us
    day17: Ran 682 times, average 734us
    day18: Ran 10 times, average 54258us
    day19: Ran 3 times, average 184167us
    day20: Ran 8 times, average 66040us
    day21: Ran 216 times, average 2316us
    day22: Ran 90 times, average 5582us
    day23: Ran 3 times, average 241454us
    day24: Ran 14 times, average 38244us
    day25: Ran 5 times, average 121567us
