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
    -rwxr-xr-x 1 joe joe 8547 Jan  5 17:39 build/day23
    -rwxr-xr-x 1 joe joe 4071 Jan  5 17:39 build/day24
    -rwxr-xr-x 1 joe joe 3635 Jan  5 17:39 build/day25

## Performance

    $ src/time.sh
    day01: Ran 2524 times, average 198us
    day02: Ran 3179 times, average 157us
    day03: Ran 1247 times, average 401us
    day04: Ran 384 times, average 1305us
    day05: Ran 244 times, average 2050us
    day06: Ran 6495 times, average 76us
    day07: Ran 985 times, average 508us
    day08: Ran 1300 times, average 384us
    day09: Ran 559 times, average 894us
    day10: Ran 2114 times, average 236us
    day11: Ran 417 times, average 1200us
    day12: Ran 1771 times, average 282us
    day13: Ran 715 times, average 700us
    day14: Ran 1382 times, average 361us
    day15: Ran 25 times, average 20717us
    day16: Ran 4354 times, average 114us
    day17: Ran 428 times, average 1169us
    day18: Ran 5 times, average 114084us
    day19: Ran 2 times, average 339949us
    day20: Ran 6 times, average 95890us
    day21: Ran 144 times, average 3491us
    day22: Ran 60 times, average 8344us
    day23: Ran 1 times, average 888688us
    day24: Ran 8 times, average 65509us
    day25: Ran 3 times, average 173591us
