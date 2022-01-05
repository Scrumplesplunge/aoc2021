# Advent of Code 2021

This repository contains my solutions to Advent of Code 2021. For 2020, I solved
them all in [plain C with no libraries, not even libc][1]. This time, I
implemented my own C-like language and solved them all in that. The `/lang`
subdirectory contains the source code for a compiler, written in C++, and the
Advent of Code puzzles are solved in that language. The compiler outputs x86-64
assembly code, and the compiled binaries are dependency-free, small, and fast.

[1]: https://github.com/Scrumplesplunge/aoc2020
