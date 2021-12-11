.PHONY: all clean check

SOURCES = $(wildcard src/day[0-2][0-9].aoc)
SOLVERS = ${SOURCES:src/%.aoc=build/%}
PUZZLES = $(shell find puzzles -name '*.output')
OUTPUT_BASENAMES = $(subst /,.,${PUZZLES:puzzles/%=%})
OUTPUTS = ${OUTPUT_BASENAMES:%=build/%}
.PRECIOUS: ${OUTPUTS}
.PRECIOUS: ${SOURCES:src/%.aoc=build/%.s}
.PRECIOUS: ${SOURCES:src/%.aoc=build/%.o}

all: ${SOLVERS}

check: build/tests
	@cat build/tests

build/compiler: | build
	(cd lang && cmake -B ../build && ${MAKE} -C ../build)

build:
	mkdir build

build/%.s: src/%.aoc build/compiler
	build/compiler $< >$@.tmp && mv $@.tmp $@

build/%.o: build/%.s
	as -Os $^ -o $@

build/%: build/%.o | lang/link.ld
	ld $^ -T lang/link.ld -o $@
	llvm-strip --strip-sections $@

build/day00.%.output: build/day00 puzzles/day00/%.input
	build/day00 <puzzles/day00/$*.input >$@
build/day00.%.verdict: puzzles/day00/%.output build/day00.%.output
	src/verdict.sh $^ >$@

build/day01.%.output: build/day01 puzzles/day01/%.input
	build/day01 <puzzles/day01/$*.input >$@
build/day01.%.verdict: puzzles/day01/%.output build/day01.%.output
	src/verdict.sh $^ >$@

build/day02.%.output: build/day02 puzzles/day02/%.input
	build/day02 <puzzles/day02/$*.input >$@
build/day02.%.verdict: puzzles/day02/%.output build/day02.%.output
	src/verdict.sh $^ >$@

build/day03.%.output: build/day03 puzzles/day03/%.input
	build/day03 <puzzles/day03/$*.input >$@
build/day03.%.verdict: puzzles/day03/%.output build/day03.%.output
	src/verdict.sh $^ >$@

build/day04.%.output: build/day04 puzzles/day04/%.input
	build/day04 <puzzles/day04/$*.input >$@
build/day04.%.verdict: puzzles/day04/%.output build/day04.%.output
	src/verdict.sh $^ >$@

build/day05.%.output: build/day05 puzzles/day05/%.input
	build/day05 <puzzles/day05/$*.input >$@
build/day05.%.verdict: puzzles/day05/%.output build/day05.%.output
	src/verdict.sh $^ >$@

build/day06.%.output: build/day06 puzzles/day06/%.input
	build/day06 <puzzles/day06/$*.input >$@
build/day06.%.verdict: puzzles/day06/%.output build/day06.%.output
	src/verdict.sh $^ >$@

build/day07.%.output: build/day07 puzzles/day07/%.input
	build/day07 <puzzles/day07/$*.input >$@
build/day07.%.verdict: puzzles/day07/%.output build/day07.%.output
	src/verdict.sh $^ >$@

build/day08.%.output: build/day08 puzzles/day08/%.input
	build/day08 <puzzles/day08/$*.input >$@
build/day08.%.verdict: puzzles/day08/%.output build/day08.%.output
	src/verdict.sh $^ >$@

build/day09.%.output: build/day09 puzzles/day09/%.input
	build/day09 <puzzles/day09/$*.input >$@
build/day09.%.verdict: puzzles/day09/%.output build/day09.%.output
	src/verdict.sh $^ >$@

build/day10.%.output: build/day10 puzzles/day10/%.input
	build/day10 <puzzles/day10/$*.input >$@
build/day10.%.verdict: puzzles/day10/%.output build/day10.%.output
	src/verdict.sh $^ >$@

build/day11.%.output: build/day11 puzzles/day11/%.input
	build/day11 <puzzles/day11/$*.input >$@
build/day11.%.verdict: puzzles/day11/%.output build/day11.%.output
	src/verdict.sh $^ >$@

build/day12.%.output: build/day12 puzzles/day12/%.input
	build/day12 <puzzles/day12/$*.input >$@
build/day12.%.verdict: puzzles/day12/%.output build/day12.%.output
	src/verdict.sh $^ >$@

build/day13.%.output: build/day13 puzzles/day13/%.input
	build/day13 <puzzles/day13/$*.input >$@
build/day13.%.verdict: puzzles/day13/%.output build/day13.%.output
	src/verdict.sh $^ >$@

build/day14.%.output: build/day14 puzzles/day14/%.input
	build/day14 <puzzles/day14/$*.input >$@
build/day14.%.verdict: puzzles/day14/%.output build/day14.%.output
	src/verdict.sh $^ >$@

build/day15.%.output: build/day15 puzzles/day15/%.input
	build/day15 <puzzles/day15/$*.input >$@
build/day15.%.verdict: puzzles/day15/%.output build/day15.%.output
	src/verdict.sh $^ >$@

build/day16.%.output: build/day16 puzzles/day16/%.input
	build/day16 <puzzles/day16/$*.input >$@
build/day16.%.verdict: puzzles/day16/%.output build/day16.%.output
	src/verdict.sh $^ >$@

build/day17.%.output: build/day17 puzzles/day17/%.input
	build/day17 <puzzles/day17/$*.input >$@
build/day17.%.verdict: puzzles/day17/%.output build/day17.%.output
	src/verdict.sh $^ >$@

build/day18.%.output: build/day18 puzzles/day18/%.input
	build/day18 <puzzles/day18/$*.input >$@
build/day18.%.verdict: puzzles/day18/%.output build/day18.%.output
	src/verdict.sh $^ >$@

build/day19.%.output: build/day19 puzzles/day19/%.input
	build/day19 <puzzles/day19/$*.input >$@
build/day19.%.verdict: puzzles/day19/%.output build/day19.%.output
	src/verdict.sh $^ >$@

build/day20.%.output: build/day20 puzzles/day20/%.input
	build/day20 <puzzles/day20/$*.input >$@
build/day20.%.verdict: puzzles/day20/%.output build/day20.%.output
	src/verdict.sh $^ >$@

build/day21.%.output: build/day21 puzzles/day21/%.input
	build/day21 <puzzles/day21/$*.input >$@
build/day21.%.verdict: puzzles/day21/%.output build/day21.%.output
	src/verdict.sh $^ >$@

build/day22.%.output: build/day22 puzzles/day22/%.input
	build/day22 <puzzles/day22/$*.input >$@
build/day22.%.verdict: puzzles/day22/%.output build/day22.%.output
	src/verdict.sh $^ >$@

build/day23.%.output: build/day23 puzzles/day23/%.input
	build/day23 <puzzles/day23/$*.input >$@
build/day23.%.verdict: puzzles/day23/%.output build/day23.%.output
	src/verdict.sh $^ >$@

build/day24.%.output: build/day24 puzzles/day24/%.input
	build/day24 <puzzles/day24/$*.input >$@
build/day24.%.verdict: puzzles/day24/%.output build/day24.%.output
	src/verdict.sh $^ >$@

build/day25.%.output: build/day25 puzzles/day25/%.input
	build/day25 <puzzles/day25/$*.input >$@
build/day25.%.verdict: puzzles/day25/%.output build/day25.%.output
	src/verdict.sh $^ >$@

build/tests: ${OUTPUTS:%.output=%.verdict}
	cat $(sort $^) >$@

clean:
	rm -r build
