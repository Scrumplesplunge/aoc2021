#define _GNU_SOURCE
#include <fcntl.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static void die(const char *message) {
  perror(message);
  exit(1);
}

struct run_args {
  struct timespec *start;
  char *const solver;
  char *const puzzle;
};

static int run_impl(void *a) {
  struct run_args *args = a;
  // Open the puzzle file as STDIN.
  const int puzzle = open(args->puzzle, O_RDONLY);
  if (puzzle == -1) die("open");
  const int new_stdin = dup2(puzzle, STDIN_FILENO);
  if (new_stdin == -1) die("dup2");
  // Start the timer and execute the solver.
  char *argv[] = {args->solver, NULL};
  clock_gettime(CLOCK_MONOTONIC, args->start);
  execv(argv[0], argv);
  die("execv");
}
static char run_stack[4096];

static int run(char *solver, char *puzzle) {
  struct timespec start, end;
  struct run_args args = {
      .start = &start,  // The start time will be recorded by run_impl.
      .solver = solver,
      .puzzle = puzzle,
  };
  const int pid =
      clone(run_impl,
            run_stack + sizeof(run_stack), // Stack assumed to grow downwards.
            CLONE_VM | SIGCHLD, // Use the same memory map, signal on exit.
            &args);
  if (pid == -1) die("clone");
  int status;
  if (waitpid(pid, &status, 0) == -1) die("waitpid");
  clock_gettime(CLOCK_MONOTONIC, &end);
  if (status != 0) {
    fprintf(stderr, "solver exited with code %d\n", status);
    exit(1);
  }
  const int sec = end.tv_sec - start.tv_sec;
  const int usec = (end.tv_nsec - start.tv_nsec) / 1000;
  return 1000000 * sec + usec;
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    fprintf(stderr, "Usage: microtime <solver> <puzzle>\n");
    return 1;
  }
  char *const solver = argv[1];
  char *const puzzle = argv[2];
  // Run for 500ms.
  int runs = 0;
  int total_usec = 0;
  while (total_usec < 500000) {
    total_usec += run(solver, puzzle);
    runs++;
  }
  fprintf(stderr, "Ran %d times, average %dus\n", runs, total_usec / runs);
}