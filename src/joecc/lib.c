#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

extern int get_current_micros(void) asm ("min_caml_get_current_micros");
extern int modulo(int, int);
extern int divide(int, int);
extern int rand_int(int);

int get_current_micros() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}

int modulo(int lhs, int rhs) { return lhs % rhs; }

int divide(int lhs, int rhs) { return lhs / rhs; }

int rand_int(int n) {
  srandom(get_current_micros());
  return random() % n + 1;
}
