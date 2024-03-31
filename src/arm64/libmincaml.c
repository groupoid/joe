#include <stdio.h>
#include <math.h>

// NOTE: コンパイル後のシンボルのプリフィックスに _ がつくのを避ける
void min_caml_print_int(long n) asm("min_caml_print_int");
void min_caml_print_newline() asm("min_caml_print_newline");
long* min_caml_create_array(long number_of_element, long init_value) asm("min_caml_create_array");
double* min_caml_create_float_array(long number_of_element, double float_value) asm("min_caml_create_float_array");
long min_caml_truncate() asm("min_caml_truncate");
void min_caml_print_float(double d) asm("min_caml_print_float");
void min_caml_print_byte(long n) asm("min_caml_print_byte");
long min_caml_read_int() asm("min_caml_read_int");
double min_caml_read_float() asm("min_caml_read_float");
double min_caml_atan(double x) asm("min_caml_atan");
double min_caml_cos(double x) asm("min_caml_cos");
double min_caml_floor(double x) asm("min_caml_floor");
double min_caml_sin(double x) asm("min_caml_sin");
double min_caml_abs_float(double x) asm("min_caml_abs_float");
double min_caml_float_of_int(long n) asm("min_caml_float_of_int");
long min_caml_int_of_float(double d) asm("min_caml_int_of_float");
double min_caml_sqrt(double d) asm("min_caml_sqrt");

void min_caml_print_int(long n) {
  printf("%ld", n);
}

void min_caml_print_newline() {
  printf("\n");
}

long* min_caml_create_array(long number_of_element, long init_value) {
  long *heap_ptr;

  // x27 に格納されたヒープのアドレスを heap_ptr へ書き出す
  asm volatile ("mov %0, x27" : "=r"(heap_ptr));

  // Array の先頭アドレスを取得
  long *array_ptr = heap_ptr;

  for (long i = 0l; i < number_of_element; i++) {
    // Array へ書き込んだ後、ヒープの先頭アドレスを8バイト進める
    *heap_ptr = init_value;
    heap_ptr += 1;
  }

  // ヒープの先頭アドレスを x27 に書き戻す
  asm volatile ("mov x27, %0" : : "r"(heap_ptr));

  return array_ptr;
}

double* min_caml_create_float_array(long number_of_element, double float_value) {
  double *heap_ptr;

  // x27 に格納されたヒープのアドレスを heap_ptr へ書き出す
  asm volatile ("mov %0, x27" : "=r"(heap_ptr));

  // Array の先頭アドレスを取得
  double *array_ptr = heap_ptr;

  for (long i = 0l; i < number_of_element; i++) {
    // Array へ書き込んだ後、ヒープの先頭アドレスを8バイト進める
    *heap_ptr = float_value;
    heap_ptr += 1;
  }

  // ヒープの先頭アドレスを x27 に書き戻す
  asm volatile ("mov x27, %0" : : "r"(heap_ptr));

  return array_ptr;
}

// truncate
long min_caml_truncate(double d) {
  return (long)d;
}

void min_caml_print_float(double d) {
  printf("%lf", d);
}

void min_caml_print_byte(long n) {
  putchar(n);
}

long min_caml_read_int() {
  long l;
  // fscanf(fp, "%ld", &l);
  scanf("%ld", &l);
  return l;
}

double min_caml_read_float() {
    double d;
    // fscanf(fp, "%lf", &d);
    scanf("%lf", &d);
    return d;
}

// atan
double min_caml_atan(double x) {
  return atan(x);
}

// cos
double min_caml_cos(double x) {
  return cos(x);
}

// floor
double min_caml_floor(double x) {
  return floor(x);
}

// sin
double min_caml_sin(double x) {
  return sin(x);
}

// abs_float
double min_caml_abs_float(double x) {
  return fabs(x);
}

// float_of_int
double min_caml_float_of_int(long n) {
  return (double)n;
}

// int_of_float
long min_caml_int_of_float(double d) {
  return (long)d;
}

// sqrt
double min_caml_sqrt(double d) {
  return sqrt(d);
}
