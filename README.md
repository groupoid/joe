Joe: MinCaml compiler and virtual machine
=========================================

Features
--------

* 4K LOC
* Intel/ARM 64-bit native compiler (joe)
* Byte-code IR compiler (joe)
* Byte-code IR virtual machine (vm)
* Original MinCaml codebase

Setup
-----

```
$ opam switch set 4.14.2
$ opam install ppx_deriving ppx_inline_test
$ dune build
```

Samples
-------

Compile sample for MinCaml bytecode:

```sh
$ _build/install/default/bin/vm -compile examples/fact.ml
```

Run sample from MinCaml bytecode in VM interpreter:

```sh
$ _build/install/default/bin/vm -exec examples/fact.joe
10
3628800
```

Compile sample for Apple M1 (from MinCaml to Assembler):

```sh
$ _build/install/default/bin/joe -arm examples/ack.ml
Generating assembly...OK
```

Compile assembler and link for macOS:

```sh
$ gcc examples/ack.arm.s src/arm64/libmincaml.c src/arm64/stub.c -o ack
```

Run sample natively on M1:

```sh
$ ./ack
509
```

Resources
---------

* <a href="https://github.com/hz7k-nzw/sicp-in-smlnj">Kenji Nozawa. SICP in Standard ML</a>.
* <a href="https://esumii.github.io/min-caml/index-e.html">Eijiro Sumii. A Crash Course for the MinCaml compiler</a>.
* <a href="https://esumii.github.io/min-caml/paper.pdf">Eijiro Sumii. MinCaml: A Simple and Efficient Compiler for a Minimal Functional Language</a>.
* <a href="https://tonpa.guru/stream/2024/2024-03-30%20Інформатика.htm">М. Сохацький. Інформатика</a>.
* <a href="https://www.cs.cmu.edu/~rwh/students/okasaki.pdf">C. Okasaki. Purely Functional Data Structures</a>.
* <a href="https://www.cs.cmu.edu/~rwh/isml/book.pdf">R. Harper. Programming in Standard ML</a>.
* Andrew W. Appel. Modern Compiler Implementation in ML.
* Andreas Rossberg. HaMLet: To Be Or Not To Be Standard ML.

Credits
-------

* Namdak Tonpa
