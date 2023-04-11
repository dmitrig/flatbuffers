# Flatbuffers

Runtime library for flatbuffers in ocaml.

## Building
Either compile the flatbuffers project first, so the `flatc` binary is available in
the parent directory, or run dune with `--ignore-promoted-rules`.

    opam install . --deps-only -t
    dune test

Includes an ocaml version of the cpp encode/decode benchmark (see `fb_bench.cpp`).

    dune exec --profile=release --display=quiet test/bench/fb_bench.exe

Read performance appears to be close to Java (openjdk 11) using the baseline
compiler and close to 2x faster with flambda.

## Usage
Run flatc with the `--ocaml` flag.

    ./flatc --ocaml ./samples/monster.fbs

This will output `monster.ml` and `monster.mli` in the working directory. No
additional flags are currently supported.

## TODO
* support nested flatbuffer fields
* lookup by key in sorted vectors
* generate object API
* generate verification API
* 32-bit / big-endian arch support?
