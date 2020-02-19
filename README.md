# QuickCheckVEngine

**QCVEngine** is a very basic QuickCheck-based random instruction generator
for RISC-V (currently supports most 32-bit and 64-bit RISC-V standard extentions as well as CHERI RISC-V extensions).
The main feature of the generator is a small library that makes it easy
to define instruction encoders and decoders.

Find out all of the extensions that are supported by looking at the file names in [this folder](https://github.com/CTSRD-CHERI/QuickCheckVEngine/tree/master/src/RISCV).

## Getting started

You should be able to build QCVEngine using cabal. Simply run
```sh
$ cabal configure
$ cabal build
$ cabal install
```
and the `QCVEngine` binary should be in your `~/.cabal/bin`.
