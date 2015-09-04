# Tiny Compiler

## Usage

In order to use the Tiny compiler, simply install its dependencies and build it with (Cabal)[https://www.haskell.org/cabal/].
You can then invoke `t2s` from the `dist/build/t2s` directory to obtain the x86 assembly code for a Tiny input file on the standard output stream:

```
dist/build/t2s/t2s hello_world.tiny
```
