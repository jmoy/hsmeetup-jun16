# Image manipulation

Haskell and Python programs that rotate a image and give it a 
warmer hue. 

Build the Haskell program by using `cabal build`. After the build
the executable will be in `dist/build/examples-repa/examples-repa`.
The cabal file is written to invoke the LLVM backend of GHC. If
you don't have LLVM installed, remove `-fllvm` from the `ghc-options` 
setting in the cabal file.

The programs are to be invoked as

  <progname> <deg> <red> <infile> <outfile>

where `deg` is the degrees of rotation, `red` is the percentage
by which the red channel is to be emphasized, `infile` and `outfile`
are the input and output image files. JPG files work, others may 
work too.

The Haskell program is parallelized but runs single-threaded by default.
Add `+RTS -N` to run on multiple threads with the number of threads 
chosed by the  runtime. Add `+RTS -Nk` to run on `k` threads
(eg. `+RTS -N2` for two threads).
