# Sieve of Eratosthenes

Various programs that count the number of primes less than 
a given number `n` using the sieve of Eratosthenes. Build by
invoking `make` in this directory.

All the programs are to be invoked as 

    <progname> <n>

## Haskell

* SieveVec: Uses immutable vectors. 
* SieveFor: Mutable vectors and `forM_`.
* SieveST: Mutable vectors and loops written as explicitly 
           named functions.

## Python

`numba`'s JIT is essential for performance comparable to the C++ and
Haskell ST versions.

## C++

sieve\_cc: A further speedup is possible if you replace `vector<char>`
          with `vector<bool>` which uses a bitvector representation.

