# Haskell

https://www.haskell.org/

Haskell repositories: https://github.com/haskell

GHC = Glasgow Haskell Compiler: https://gitlab.haskell.org/ghc

GHC User’s Guide: https://downloads.haskell.org/ghc/latest/docs/users_guide/

<br/>

After [Roc](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Roc#roc),
this is my second implementation of the microbenchmark program in a _pure_ functional programming language:
[Two branches of Functional Programming (FP): pure and impure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#two-branches-of-functional-programming-fp-pure-and-impure)

---

Table of contents:

- [Installation tips](#installation-tips)
- [Checking a Haskell package on the fly](#checking-a-haskell-package-on-the-fly)
- [On how to make a faster microbenchmark program](#on-how-to-make-a-faster-microbenchmark-program)
- [Using the LLVM backend](#using-the-llvm-backend)

<br/>

---

## Installation tips

I started with a "proper" installation with GHCup: https://www.haskell.org/ghcup/

This is very helpful with the (global) management of Haskell libraries (with the _cabal_ project builder and library manager); I use some libraries in my microbenchmark program.

At first, I installed some potentially missing pre-requisites:

```
$ sudo apt-get install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
...
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
...  # press [ENTER] to continue
...  # press [K] to keep current config
...  # press [P] to add the required PATH variable to "~/.bashrc"
...  # press [N] to not install a haskell-language-server (HLS)
...  # press [Y] to enable better integration of stack with GHCup
...  # this takes time!
$
```

Do not forget to update your _~/.bashrc_ configuration file! I moved the newly added line:

```
[ -f "~/.ghcup/env" ] && . "~/.ghcup/env" # ghcup-env
```

..from the very file end more up because some configurations only work correctly when they are located at the very end of this configuration file!

Then I shortly checked the GHC version:

```
$ source ~/.bashrc
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.3
$ 
```

We need to install a few libraries to render the Haskell implementation close to the other implementations:

```
$ cabal install --lib vector
...  # this takes time! A warning is shown at the end of the installation process:
...  # "The presence of such an environment file is likely to confuse or break other tools..."  
$ cabal install --lib random  # install the random library
...
$
```

<br/>

At Curry, you may also have a look at [Correctly installing Haskell](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Curry#correctly-installing-haskell) to manage different Haskell compiler (GHC) versions.

Switching to GHC version 9.10.3 can be done like this, if installed:

```
$ ghcup list  # first, list available and installed GHC resources
...
✓  ghc   9.10.3      recommended,base-4.20.1.0
...
$ ghcup set ghc 9.10.3
[ Info  ] downloading: https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.1.0.yaml as file ~/.ghcup/cache/ghcup-0.1.0.yaml
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
[ Info  ] ghc 9.10.3 successfully set as default version
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.3
$
```

<br/>

#### Checking a Haskell package on the fly

Since I didn't use some project tool here, it's a little bit more compilcated to check the version of a used package, here _random_ for example.

But it can be done with the _:show packages_ command after the _random_ package has been loaded on the fly:

```
$ cabal repl --build-depends random
Resolving dependencies...
Build profile: -w ghc-9.10.3 -O1
In order, the following will be built (use -v for more details):
 - fake-package-0 (interactive) (lib) (first run)
Configuring library for fake-package-0...
Warning: No exposed modules
GHCi, version 9.10.3: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/cabal-repl.-16109/setcwd.ghci
ghci> :show packages
active package flags:
  -package-id splitmix-0.1.3.2-af324892109d902f5d7d270f76edda34e5a6fdb22748cfa58fc86fd41e4771e8
  -package-id transformers-0.6.1.1-5a1c
  -package-id mtl-2.3.1-158c
  -package-id pretty-1.1.3.6-b981
  -package-id ghc-boot-th-9.10.3-889f
  -package-id template-haskell-2.22.0.0-1c23
  -package-id array-0.5.8.0-640b
  -package-id deepseq-1.5.0.0-c1f4
  -package-id bytestring-0.12.2.0-bd49
  -package-id rts-1.0.2
  -package-id ghc-prim-0.12.0-171e
  -package-id ghc-bignum-1.3-70f4
  -package-id ghc-internal-9.1003.0-7a4b
  -package-id random-1.3.1-a6ee7d97c12a69a20c457d9a7507369f986e294a2c0cf335687ef45619def468
  -package-id random-1.3.1-a6ee7d97c12a69a20c457d9a7507369f986e294a2c0cf335687ef45619def468
  -package-id base-4.20.2.0-f074
ghci> :quit
Leaving GHCi.
$
```

So, I'm using _random_ package in version 1.3.1, with slighlty older sources v1.3.0 available at: https://github.com/haskell/random as of 2026-10-20.

<br/>

## On how to make a faster microbenchmark program

At first, I experimented with data types (with the help of Google AI).  

I came to the conclusion that using Haskell's (imperative) **mutable vectors** makes a faster executable than doing traditional, functional list building (here with prepending an item ("front-appending") and then reversing the accumulated results), not dramatically, but by around -25% of program execution time according to my experiments.

<br/>

Then, I played with compiler switches. Here are the results when running time measurement command _$ multitime -n 10 ./random_streams_for_perf_stats<...>_:

compilation command | mean, real program execution time | comment
--- | --- | ---
ghc random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_devel | 106 milliseconds | my command for development
ghc -O2 random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim | 43 milliseconds | basic command for an optimal executable: my command for production
ghc -O2 -threaded -rtsopts -with-rtsopts="-N" -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim1 | 60 milliseconds | full set of optimzation switches (Google AI)
ghc -O2 -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim3 | 42 milliseconds | targeted testing of the LLVM backend related -fllvm switch
ghc -O2 -threaded random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim4 | 43 milliseconds |
ghc -O2 -threaded -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim2 | 43 milliseconds |

<br/>

I also experimented with the _Data.ByteString_ library is seen in the [A History of Haskell: Being Lazy With Class](https://dl.acm.org/doi/10.1145/1238844.1238856) paper from 2007,
where strings are represented "as byte vectors rather than lists of characters". But this solution cannot beat the implementation with Boxed Mutable Vectors (BMV) for string building
(and a Unboxed Mutable Vector, UMV, to store the generated random integer numbers) in terms of execution speed (while keeping the basic algorithm of the "masterloop").

<br/>

While working on a [Miranda transpilation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell/Miranda/random_streams_for_perf_stats.m), Google AI suggested to make user defined function _integer_to_hex_string_ more concise by turning this part: 

```
    toHex count k acc =
      let !remainder = k `mod` 16
          !nextK     = k `div` 16
          !char      = hexChar remainder
       in toHex (count - 1) nextK (char : acc)

    hexChar :: Int -> Char
    hexChar 0  = '0'
    hexChar 1  = '1'
    hexChar 2  = '2'
    hexChar 3  = '3'
    hexChar 4  = '4'
    hexChar 5  = '5'
    hexChar 6  = '6'
    hexChar 7  = '7'
    hexChar 8  = '8'
    hexChar 9  = '9'
    hexChar 10 = 'a'
    hexChar 11 = 'b'
    hexChar 12 = 'c'
    hexChar 13 = 'd'
    hexChar 14 = 'e'
    hexChar 15 = 'f'
    hexChar _  = '0'
```

into just this:

```
    toHex count k acc =
      let !remainder = k `mod` 16
          !nextK     = k `div` 16
          -- Look up the index directly from the string using !!:
          !char      = "0123456789abcdef" !! remainder
       in toHex (count - 1) nextK (char : acc)
```

However, while this solution is decreasing the number of lines of source code, it is also increasing the program execution time, here by around 5%.

So, I decided to stick to the verbose and fast solution, as originally started in my [Ada implementation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/f6208c9c8de1d0d0eb74f9b7f12cf01111e3a527/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb#L105).

By the way: doing the same in [Miranda](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell/Miranda/random_streams_for_perf_stats.m),
that is converting this elegant solution: _char = "0123456789abcdef" ! remainder_ into something like shown above in Haskell is not a good idea.
It increases the execution time of the Miranda script by more than 8%!

<br/>

#### Using the LLVM backend

One conclusion from above list is: using the
[LLVM backend](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/25%20-%20LLVM%20compiler%20infrastructure#llvm-compiler-infrastructure) (*) doesn't improve the execution speed of the
"speed part" of the microbenchmark program already optimized with switch _-O2_ in any statistically significant way: 

> It generally produces code with performance as good as the native code generator but for some cases can produce much faster code. This is especially true for numeric, array heavy code using packages like vector.

from: [5.10.2. LLVM Code Generator](https://downloads.haskell.org/ghc/latest/docs/users_guide/codegens.html#llvm-code-generator-fllvm)

Currently LLVM versions 13 to 20 are being supported (in GHC version 9.10.3), so, I first installed missing LLVM version 20 as shown at (*).

<br/>

Another conclusion from above list is that finding the best combination of compiler switches is obviously still human, manual testing work, also in "the age of AI coding".
Just activating all kind of potentially suitable compiler switches can actually make a slower executable as list entry #3 (for _random_streams_for_perf_stats_optim1_) shows! 

<br/>

With around 43 milliseconds of execution time (as of 2026-07-26), the Haskell executable is (significantly) slower than its
[OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/random_streams_for_perf_stats_main.ml),
[Bigloo Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo/random_streams_for_perf_stats.scm)
and [Common Lisp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/random_streams_for_perf_stats2.lisp) counterparts.

Interestingly, this execution time is about the same as the [Standard ML (MLton)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats3.sml) executable's execution time, a solution which is (also) using pre-allocated arrays, though those must be order-reversed before string concatenations (to do it correctly).

The Haskell executable is beating the other pure functional implementation for compilation (as of 2026-07-26) in [Roc](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Roc/random_streams_for_perf_stats.roc), a solution which isn't applying any "imperative tinkering" but doing (pure) functional list building.

<br/>

##_end
