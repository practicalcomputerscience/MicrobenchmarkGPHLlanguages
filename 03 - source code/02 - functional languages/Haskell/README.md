2026-07-26: work in progress tbd

- tbd: toc ?

<br/>

# Haskell

https://www.haskell.org/

GHC = Glasgow Haskell Compiler: https://gitlab.haskell.org/ghc

GHC User’s Guide: https://downloads.haskell.org/ghc/latest/docs/users_guide/

<br/>

After [Roc](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Roc#roc),
this is my second implementation of the microbenchmark program in a _pure_ functional programming language:
[Two branches of Functional Programming (FP): pure and impure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#two-branches-of-functional-programming-fp-pure-and-impure)

<br/>

## Installation tips

I started with a "proper" installation with GHCup: https://www.haskell.org/ghcup/

This is very helpful with the (global) management of Haskell libraries (with the _cabal_ project builder and library manager); some of them I'm using in the microbenchmark program.

At first, I installed some potentially missing pre-requisites:

```
$ sudo apt-get install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
...
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
...  # press [ENTER] to continue
...  # press [K] to keep current config
...  # press [P] to add the required PATH variable to "/home/booser/.bashrc"
...  # press [N] to not install a haskell-language-server (HLS)
...  # press [Y] to enable better integration of stack with GHCup
...  # this takes time!
$
```

Do not forget to update your _~/.bashrc_ configuration file! I moved the newly added line _[ -f "/home/booser/.ghcup/env" ] && . "/home/booser/.ghcup/env" # ghcup-env_
from the very file end more up because some configurations only work correctly when they are located at the very end of this configuration file!

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

## On how to make a faster microbenchmark program

At first, I experimented with data types (with the help of Google AI).  

I came to the conclusion that using Haskell's (imperative) **mutable vectors** makes a faster executable than doing traditional, functional list building (here with prepending an item ("front-appending") and then reversing the accumulated results), not dramatically, but by around -25% according to my experiments.

<br/>

Then, I played with compiler switches. Here are the results when running time measurement command _$ multitime -n 10 ./random_streams_for_perf_stats<...>_:

compilation command | mean, real program execution time | comment
--- | --- | ---
ghc random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_devel | 106 milliseconds | compilation command during program development
ghc -O2 random_streams_for_perf_stats.hs -o random_streams_for_perf_stats | 45 milliseconds | basic compilation command for an optimal executable
ghc -O2 -threaded -rtsopts -with-rtsopts="-N" -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim1 | 58 milliseconds | applying the full set of optimzation switches (Google AI)
ghc -O2 -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim3 | 45 millisconds | targeted testing of the LLVM backend related -fllvm switch
ghc -O2 -threaded random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim4 | 43 millisconds | -threaded alone has a slightly positive effect
ghc -O2 -threaded -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_optim2 | 43 millisconds | no improvement when adding -fllvm

<br/>

#### Using the LLVM backend

One conclusion from above list is: with the "speed part" of the microbenchmark program, using the
[LLVM backend](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/25%20-%20LLVM%20compiler%20infrastructure#llvm-compiler-infrastructure) doesn't improve the execution speed already optimized with switch _-O2_: 

> It generally produces code with performance as good as the native code generator but for some cases can produce much faster code. This is especially true for numeric, array heavy code using packages like vector.

from: [5.10.2. LLVM Code Generator](https://downloads.haskell.org/ghc/latest/docs/users_guide/codegens.html#llvm-code-generator-fllvm)

However, currently only LLVM versions 13 to 15 are being supported with compiler switch _-fllvm_ (in GHC version 9.10.3), so, I first installed LLVM version 15:

```
$ sudo apt-get install clang-15 llvm-15-dev libclang-common-15-dev libclang-15-dev
...
$ mkdir -p ~/.local/bin
$ ln -sf /usr/bin/opt-15 ~/.local/bin/opt  # make a soft like to the LLVM optimizer and analysis printer in version 15
$ ln -sf /usr/bin/llc-15 ~/.local/bin/llc  # make a soft like to the LLVM system compiler in version 15
$ ln -sf /usr/bin/clang-15 ~/.local/bin/clang  # make a soft like to the LLVM clang compiler in version 15
$
```

Then I added line _export PATH="$HOME/.local/bin:$PATH"_ to my _~/.bashrc_ configuration file and activated it with command _$ source ~/.bashrc_.

However, (only) compiling and building with command _$ ghc -O2 -fllvm random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_llvm_ didn't render a faster executable (though, it wasn't slower either).

<br/>

Another conclusion from above list is that the best combination of compiler switches is obviously still human, manual testing work, also in "the age of AI coding".
Just activating all kind of potentially suitable compiler switches can actually make an generated executable slower as list entry #3 (for _random_streams_for_perf_stats_optim1_) shows! 

<br/>

With around 43 milliseconds of execution time (as of 2026-07-26), the Haskell based executable is (significantly) slower than its
[OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/random_streams_for_perf_stats_main.ml),
[Bigloo Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo/random_streams_for_perf_stats.scm)
and [Common Lisp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/random_streams_for_perf_stats2.lisp) counterparts.

Interestingly, its about the same as the Standard Ml (MLton) based executable's execution time, a solution which is (also) using pre-allocated arrays, though those must be order-reversed before string concatenations.

Though the Haskell executable is beating the other pure functional implementation in [Roc](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Roc/random_streams_for_perf_stats.roc), a solution which isn't applying any "imperative tinkering" but doing functional list building.

<br/>

tbd 

<br/>

##_end
