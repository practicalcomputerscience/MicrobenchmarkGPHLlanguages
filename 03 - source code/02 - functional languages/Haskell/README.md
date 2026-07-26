2026-07-26: work in progress tbd

<br/>

# Haskell

https://www.haskell.org/ghc/

GHC = Glasgow Haskell Compiler: https://gitlab.haskell.org/ghc

<br/>

After [Roc](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Roc#roc),
this is my second implementation of the microbenchmark program in a _pure_ functional programming language:
[Two branches of Functional Programming (FP): pure and impure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#two-branches-of-functional-programming-fp-pure-and-impure)

<br/>

## Installation tips

I started with a "proper" installation via GHCup: https://www.haskell.org/ghcup/

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





<br/>

##_end
