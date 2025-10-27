2025-10-25: work in progress: check TBD's

# Gleam

Gleam is a statically typed, functional language on Erlang's virtual machine (vm) [BEAM](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/): 

- https://gleam.run/
- https://github.com/gleam-lang/gleam

---

Table of contents:

- [Installation tips](#installation-tips)
- [Type checking in Gleam](#type-checking-in-gleam)
- [Lists in Gleam](#lists-in-gleam)

---

### Installation tips

I looks like that Gleam's preferred package manager is **Homebrew**. Since "Gleam compiles to Erlang code", the installation of Gleam with Homebrew should also automatically install Erlang:

- https://brew.sh/
- https://docs.brew.sh/Homebrew-on-Linux
- https://gleam.run/getting-started/installing/

```
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Above command installed Homebrew in directory: _/home/linuxbrew/.linuxbrew_

Then I manually added this line to my _.bashrc_ file, because I want to keep the configuration for [SDKMAN and Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/20%20-%20language%20versions#on-sdkman-and-kotlin) at the bottom of this configuration file:

```
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
```

Activate the _.bashrc_ file and test the help command of Homebrew for example:

```
$ source ~/.bashrc
$ brew help
...
Further help:
  brew commands
  brew help [COMMAND]
  man brew
  https://docs.brew.sh
$
```

Now, I should be able to install Gleam and Erlang, which takes its time, and do some tests:

```
$ brew install gleam
...
==> Pouring gleam--1.13.0.x86_64_linux.bottle.tar.gz
🍺  /home/linuxbrew/.linuxbrew/Cellar/gleam/1.13.0: 9 files, 23MB
==> Running `brew cleanup gleam`...
...
$ erl -version
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 16.1.1
$ gleam -V
gleam 1.13.0
$ gleam new hello_world  # create a little Gleam project
$ cd hello_world
$ gleam test
  Resolving versions
Downloading packages
 Downloaded 2 packages in 0.04s
      Added gleam_stdlib v0.65.0
      Added gleeunit v1.6.1
  Compiling gleam_stdlib
  Compiling gleeunit
  Compiling hello_world
   Compiled in 0.46s
    Running hello_world_test.main
.
1 tests, no failures
$ cat ./src/hello_world.gleam
import gleam/io
pub fn main() -> Nil {
  io.println("Hello, world from Gleam on the Erlang virtual machine!")
}
$ gleam run
  Compiling hello_world
   Compiled in 0.22s
    Running hello_world.main
Hello, world from Gleam on the Erlang virtual machine!
$
```

Install this package: _$ gleam add simplifile_ to make the Gleam programs run.

### Type checking in Gleam

> Gleam has no null, no implicit conversions, **no exceptions**, and always performs full type checking. If the code compiles you can be reasonably confident it does not have any inconsistencies that may cause bugs or crashes.

from: https://tour.gleam.run/everything/#basics-type-checking

(my emphasis in bold)

### Lists in Gleam

So far, I've only found [Lists](https://tour.gleam.run/everything/#basics-lists) ("ordered collections of values", (*)) and not mutable arrays, like in [OCaml](TBD) or [MLton Standard ML](TBD).

This means that I cannot pre-allocate any memory for the three "ordered collections of values", that is _x_, _bits_x_ and _bits_hex_: (TBD)

However, Gleam claims (*) that:

> Lists are immutable single-linked lists, meaning they are very efficient to add and remove elements from the front of the list.

However, since the returned lists are in reverse order due to prepending  them in the master loop, I reverse them back to correct order, the order like in the other microbenchmark programs.

Prepending was adviced to me by the code checker (with command _$ gleam test_) for my first version like this: _[..x, new_seed]_, a version which caused a _Syntax error_ even:

> Lists are immutable and singly-linked, so to append items to them
> all the elements of a list would need to be copied into a new list.
> This would be slow, so there is no built-in syntax for it.
> 
> Hint: Prepend items to the list and then reverse it once you are done.

So, I changed it to: _[new_seed, ..x]_

<br/>

##_end
