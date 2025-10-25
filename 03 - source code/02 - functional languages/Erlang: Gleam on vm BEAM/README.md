2025-10-25: work in progress

# Gleam

Gleam is a statically typed, functional language on Erlang's virtual machine (vm) [BEAM](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/): 

- https://gleam.run/
- https://github.com/gleam-lang/gleam

---

Table of contents:

- [Installation tips](#installation-tips)

<br/>

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

<br/>



(TBD)

<br/>

##_end
