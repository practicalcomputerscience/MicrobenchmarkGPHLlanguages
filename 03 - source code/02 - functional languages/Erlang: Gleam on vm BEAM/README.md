2025-10-25: work in progress: check TBD's

# Gleam

Gleam is a statically typed and minimalistic functional language on Erlang's virtual machine (vm) [BEAM](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/): 

- https://gleam.run/
- https://github.com/gleam-lang/gleam

Expect to use **Erlang** resources for certain functionalities. Also be aware that this is a young programming language, where changes are still common.

Gleam code can also be transpiled into **JavaScript** code.

Re "minimalistic functional language", here an example:

> [!NOTE]
> You cannot print the type of a variable during runtime!

(Theoretically) you could do this indirectly by delibertalely making errors in your code and then looking at the tip of the _$ gleam test_ command.

(TBD)

---

Table of contents:

- [Installation tips](#installation-tips)
- [Lists](#lists)
- [Using Erlang from Gleam](#using-erlang-from-gleam)
- [From a list of integer numbers to a string](#from-a-list-of-integer-numbers-to-a-string)

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

### Lists

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

### Using Erlang from Gleam

With the "speed part" of the [program](TBD), coding in Gleam was a rather pleasant experience. But with the development of the [full program](TBD) things started to become complicated when [Reading user input from the keyboard into a string](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages?tab=readme-ov-file#reading-user-input-from-the-keyboard-into-a-string):

> [!IMPORTANT]
> Apparently and only recently certain functionalities have been pushed out of Gleam! This means that Erlang resources have to be used directly, something which isn't so easy (in my opinion).

Look at this example from 2024 only: the _erlang_ package is no longer existing and with it the _erlang.get_line_ function is gone: https://github.com/gleam-lang/gleam/discussions/2748

So, a _get_line_ functionality must now, in 2025, be used from Erlang directly.

A search brought me to the _io_ module of Erlang's standard library: https://www.erlang.org/doc/apps/stdlib/io.html#get_line/1

Then I asked MS Bing AI (again), and got this answer to prompt "Gleam Erlang io get_line": 

```
import gleam/io
import gleam/string
// import gleam/erlang  // this is not needed anymore

// Declare an external function binding to Erlang's io:get_line/1
@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> String

pub fn main() {
  // Prompt the user
  let line = get_line("Enter something: ")

  // Remove trailing newline characters
  let clean_line = string.trim(line)

  // Output the result
  io.println("You entered: " <> clean_line)
}
```

Run this program like this for example:

```
$ gleam run --no-print-progress
Enter something: 45 6.66
You entered: 45 6.66
$ 
```

This is exactly what I've been looking for!

<br/>

### From a list of integer numbers to a string

Putting together a string like this for example: "ABC", starting from a list of integer numbers, is not the easiest task in Gleam. You have to go via (Unicode) codepoints, a subtask which involves obtaining results of the [Result](https://github.com/gleam-lang/stdlib/blob/main/src/gleam/result.gleam) type ("Option" type in functional programming) - at least implicitly.

The "trick" here is avoid touching the _Result_ type with your code, like this for example:

```
import gleam/list
import gleam/string

pub fn main() {
  let int_numbers = [65, 66, 67]  // 'A', 'B', 'C': example list of integer numbers for Unicode codepoints

  // convert a list of integer numbers into a list of codepoints:
  let codepoints = list.filter_map(int_numbers, fn(n) { string.utf_codepoint(n) })
  // the danger lies here at function: utf_codepoint(value: Int) with return type: Result(UtfCodepoint, Nil)
  // codepoints looks like this: [Ok(65), Ok(66), Ok(67)]

  // convert a list of codepoints into a string:
  let strings = string.from_utf_codepoints(codepoints)  // luckily, function from_utf_codepoints accepts an argument like [Ok(65), Ok(66), Ok(67)]
  echo strings  // "ABC"
}
```

See sources from here:
- function [utf_codepoint](https://github.com/gleam-lang/stdlib/blob/126db53b626e38cd5aea98a2937a16a51662a6b6/src/gleam/string.gleam#L740C8-L740C64)_
- function [from_utf_codepoints](https://github.com/gleam-lang/stdlib/blob/126db53b626e38cd5aea98a2937a16a51662a6b6/src/gleam/string.gleam#L734C8-L734C27), which is using resources from Erlang or JavaScript


TBD

<br/>

##_end
