# Gleam

Gleam is a statically typed and minimalistic **functional** language on Erlang's virtual machine (vm) [BEAM](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/): 

- https://gleam.run/
- https://github.com/gleam-lang/gleam

Expect to use **Erlang** resources for certain functionalities. Also be aware that this is a young programming language, where changes are still common.

Gleam code can also be transpiled into **JavaScript** code.

Re "minimalistic functional language", here an example:

> [!NOTE]
> You cannot print the type of a variable during runtime!

(Theoretically) you could do this indirectly by delibertalely making errors in your code and then looking at the tips given by the _$ gleam test_ command.

Also noteworthy: Gleam does not know **exceptions**:

- https://tour.gleam.run/advanced-features/use/
- https://gleam.run/frequently-asked-questions/#why-does-division-by-zero-return-zero

<br/>

I have chosen statically typed Gleam over much more popular [Elixir](https://elixir-lang.org/) (on Erlang's virtual machine), because that is dynamically typed. I thought statically typed would be just more interesting, specifically with functional programming languages.

---

Table of contents:

- [Installation tips](#installation-tips)
- [Lists](#lists)
- [Using Erlang from Gleam](#using-erlang-from-gleam)
- [From a list of integer numbers to a string](#from-a-list-of-integer-numbers-to-a-string)
- [Recursive functions in Gleam](#recursive-functions-in-gleam)
- [Conclusion](#conclusion)

<br/>

---

## Installation tips

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

Install this package: _$ gleam add simplifile_ in a Gleam project directory to make the Gleam program workable -- **after** having created the related project with: _$ gleam new <project name>_

<br/>

## Lists

So far, I've only found [Lists](https://tour.gleam.run/everything/#basics-lists) ("ordered collections of values", (*)) and not mutable arrays, like in [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/password_encryption_main.ml) or [MLton Standard ML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats3.sml).

This means that I cannot pre-allocate any memory for the three "ordered collections of values", that is _x_, _bits_x_ and _bits_hex_: [recursive master loop](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/aba4c605c0c1a089f51374d935545eb780135d61/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM/random_streams_for_perf_stats.gleam#L104)

However, Gleam claims (*) that:

> Lists are immutable single-linked lists, meaning they are very efficient to add and remove elements from the front of the list.

However, since the returned lists are in reverse order due to prepending  them in the master loop, I reverse them back to correct order, the order like in the other microbenchmark programs.

Prepending was adviced to me by the code checker (with command _$ gleam test_) for my first version: _[..x, new_seed]_, a version which caused a _Syntax error_ even:

> Lists are immutable and singly-linked, so to append items to them
> all the elements of a list would need to be copied into a new list.
> This would be slow, so there is no built-in syntax for it.
> 
> Hint: Prepend items to the list and then reverse it once you are done.

So, I changed it to: _[new_seed, ..x]_

<br/>

## Using Erlang from Gleam

With the "speed part" of the [program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM/random_streams_for_perf_stats.gleam#L104), coding in Gleam was a rather pleasant experience. But with the development of the [full program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM/random_bitstring_and_flexible_password_generator.gleam) things started to become complicated when [Reading user input from the keyboard into a string](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages?tab=readme-ov-file#reading-user-input-from-the-keyboard-into-a-string):

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

## From a list of integer numbers to a string

Putting together a string like this for example: "ABC", starting from a list of integer numbers, is not the easiest task in Gleam. You have to go via (Unicode) codepoints, a subtask which involves obtaining results of the [Result](https://github.com/gleam-lang/stdlib/blob/main/src/gleam/result.gleam) type ("Option" type in functional programming) - at least implicitly.

The "trick" here is avoid touching the _Result_ type with your code, like this for example:

```
import gleam/list
import gleam/string

pub fn main() {
  let int_numbers = [65, 66, 67]  // 'A', 'B', 'C': example list of integer numbers to represent Unicode codepoints

  // convert a list of integer numbers into a list of codepoints:
  let codepoints = list.filter_map(int_numbers, fn(n) { string.utf_codepoint(n) })
  // the danger lies here at function: utf_codepoint(value: Int) with return type: Result(UtfCodepoint, Nil)
  // codepoints looks like this: [Ok(65), Ok(66), Ok(67)]

  // convert a list of codepoints into a string:
  let strings = string.from_utf_codepoints(codepoints)
  // luckily, function from_utf_codepoints accepts an argument like: [Ok(65), Ok(66), Ok(67)]

  echo strings  // "ABC"
}
```

See sources from here:
- function [utf_codepoint](https://github.com/gleam-lang/stdlib/blob/126db53b626e38cd5aea98a2937a16a51662a6b6/src/gleam/string.gleam#L740C8-L740C64)
- function [from_utf_codepoints](https://github.com/gleam-lang/stdlib/blob/126db53b626e38cd5aea98a2937a16a51662a6b6/src/gleam/string.gleam#L734C8-L734C27), which is using resources from Erlang or JavaScript

Above working program means that also a single string character has to be treated in list form:

```
let my_single_codepoint_as_int = [65] 
```

So far, I haven't found a more direct way to convert single integer number 65 into a string of one character, that is "A".

<br/>

## Recursive functions in Gleam

The implementation of the two recursive functions in the [full program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM/random_bitstring_and_flexible_password_generator.gleam) may not to the best approach, but they work:

```
// recursive password loop
...
fn pw_generator (j: Int, pw_str: String, n: Int, x: List(Int), char_set: String) -> String {
  ...
}

// recursive master loop
fn masterloop(n: Int, seed: Int, x: List(Int), bits_x: List(String), bits_hex: List(String)) {
  ...
}
```

Compared to other functional programming languages, these recursions look clumsy in at least two aspects in my opinion.

One aspect is that functions _masterloop_ and _pw_generator_ may be part of the _main_ function in those other functional languages and may not be just some other user defined functions like in my Gleam code.

The second aspect is that in other functional languages, I could locate the recursive "accumulator loop" inside the bigger function, but it seems that such a construct is not supported in Gleam. The official documention places the "accumulator loop" in its own private function, in this example called _factorial_loop_: https://tour.gleam.run/flow-control/tail-calls/ (**)

One reason for these clumsy recursions may be the fact that Gleam **doesn't have mutable global variables**, something I use in other functional languages if possible.

However, Gleam claims that **tail call optimisation** is applied (**):

> When a function is called a new stack frame is created in memory to store the arguments and local variables of the function. If lots of these frames are created during recursion then the program would use a large amount of memory, or even crash the program if some limit is hit. To avoid this problem Gleam supports tail call optimisation, which allows the stack frame for the current function to be reused if a function call is the last thing the function does, removing the memory cost. 

<br/>

## Conclusion

All in all, I think that Gleam is an interesting alternative on Erlang's virtual machine BEAM.

With:

- immutable single-linked lists
- no arrays and no vectors
- no mutable global variables
- tail call optimisation

..Gleam may give the impression that it may be a "pure functional programming language". But it's not. You don't have to worry too much about side effects, like input/output operations, here.

<br/>

##_end
