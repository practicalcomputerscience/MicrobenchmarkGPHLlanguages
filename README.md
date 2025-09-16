2025-07-16: starting this heavy work in progress

To-do:
- fix TBD's
- own Wiki page
- implement a concurreny solution in Chapel, compare to the Go solution
- test: exhausting the generated random bitstream when user asks for a super-long password

<br/>

Table of contents:

- [Microbenchmark: a simple pseudo-random number generator with a user dialog for password generation in 25+ general purpose, high-level programming languages](#microbenchmark-a-simple-pseudo-random-number-generator-with-a-user-dialog-for-password-generation-in-25-general-purpose-high-level-programming-languages)
- [Python environments](#python-environments)
- [User dialog](#user-dialog)
- [Reading user input from the keyboard into a string](#reading-user-input-from-the-keyboard-into-a-string)
- [In terms of execution times this is a string concatenation benchmark](#in-terms-of-execution-times-this-is-a-string-concatenation-benchmark)
- [This program is not bullet proof](#this-program-is-not-bullet-proof)
- [Motivation and orientation](#motivation-and-orientation)
- [Parameters for the LCG (linear congruential generator)](#parameters-for-the-lcg-(linear-congruential-generator))
- [Other aspects of a computer programming language](#other-aspects-of-a-computer-programming-language)
- [On including lots of comments in my source code files](#on-including-lots-of-comments-in-my-source-code-files)
- [On debugging](#on-debugging)
- [On functional programming](#on-functional-programming)
- [On configuring building and execution environments](#on-configuring-building-and-execution-environments)
- [Error handling, exception handling and "Quality control"](#error-handling-exception-handling-and-quality-control)
- [Prompt engineering](#prompt-engineering)
- [AI experiments](#ai-experiments)
- [The 1 second execution time limit](#the-1-second-execution-time-limit)
- [Measuring program execution times](#measuring-program-execution-times)

---

# Microbenchmark: a simple pseudo-random number generator with a user dialog for password generation in 25+ general purpose, high-level programming languages

Starting in March 2025, this is an opionated project which has its origin in just implementing a simple pseudo-random number generator, here a Linear Congruential Generator (LCG), in my usual programming language Python.

It's core algorithm is this in Python:

```
x[i] = (a*x[i-1] + c) % m
```

This recursion is part of the so called "masterloop", which at the same time makes the core of the first part of the program. I call it the "speed part" because here I measure the execution time of a program, be it compiled or interpreted like in this case:

```import numpy as np
from io import StringIO

END = 62501  # 62501 for exactly 1M binary digits

m = 65521  # = 2^16 - 15
a = 17364
c = 0

file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"

x = [0 for i in range(0,END)]
x[0] = np.random.randint(0, m, size=1, dtype=int)[0]

bits_x = StringIO()
bits_hex = StringIO()

print("\ngenerating a random bit stream...")
for i in range(1,END):  # masterloop
    x[i] = (a*x[i-1] + c) % m
    bits_x_str = format(x[i], '016b')
    bits_x.write(bits_x_str)

    bits_x_str = format(x[i], '04x')  # needed for program ENT
    bits_hex.write(bits_x_str)

try:
  with open(file_bits_x, "w", encoding="utf8") as f:
      f.write(bits_x.getvalue())
except Exception as e:
  print(f"could not write to file: {file_bits_x} ! -- {e}")
else:
  print(f"Bit stream has been written to disk under name:  {file_bits_x}")

try:
  with open(file_bits_hex, "w", encoding="utf8") as f:
      f.write(bits_hex.getvalue())
except Exception as e:
  print(f"could not write to file: {file_bits_hex} ! -- {e}")
else:
  print(f"Byte stream has been written to disk under name: {file_bits_hex}")
```

<br/>

#### Python environments 

In Linux, but probably also increasingly in other environments like Windows too, it has become very important to work with **Python environments** to not mess up your default Python installation which automatically comes with your Linux distribution. I think this has become specifically important for Ubuntu (https://ubuntu.com/download), which I'm using here (as Ubuntu 24 LTS). ```$``` indicates the Bash shell in Linux (I've just chosen _prng_test_ as my environment name):

1. create environment: ```$ python3 -m venv ./prng_test```
2. activate environment: ```$ source ./prng_test/bin/activate```
3. change directory: ```(prng_test) $ cd ./prng_test```
4. install _numpy_ package: ```(prng_test) $ pip3 install numpy```
5. run the program: ```(prng_test) $ python3 ./random_streams_for_perf_stats.py```
6. simple time measurement: ```(prng_test) $ time python3 ./random_streams_for_perf_stats.py```
7. deactivate this environment after work: ```(prng_test) $ deactivate```
8. remove or delete an enviroment if desired: ```$ rm -rf ./prng_test```

Console input and output:
```
(prng_test) $ time python3 ./random_streams_for_perf_stats.py

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0,148s  # this is the so called "wall clock" time. I focus on this time measurement.
user	0m1,201s
sys	0m0,017s
(prng_test) $
```

Check both files with generated strings:
```
(prng_test) $ ls -l *bitstring*
-rw-rw-r-- 1 ... ... 1000000 Jul 16 15:20 random_bitstring.bin
-rw-rw-r-- 1 ... ...  250000 Jul 16 15:20 random_bitstring.byte
(prng_test) $
```

<br/>

Then I check the quality of randomness of the random "bit stream" string at: https://mzsoltmolnar.github.io/random-bitstream-tester/

..where I manually copy and paste exactly 1,000,000 binary digits (with ASCII characters '0' and '1') into the "Manual bitstream input" with the target to pass all tests with no "Error" indication. Usually this is the case.

A second string with 250,000 random hexadecimal digits (with ASCII characters from '0' to 'f') is also generated as input for this program:

*ENT - A Pseudorandom Number Sequence Test Program*: https://www.fourmilab.ch/random/

..though I'm not sure what to make out of its test results.

<br/>

#### User dialog

Out of the first random binary digits, this program - after some dialog with the user on the console - is creating a password of printable, random characters of the desired length:

```
import re
...  # see from above
N_CHAR = 12
answer = False
while answer is False:
    reply = input(f'\nPassword of {N_CHAR} printable chars OK? \
"y" or another integer number >= 8: ')
    if reply == 'y':
        answer = True
    else:
        try:
            N_CHAR = int(reply)
            if N_CHAR >= 8:
                answer = True
            else:
                N_CHAR = 12
                print('enter an integer number >= 8 or "y"')
        except ValueError:
            N_CHAR = 12
            print('enter an integer number >= 8 or "y"')

def binary_to_string(bits):
    return ''.join([chr(int(i, 2)) for i in bits])

WITH_SPECIAL_CHARS = True
answer = False
while answer is False:
    reply = input('\nDo you want me to use special characters like .;,+*... ? "y" or "n": ')
    if reply == 'y':
        answer = True
    else:
        WITH_SPECIAL_CHARS = False
        answer = True

if WITH_SPECIAL_CHARS is True:
    pattern = re.compile(r"[A-Za-z0-9!\"#$%&'()*+,-./:;<=>?@[\]\\^_`{|}~]+")
else:
    pattern = re.compile(r"[A-Za-z0-9]+")

i = 0  # char counter in password
j = 0  # char counter in bits_char
pw_chars = []

while i < N_CHAR:
    bin0 = f'{x[j]:016b}'
    bin0_0 = bin0[0:8]   # position 8 is exclusive in Python
    bin0_1 = bin0[8:16]

    char0 = binary_to_string([bin0_0])
    char1 = binary_to_string([bin0_1])

    if pattern.fullmatch(char0) is not None:
        pw_chars.append(char0)
        i += 1
        if i == N_CHAR:
            break
    if pattern.fullmatch(char1) is not None:
        pw_chars.append(char1)
        i += 1
    j += 1

pw_string = ''.join(pw_chars)
print(f'\nmy password of {N_CHAR} characters is:', pw_string)
```

Run the complete program like this:
```
(prng_test) $ python3 ./random_bitstring_and_flexible_password_generator.py

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

Password of 12 printable chars OK? "y" or another integer number >= 8: y

Do you want me to use special characters like .;,+*... ? "y" or "n": y

my password of 12 characters is: {5mkkR""dmtC
(prng_test) $ 
```

#### Reading user input from the keyboard into a string

It's amazing what you can learn about a new programming language when implementing - or trying to implement - a little dialog with the user on the console, that is reading user input from the keyboard into a string in the year 2025!

So far with one programming language (https://www.ponylang.io/) I had to give up further development because I was not able to implement this dialog. With one famous language, that is Go, I'm still not sure if this allegedly little task is finally working like it should and actually does in almost all other programming languages!


### In terms of execution times this is a string concatenation benchmark

Though, it took me some programming languages to figure this out nonchalantly. 

Now when I test a new programming language, I usually still implement the naive solution first and if it feels not very speedy, I start to look for a string builder or similar concept.

Often a string builder dramatically improves execution speed but not always! I try to remember the language where naive string concatenation was not the major speed bottleneck, but how to store the generated random integer numbers ```x[i]``` effectively!

This is why the much praised best practices should be documented immediately and sufficiently.

#### This program is not bullet proof

It checks for a minimum number of 8 password characters, but usually not for an upper limit, except in its **Ada** variant (Ada is a language which is incentivizing to do things like this: https://alire.ada.dev/). Theoretically, a user could exhaust the generated random bitstream when asking for a super-long password. I've not tested this so far. See from source code above at chapter: [User dialog](#user-dialog)

The Zig program (![Zig source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig)) is an example of a non-bullet proof program: at both questions, the user, who is not aware of this, can only safely enter up to 99 characters as his answer (stored into an array of type u8):

```
var buf99: [99]u8 = undefined;
```

If he enters 100 characters or more, the program will go into exception and terminates. I have not tested the latest program version yet, but with the much smaller buffer of only 16 characters, which I initially also used for the user inputs:

```
.../random_bitstring_and_flexible_password_generator.zig:172:13: 0x10e19b8 in main (random_bitstring_and_flexible_password_generator)
        if (try stdin.readUntilDelimiterOrEof(buf16[0..], '\n')) |answer_str| {  
```

<br/>

## Motivation and orientation

After improving my Python program here and there, I thought that this a good computer program for me to test new languages.

Though only in the middle of June 2025 I made further improvements to my original Python program because I learned that also Python has a concept for fast string concatenation: *StringIO*: https://docs.python.org/3/library/io.html#io.StringIO

This helped to make the Python program running faster. In other words: I still revisit old programs to improve them, not only in terms of execution speed but also in terms of number of lines of source code ("verbosity", "LOC") or memory leaks after program exit, if feasible.

Though this is not meant to be project for benchmarking execution speeds, nor has it started as such, but I always take a deeper look into a program when I have the feeling that it's maybe unfairly on the slow side because of my deficits with using a new programming language.

However, measuring execution speeds is the easiest of all benchmarking:

_**Even though most benchmarks aren’t worth the pixels they’re printed on, people seem to like them...**_ from: https://wren.io/performance.html

True, but nevertheless they are fun and provide - across several microbenchmarks - orientation.

<br/>

**Go** (https://go.dev/) was the next general purpose, high-level programming language after Python. And then one (general purpose, high-level) programming language followed the other, sometimes chaotically, sometimes more systematically.

I'm aware of other (micro-)benchmarks like these for example:

- https://github.com/jinyus/related_post_gen

- https://programming-language-benchmarks.vercel.app/

- https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html

- https://freedium.cfd/https://medium.com/@codeperfect/we-tested-7-languages-under-extreme-load-and-only-one-didnt-crash-it-wasn-t-what-we-expected-67f84c79dc34

- https://eklausmeier.goip.de/blog/2023/03-25-performance-comparison-c-vs-java-vs-javascript-vs-php-vs-python-vs-cobol-dart/

- and also the **TIOBE Index** of popularity: https://www.tiobe.com/tiobe-index/

To some extent these sites have been a source of inspiration to test new languages. Often I become curious about a new language for me by reading about the background of the programming language I'm currently working with.


<br/>

#### Parameters for the LCG (linear congruential generator)

Originally I tumbled into this web page: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html from where I've chosen _modul p_ = 2^16 - 15 = 	65521 and which I usually name **m** or **M** in my programs. Together with multiplicator _a_ = 17364, usually named **a** or **A** in my programs.

Parameter _c_ is 0 with this algorithm, which is named _b_ in the web page linked above. I've already forgotten why I changed the name from _b_ to _c_.

Here's the original source of this LCG: https://www.jstor.org/stable/2585108 : _Tables of Linear Congruential Generators of Different Sizes and Good Lattice Structure_, Pierre L'Ecuyer, Mathematics of Computation, Vol. 68, No. 225 (Jan., 1999), pp. 249-260 (12 pages), Published By: American Mathematical Society -- DOI: 10.1090/S0025-5718-99-00996-5

<br/>

The choice of 2^16 - 15 = 65521 was easy for me because it's the highest 16-bit modulus where the whole, recursive calculation can fit into 32 bits. This makes this pseudo-random number generator easy to port from one platform to the other.

<br/>

#### Other aspects of a computer programming language

The program is too short to explicitly test important aspects of general purpose, high-level programming languages, like:

- object orientation and

- concurreny for example.

Though, in one instance I've made a derivative program of the "speed part" to see how concurrency works in Go. This was rather easy and as easy as advertised. However, I've no intention to do this with other programming languages with the exception of Chapel (https://chapel-lang.org/) maybe, where I _accidentally_ tumbled into its _foreach_ loop: https://chapel-lang.org/docs/technotes/foreach.html

<br/>

Many general purpose, high-level programming languages have been designed in the last 20 years or so from scratch to natively, that is without an extra framework, facilitate programming **concurrency** safely and conveniently, like for example Clojure, Go, Julia, Mojo, Rust, Scala, Swift, V and so on.

Once I collected these frameworks, which in one way or the other promote concurrent program execution:

- AMD ROCm / HIP: https://github.com/ROCm/HIP
- C++ Accelerated Massive Parallelism (C++ AMP): https://learn.microsoft.com/en-us/cpp/parallel/amp/cpp-amp-cpp-accelerated-massive-parallelism
- CUDA (Compute Unified Device Architecture by NVIDIA): https://developer.nvidia.com/cuda-zone
- Kokkos: https://performanceportability.org/perfport/frameworks/kokkos/
- Message Passing Interface (MPI): https://www.mpi-forum.org/
- oneAPI DPC++ (DPC++ = Data Parallel C++): https://intel.github.io/llvm/GetStartedGuide.html
- OpenACC: https://www.openacc.org/
- OpenCL: https://www.khronos.org/opencl/
- OpenMP: https://www.openmp.org/
- Open SYCL, now called AdaptiveCpp: https://github.com/AdaptiveCpp/AdaptiveCpp

These HPC (High Performance Computing) frameworks seem to share one feature of general purpose, high-level programming language: there's no guarantee that the framework of your choice will survive the next 20 years in well maintained shape!

<br/>

#### On including lots of comments in my source code files

Usually lots of comments in source code files, potentially not following any strategy, is seen as bad: https://expertbeacon.com/putting-comments-in-code-the-good-the-bad-and-the-ugly/

However, I'm just doing this since ages. So, my comments may not even be helpful for myself after some time (see above at "I try to remember the language..." for example). Thus I'm also usually not putting any directives for later documentation into my source code files. I also don't use any documentation or versioning tools. My version control is the last date in the header comment block, manually set.

I just work with Notepad++ (https://notepad-plus-plus.org/) at a Windows 11 PC as my source code editor (but Ubuntu Linux is my testing environment). I also tested other editors only to drop them. I also use Notepad++ for configuration files of build tools for example. Thus, I'm also not using any IDE (Integrated development environment), though for Scala I installed IntelliJ IDEA (Community Edition for Windows: https://www.jetbrains.com/idea/download/?section=windows) to play around a bit with its numerous possibilities.

But I'm not here for an exercise in enterprise level software engineering.

#### On debugging

In no case so far I used an explicit debugging tool. My "debugger" are still print expressions in all languages, the "Poor man's debugger", which I finally comment. So, my source code is full of expressions like these, here from the Ada program:

```
   --  for testing:
   --  Put ("x (1) = "); Ada.Integer_Text_IO.Put (x (1)); New_Line;
```

The Crystal programming language (https://crystal-lang.org/) even features the _p!_ macro (https://crystal-lang.org/api/1.17.0/toplevel.html#p!(*exps)-macro) to be "Useful for print style debugging", for example like this:

```
# p! bits_x  # for testing
```

<br/>

### On functional programming

This project was the first time I deliberately came into contact with **functional programming languages**. At one point it made me to divide the programs into two sections:

- imperative programming languages (I've not seen yet an Object-Oriented Programming (OOP) language which is not imperative "by nature") and

- functional programming languages, from "soft" like Common Lisp (https://lisp-lang.org/) to "harsh" or "pure" like it's officially called, here with Roc: https://www.roc-lang.org/

The concepts of functional programming and concurrency are linked because:

> As I explained previously pure functions and immutable data make a program thread-safe. Hence your software becomes scalable.

From: https://medium.com/twodigits/advantages-and-disadvantages-of-functional-programming-52a81c8bf446

<br/>

#### On configuring building and execution environments

My testing environment is this:

- Operating system (OS): Ubuntu 24 LTS or in long, currently (_$ uname -a_): _Linux ... 6.8.0-63-generic #66-Ubuntu SMP PREEMPT_DYNAMIC Fri Jun 13 20:25:30 UTC 2025 x86_64 x86_64 x86_64 GNU/Linux_

My (older) hardware from 2022 is always this:

- CPU: 11th Gen Intel(R) Core(TM) i7-11700K @ 3.6GHz
- mainboard: Gigabyte Technology, Z590 GAMING X
- RAM: 32GB of DDR4-3200
- SSD: 512GB Crucial MX500 SSD, CT500MX500SSD1Z

For the language versions see here: ![Language versions](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/20%20-%20language%20versions)

Some versions were already a bit outdated when I started this project; for example with Perl in version 5.38.2, which just came with Ubuntu 24 LTS. This is an example where I don't want to break things potentially. However, if possible and deemed safe, I built or installed the latest stable language versions and latest build tools when I started a new language. 

With some languages I use a related build tool, for example _sbt_ for Scala (https://www.scala-sbt.org/), _Dune_ for OCaml (https://dune.readthedocs.io/en/stable/quick-start.html), _Alire_ for Ada (https://alire.ada.dev/), or _Leiningen_ for Clojure (https://leiningen.org/).

Also mastering compiler switches, like in C for example, and mastering build tools has become an art in itself!

<br/>

#### Error handling, exception handling and "Quality control"

I use the terms "error handling" and "exception handling" interchangeable, but in case of doubts I mean _exception handling_.

Functional programming has become significantly more relevant for the mainstream of computer programming in the last 20 years or so. Rust and Co. are heavily influenced by it. This is the real change in computer programming with general purpose, high-level programming languages for me in the last two decades.

And Error handling played a driving force here from my point of view, because the functional approach to error handling has slipped into languages that are imperative by nature and not functional. Traditionally, the imperative approach to error handling is the _try-catch_ block as shown above in the second source code box with Python code.

A _pure_ functional programming language like Haskell, or here Roc, is _forcing_ you to care about error handling. Well, hopefully.

I call this error handling "Quality control", the quality control of data and information returned from function calls. Even though in numerous cases, I just don't care about it - like in the "old days" - and suppress error handling, like here in Go with these (mandatory) _ = <_error value_> expressions:

```
...
  if WITH_SPECIAL_CHARS {
    pattern, reg_err = regexp.Compile("[[:graph:]]+")  // true case
    _ = reg_err  // get rid of the "declared and not used" error message
  } else {
    pattern, reg_err = regexp.Compile("[[:alnum:]]+")
    _ = reg_err
  }
...
```

from here: ![random_bitstring_and_flexible_password_generator.go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go/random_bitstring_and_flexible_password_generator.go)

<br/>

So, it's advisable to learn a little bit of functional programming nowadays, even though _Functional Programming is (obviously) not taking over the world_ as demonstrated here: [A little exercise in Common Lisp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/README.md#a-little-exercise-in-common-lisp)

<br/>

#### Prompt engineering

It was already with the Go program where I started to use MS Bing AI for writing me little functions. Later, with the functional programming languages like OCaml for example with enough sources around, my usage of MS Bing AI increased. However, in all cases I more or less translated the basic program manually from language to language. I tried to estimate the "hit rate" of my prompts (https://en.wikipedia.org/wiki/Prompt_engineering). I think it's somewhere between 5% and 25%. Because if a procedure is easy enough to write for me, even in a new programming language, where I often search in examples and GitHub repositories, it's faster for me to focus on writing the source code instead of trying to find better prompts.

I also consulted Stack Overflow (https://stackoverflow.com/questions) like in the "old times". From time to time, but not systematically, I added related reference notes in the source code files, also about my prompt engineering.

<br/>

Then come the turn to **Standard ML** (https://smlfamily.github.io/), where I discovered the **LunarML** transpiler (https://github.com/minoki/LunarML), which by default transpiles Standard ML code into **Lua** code, but also, if desired, into **JavaScript** code. However, I noticed that the "Hello world!" example of LunarML in Standard ML with two lines of source code:

```
$ cat ./LunarML-0.2.1/example/hello.sml
val it = 1 + 2;
print "Hello world!\n";
$
```

..translates into 95 lines of Lua source code, Lua of all languages! (Lua is a language designed for simplicity: https://www.lua.org/about.html). The JavaScript translation of this "Hello world!" example has only 5 lines of source code:

```
$ lunarml compile --nodejs ./LunarML-0.2.1/example/hello.sml
$ cat ./LunarML-0.2.1/example/hello.mjs
import {stdout} from "node:process";
cont: {
stdout.write(Uint8Array.of(72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33, 10));
break cont;
}
$
```

My Standard ML program with around 120 lines of source code (in one _~.sml_ file and one _~.mlb_ file) transpiled into 4,490 lines of Lua source code and 3,264 lines of JavaScript source code respectively! Apparently these source code files are not meant for the human reader, though both programs work correctly.

#### AI experiments

This gave me the idea to test two of these AI based translation services in the web with free and limited trials. I entered my Perl and PowerShell versions with target language Lua. The resulting Lua source code files were not working. Then I helped with my Ada version with its user defined functions to help overcoming the problem of functions which are available in one language but not in the core of another. This helped. Both services generated working and concise Lua scipts, albeit both almost equally slow. My manually produced Lua script only needs a third of the execution time of the AI generated scripts.

However, this episode showed me how challenging such a translation task really can become. This task is not only about the cores of two programming languages, but in many practical cases also about standard and third party **libraries**! The true reason why my Ada program was so helpful was because of my incompetence! Originally, I wanted to use the _Strings Edit_ library: https://www.dmitry-kazakov.de/ada/strings_edit.htm, but I was not able to figure out within an acceptable amount of time how to use such a third party library for Ada. Consequently, my Ada program has the most lines of source code with 231, the highest so far.

Bottom line for me: for convincingly translating whole programs from one arbitrary Top 50 language into another, this technology - even with all its massive success in a few years - still has to go some way. But for helping with writing a procedure here and there, based on "good" prompts, AI based coding is here to stay.

<br/>

#### The 1 second execution time limit

After some languages I noticed that all of them - often after some iterations of improvements - can run the "speed part" in under 1 second (with the PowerShell script being the edge case):

![plot](./02%20-%20execution%20times/mean_stddev_err_whiskers%20--%20no%20GraalVM.png)

I've also language implementations where - for various reasons - I'm not able to bring down the execution time under 1 second. I may list these solutions later and separately (TBD). This 1 second limit for my "official" listing is arbitrary, like so many things with benchmarks, but on the other side: most languages have no problem with this limit.

#### Measuring program execution times

With the first languages I put a stopwatch into the source code. But after doing so in the Mojo program, here like this:

```
from time import monotonic
...
    # stopwatch:
    var t0: UInt64
    t0 = monotonic()  # the current monotonic time in nanoseconds
    var t1:       UInt64
    var duration: UInt64
...
    t1 = monotonic()
    duration = (t1 - t0) / 1_000_000
    print("this took", duration, "ms to run")
...
```

..I got doubts about an acceptable level of correctness. So, I deleted source code like this and started to measure the execution time of a program only _externally_.

For slower programs, maybe with an execution time of 100 milliseconds and up, I first used a Bash shell script named _exe_times_statistics_for_one_test_case_in_cwd2_ or a variant named _...cwd2a_: ![Bash shell scripts to measure slower execution times](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/README.md#bash-shell-scripts-to-measure-slower-execution-times), like this for example for the PowerShell script:

```
$ ./exe_times_statistics_for_one_test_case_in_cwd2 pwsh random_streams_for_perf_stats.ps
```

However, I got doubts again, although in all cases where I compared both methods to measure execution times specifically the results are quite the same:

```
$ sudo perf stat -r 20 pwsh random_streams_for_perf_stats.ps
```

So, in the end I only use the _perf-stat_ program because this is the method which can also precisely measure the execution times of the faster programs.

<br/>

Implementing a stopwatch in a new programming language is usually a learning experience and sometimes, in one or the other functional programming language, even a challenge like this in OCaml:

```
...
  let t10a = Mtime_clock.now_ns () in
  let t10b: int64 = Int64.div t10a 1_000_000L in
  let t10c: int = Int64.to_int t10b in
...
  let t11a = Mtime_clock.now_ns () in
  let t11b: int64 = Int64.div t11a 1_000_000L in
  let t11c: int = Int64.to_int t11b in
  let duration = t11c - t10c in
  Printf.printf "\nthis took %dms to run\n" duration;
  (* https://github.com/janestreet/core/blob/4e9e8cfb8d2e2016aef5f631a57ae9a936ba7b60/core/src/timezone.ml#L37 *)
...
```

So, I kept the old program versions to see how to read the operating system's monotonic clock.

<br/>

##_end
