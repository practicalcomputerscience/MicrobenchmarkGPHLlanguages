2025-07-16: starting this heavy work in progress

(this page may go to a GitHub Page of this account)

To-do:
- Wiki page
- concurreny in Chapel
- test: exhausting the generated random bitstream when user asks for a super-long password

<br/>

# Microbenchmark: a simple pseudo-random number generator with a user dialog for password generation in 25+ general purpose, high-level programming languages

Starting in March 2025, this is a project which has its origin in just implementing a simple pseudo-random number generator, here a Linear Congruential Generator (LCG), in my usual programming language Python.

It's core algorithm, which I call the "masterloop", is this in Python:

```
x[i] = (a*x[i-1] + c) % m
```

Here's the complete "masterloop" in Python which is at the same time the first part of the program. I call it the "speed part" because only here I measure the execution speed of a program, be it compiled or interpreted like in this case:

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
for i in range(1,END):
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
  print(f"Byte stream has been written to disk under name: {file_bits_hex}")```

<br/>

#### Python environments 

In Linux, but probably also increasingly in other environments like Windows too, it has become very important to work with **Python environments** to not mess up your default Python installation which automatically comes with your Linux distribution. I think that this has become specifically important for Ubuntu (https://ubuntu.com/download), which I'm using here (as Ubuntu 24 LTS). ```$``` indicates the Bash shell in Linux (I've just chosen _prng_test_ as my environment name):

1. create environment: ```$ python3 -m venv ./prng_test```
2. activate environment: ```$ source ./prng_test/bin/activate```
3. change directory: ```(prng_test) $ cd ./prng_test```
4. install _numpy_ package: ```(prng_test) $ pip3 install numpy```
5. run the program: ```(prng_test) $ python3 ./random_streams_for_perf_stats.py```
6. simple time measurement: ```(prng_test) $ time python3 ./random_streams_for_perf_stats.py```
7. deactivate this environment after work: ```(prng_test) $ deactivate```
8. remove or delete an enviroment if desired: ```$ rm -rf ./prng_test```

Console output:
```(prng_test) $ time python3 ./random_streams_for_perf_stats.py```

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0,148s  # this is the so called "wall clock" time. I focus on this time measurement.
user	0m1,201s
sys	0m0,017s
(prng_test) $```

Check both files with generated strings:
```(prng_test) $ ls -l *bitstring*```
-rw-rw-r-- 1 booser booser 1000000 Jul 16 15:20 random_bitstring.bin
-rw-rw-r-- 1 booser booser  250000 Jul 16 15:20 random_bitstring.byte
(prng_test) $```

<br/>

Then I check the quality of randomness from the random "bit stream" string at: https://mzsoltmolnar.github.io/random-bitstream-tester/

..where I manually copy and paste exactly 1,000,000 binary digits (with ASCII characters '0' and '1') into the "Manual bitstream input" with the target to pass all tests with no "Error" indication. Usually this is the case.

A second string with 250,000 random hexadecimal digits (with ASCII characters from '0' to 'f') is also generated as input for this program:

*ENT - A Pseudorandom Number Sequence Test Program*: https://www.fourmilab.ch/random/

..though I'm not sure what to make out of its test results.

<br/>

Out of the first random binary digits, this program - after some dialog with the user on the console - is creating a password of printable, random characters of the desired length. It's amazing what you can learn about a new programming language when implementing - or trying to implement - a little dialog with the user on the console, that is with user input from the keyboard in the year 2025!

So far with one programming language (https://www.ponylang.io/) I had to give up further development because I was not able to implement this dialog.


### In terms of execution speed this is a string concatenation benchmark

Though, it took me some programming languages to figure this out nonchalantly. 

Now when I test a new programming language, I usually still implement the naive solution first and if it feels not very speedy, I start to look for a string builder or similar concept.

Often a string builder dramatically improves execution speed but not always! I try to remember the language where naive string concatenation was not the major speed bottleneck, but how to store the generated random integer numbers ```x[i]```!

(TBD)

#### This program is not bullet proof

It checks for a minimum number of 8 password characters, but usually not for an upper limit unless in its **Ada** variant (Ada is a language which is incentivizing to do things like this). Theoretically, a user could exhaust the generated random bitstream when asking for a super-long password. I've not tested this so far.

#### Motivation and orientation

After improving my Python program here and there, I thought that this a good computer program for me to test new languages.

Though only in mid of June 2025 I made further improvements to my original Python program because I learned that also Python has a concept for fast string concatenation: *StringIO* https://docs.python.org/3/library/io.html#io.StringIO

This helped to make the Python program running faster. In other words: I still revisit old programs to improve them, not only in terms of execution speed but also in terms of number of lines of source code ("verbosity", "LOC") or memory leaks after program exit, if feasible.

Though this is not meant to be project for benchmarking execution speeds, nor has it started as such, but I always take a deeper look into a program when I have the feeling that it's maybe unfairly on the slow side. Benchmarking execution speeds is the easiest of all benchmarking:

_**Even though most benchmarks aren’t worth the pixels they’re printed on, people seem to like them...**_ from: https://wren.io/performance.html

True, but nevertheless they are fun and provide - across several microbenchmarks - orientation.

<br/>

## A computer program for me to test new languages

Like **Go** for example (https://go.dev/), which was the next general purpose, high-level programming language after Python. And then one (general purpose, high-level) programming language followed the other, sometimes chaotically, sometimes more systematically.

I'm aware of other (micro-)benchmarks like these for example:

- https://github.com/jinyus/related_post_gen

- https://programming-language-benchmarks.vercel.app/

- https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html

- https://freedium.cfd/https://medium.com/@codeperfect/we-tested-7-languages-under-extreme-load-and-only-one-didnt-crash-it-wasn-t-what-we-expected-67f84c79dc34

- https://eklausmeier.goip.de/blog/2023/03-25-performance-comparison-c-vs-java-vs-javascript-vs-php-vs-python-vs-cobol-dart/

- and also the **TIOBE Index** for popularity: https://www.tiobe.com/tiobe-index/

To some extent they also have been a source of inspiration to test new languages, but not the only ones.

<br/>

#### Parameters for the LCG (linear congruential generator)

Originally I tumbled into this web page: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html from where I've chosen modul p = 2^16 - 15 = 	65521 and which I usually name **m** or **M** in my programs. Together with multiplicator a = 17364, usually named **a** or **A** in my programs.

Parameter _c_ is 0 with this algorithm, which is named _b_ in the web page linked above. I've already forgotten why I changed the name from _b_ to _c_.

Here's the original source of this LCG: https://www.jstor.org/stable/2585108

**Tables of Linear Congruential Generators of Different Sizes and Good Lattice Structure**, Pierre L'Ecuyer, Mathematics of Computation, Vol. 68, No. 225 (Jan., 1999), pp. 249-260 (12 pages), Published By: American Mathematical Society -- DOI: 10.1090/S0025-5718-99-00996-5

<br/>

The choice of 2^16 - 15 = 65521 was easy for me because it's the highest 16-bit modulus where the whole, recursive calculation can fit into 32 bits. This makes this pseudo-random number generator easy to port from one platform to another.

<br/>

The program is too short to explicitly test important aspects of general purpose, high-level programming languages, like:

- object orientation and

- concurreny.

Though, in one instance I made a derivative program of the "speed part" in Go to see how concurrency works there. This was rather easy as advertised and it works. However, I've no intention to do this with other programming languages with the exception of Chapel (https://chapel-lang.org/) maybe, where I _accidentally_ tumbled into its _foreach_ loop: https://chapel-lang.org/docs/technotes/foreach.html !

Though many general purpose, high-level programming languages have been designed in the last 20 years or so from scratch to explicitely facilitate programming concurrency safely and conveniently, like for example Clojure, Go, Julia, Mojo, Rust, Scala, Swift, V and so on.


#### On including lots of comments in my source code

(TBD)


#### On functional programming

This project was the first time I deliberately came into contact with **functional programming languages**. At one point it made me to divide the programs into two sections:

- imperative programming languages (I've not seen yet an Object-Oriented Programming (OOP) language which is not imperative "by nature")

- functional programming languages, from "soft" like Common Lisp (https://lisp-lang.org/) to "harsh" or "pure" like they call, here Roc: https://www.roc-lang.org/

#### On vibe coding

(TBD)

#### On configuring building and execution environments

(TBD)

#### The 1 second execution time limit

(TBD)


#### Error handling, exception handling

(TBD)



##_end
