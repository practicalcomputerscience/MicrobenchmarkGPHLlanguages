2025-07-16: starting this heavy work in progress

(this page may go to a GitHub Page of this account)

To-do:
- Wiki page
- test: exhausting the generated random bitstream when user asks for a super-long password

<br/>

# Microbenchmark: a simple pseudo-random number generator with a user dialog for password generation in 25+ general purpose, high-level programming languages

Starting in March 2025, this is a project which has its origin in just implementing a simple pseudo-random number generator, here a Linear Congruential Generator (LCG), in my usual programming language Python.

It's core algorithm, which I call the "masterloop", is this in Python:

```
x[i] = (a*x[i-1] + c) % m
```

Here's the "masterloop" in Python:

(TBD)

This is the first part of the program, I call it the "speed part" because only here I measure the execution speed of a program, be it compiled or interpreted.

Then I check the quality of randomness from the random "bit stream" string at: https://mzsoltmolnar.github.io/random-bitstream-tester/

..where I manually copy and paste exactly 1,000,000 binary digits ("0","1") into its "Manual bitstream input" with the target to pass all tests with no "Error" indication. Usually this is the case.

A second string with 250,000 random hexadecimal digits is also generated as input to this program:

*ENT - A Pseudorandom Number Sequence Test Program*: https://www.fourmilab.ch/random/

..though I'm not sure what to make out of its test results.

<br/>

Out of the first random binary digits, this program - after some dialog with the user on the console - is creating a password of printable, random characters of the desired length. It's amazing what you can learn about a new programming language when implementing - or trying to implement - a little dialog with the user on the console, that is with user input from the keyboard!

With one programming language (https://www.ponylang.io/) I had to give up further development because I was not able to implement this dialog.


### In terms of execution speed this is a string concatenation benchmark

Though, it took me some programming languages to figure this out nonchalantly. 

Now when I test a new programming language, I usually still implement the naive solution first and if it feels not very speedy I start to look for a string builder or similar concept.

Often a string builder dramatically improves execution speed but not always! I try remember to the language where the naive string concatenation was not the speed bottleneck, but how to store the generated, random integer numbers ```x[i]```!

(TBD)

#### This program is not bullet proof

It checks for a minimum number of password characters of 8, but usually not for an upper limit unless in its **Ada** variant (Ada is a language which is incentivizing for things like this). Theoretically, a user could exhaust the generated random bitstream when asking for a super-long password. I've not tested this so far.

#### Motivation and orientation

After improving my Python program here and there, I thought that this a good computer program for me to test new languages.

Though only in mid of June 2025 I made further improvements to my original Python program because I learned that also Python has a concept for fast string concatenation: *StringIO* https://docs.python.org/3/library/io.html#io.StringIO

This helped to make the Python program executing faster. In other words: I still revisit old programs to improve them, not only in terms of execution speed but also in terms of number of lines of source code ("verbosity") or memory leaks after program exit, if feasible.

Though this is not meant to be project for benchmarking execution speeds, I always take a deeper look into a program when I have the feeling that it's maybe unfairly on the slow side. But benchmarking execution speeds is the easiest of all benchmarking:

_**Even though most benchmarks aren’t worth the pixels they’re printed on, people seem to like them...**_ from: https://wren.io/performance.html

True, but nevertheless they are fun and provide - across several microbenchmarks - orientation.

<br/>

## A computer program for me to test new languages

Like **Go** for example (https://go.dev/), which was the next general purpose, high-level programming language I used for implementation after Python. And then one (general purpose, high-level) programming language followed the other, sometimes chaotically, sometimes more systematically.

I'm aware of other (micro-)benchmarks like these for example:

- https://github.com/jinyus/related_post_gen

- https://programming-language-benchmarks.vercel.app/

- https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html

- https://freedium.cfd/https://medium.com/@codeperfect/we-tested-7-languages-under-extreme-load-and-only-one-didnt-crash-it-wasn-t-what-we-expected-67f84c79dc34

- https://eklausmeier.goip.de/blog/2023/03-25-performance-comparison-c-vs-java-vs-javascript-vs-php-vs-python-vs-cobol-dart/

- and also the TIOBE Index for popularity: https://www.tiobe.com/tiobe-index/

..and to some extent they have also been a source of inspiration to test new languages.

<br/>

#### Parameters for LCG (linear congruential generators)

https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

with modul p = 2^16 - 15 = 	65521, usually called **m** or **M** in my programs and multiplicator a = 17364, usually called **a** or **A** in my programs.

c is 0, called b in the webpage linked above. I've already forgotten 

Here's the original source: https://www.jstor.org/stable/2585108

**Tables of Linear Congruential Generators of Different Sizes and Good Lattice Structure**, Pierre L'Ecuyer, Mathematics of Computation, Vol. 68, No. 225 (Jan., 1999), pp. 249-260 (12 pages), Published By: American Mathematical Society -- 10.1090/S0025-5718-99-00996-5

<br/>

The choice of 2^16 - 15 = 65521 was easy for me because it's the highest 16-bit modulus where the whole, recursive calculation can fit into 32 bits.

This makes this pseudo-random number generator easy to port from one platform to another.

<br/>

The is program is too short to explicitly test important aspects of general purpose, high-level programming languages, like:

- object orientation

- concurreny.

Though, in one instance I made a short program of the "speed" part in Go to see how it works there. This was rather easy and successful.


#### On including lots of comments in my programs

(TBD)


#### On functional programming

This project was the first time I came into contact with **functional programming languages**. It made me to divide the source code of my programs into two sections:

- imperative programming languages

- functional programming languages

(TBD)


#### On vibe coding

(TBD)

#### On configuring building and execution environments

(TBD)

#### The 1 second execution time limit

(TBD)



##_end
