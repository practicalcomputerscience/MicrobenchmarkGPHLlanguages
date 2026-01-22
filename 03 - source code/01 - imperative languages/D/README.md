# D

https://dlang.org/

https://gdcproject.org/

DUB, the D package registry: https://code.dlang.org/

<br/>

D is one of the first languages, since 1999, with the aim to overcome the deficits of C++: 

2020: _Origins of the D Programming Language_

https://dl.acm.org/doi/abs/10.1145/3386323

by: WALTER BRIGHT, The D Language Foundation, USA; ANDREI ALEXANDRESCU, The D Language Foundation, USA; MICHAEL PARKER, The D Language Foundation, USA

> As its name suggests, the initial motivation for the D programming language was to improve on C and C++ while keeping their spirit. The D language was to preserve the efficiency, low-level access, and Algol-style
syntax of those languages. The areas D set out to improve focused initially on rapid development, convenience, and simplifying the syntax without hampering expressiveness.

However, one could argue that something similar to Java and its contenders happened here too: [Old computer programming languages learning new tricks](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#old-computer-programming-languages-learning-new-tricks)

---

### Installing the GDC (GCC-based D compiler)

You have the choice of three compilers (in Linux and architectures i386, amd64):

- DMD ("Digital Mars D compiler"): Official reference compiler
- GDC: GCC-based D compiler
- LDC: LLVM-based D compiler

On "Which compiler should I use?" this answer is given: "For beginners, DMD is the recommended choice, as it is the implementation closest to the D Language Specification." from: https://wiki.dlang.org/Compilers

However: **"GDC and LDC both generate substantially faster binaries than DMD."**  This is true according to my own tests.

So, I'll start with GDC after my experience that g++ v.13.3.0 compiled a faster executable than Homebrew clang 21.1.7: [C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B#c). I installed it like this:

```
($ sudo apt install zlib1g=1:1.3.dfsg-3.1ubuntu2
# this version was needed in my system; just as a tip here when version conflicts like this exist)
$
$ sudo apt install gdc
...
$ gdc --version
gdc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$
```

<br/>

### Garbage collection

Same like [Nim](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Nim#nims-memory-management), D by default is a garbage-collected programming language.

With the help of "Big AI", I had a short look into the possibilities for source code which is disabling the garbage collection, while retaining the _gdc_ compiler, so, writing something like this:

```
...
@nogc int main() {
    ...
    int* x = cast(int*) malloc(int.sizeof * END);
    ...  // and many more constructs like this memory allocation
    free(x);
}
```

However, this kind of re-writing would be a major effort, since only for example function call _std.random.uniform(1, m, rnd)_ is a _non-@nogc function_, and thus must be replaced.

Doing such re-writing would lead to massively low-level, non-idiomatic source code, something I generally refrain from in this project.

<br/>

### On how to do demanding string building in D

Same like with [Nim](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Nim#on-how-to-do-demanding-string-building-in-nim), I had somehow higher expectations for the execution speed of my microbenchmark program in a D implementation.

The first times with around 35 milliseconds that came in, already compiled with _$ gdc -O3_ for production and doing simple string concatenation, were not bad, but a bit muted compared to my expectations.

So, I started to experiment again to find potentially better tactics for efficient string concatenation.

Here's a table with indicative execution times from only one run to just get an overview:

test # | construct | compiler switches | exe time in milliseconds | comment
--- | --- | --- | --- | ---
1 | _char[] bits_x; bits_x ~= bits_x_str;_ | _-O3_ | ~35 | simple string concatenation
2 | _char[] bits_x; size_t pos_x; auto result_x = copy(bits_x_str, bits_x[pos_x .. $]); pos_x += 16; string bits_x_str_total = bits_x.idup;_ | _-O3_ | ~33 | using D's range copying: _std.algorithm.mutation : copy;_
3 | _auto bits_x = appender!string(); bits_x.put(bits_x_str); string bits_x_str_total = bits_x.data;_ | _-O3_ | ~32 | using D's string builder: _std.array : appender;_

Same like with Nim (and C), also with D it may be probable that replacing built-in function _format("%016b", x[i])_ with a user defined function would lower the execution time.

Using D's string builder ("appender") tallies a slightly lower execution time than using D's range copying ("copy"): 32.8 versus 33.3 milliseconds (mean; running _$ sudo perf stat -r 20 ..._)

Both tactics are anyway not far away from simple string concatenation with the _~=_ operator with 36.0 milliseconds (mean; running _$ sudo perf stat -r 20 ..._).

Using the appender is also a little bit less verbose than range copying, since no position counter is needed. So, using the appender is my official solution here.

<br/>

### Why is D still not very popular?

Same like at [Nim](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Nim#why-is-nim-still-not-very-popular), one could ask the same question here.

However, there's a big difference to Nim: D is a U.S. based programming language, which should have made it easier to grow its ecosystem fast, at least theoretically from my point of view.

However 2: both languages are only #51 to #100 languages in the TIOBE Index as of January 2026: https://www.tiobe.com/tiobe-index/

After reading a number of posts, I came to the conclusion that D was never too far away from C++ to really differentiate itself positively. One could claim too, that also D is missing an "elevator pitch selling point"; see at Nim at: [Potential "C successors": which one to take?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/85%20-%20C%20successors#potential-c-successors-which-one-to-take)

Even though only a [bottom answer](https://softwareengineering.stackexchange.com/a/232739) (from 2014), I found there an interesting argument, one that D shares with Nim: until this day, both languages have remained "one, single, vendor" ecosystems, where I should add: "one, **small**, single, vendor" ecosystems.

<br/>

##_end
