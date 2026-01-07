# Chapel

https://chapel-lang.org/

---

Table of contents:

- [On parallel computing with Chapel](#on-parallel-computing-with-chapel)
- [Installation tips](#installation-tips)
- [String building with Chapel](#string-building-with-chapel)

<br/>

---

## On parallel computing with Chapel

Although Chapel still has it's rough edges (see for example [here](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/70%20-%20portability%20from%20Linux%20to%20Linux#other-omissions-from-above-list)), is still not big and still under development (lot's of "... is unstable and subject to change" or similar remarks in the manuals), this language seems to be a good candidate for parallel computing for me, next to [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go).

There have been many serious attempts for integrated languages for parallel computing before: https://chapel-lang.org/blog/posts/10myths-part2/

Let's see if this one will survive. [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#mojo) in the end also falls into this category from my point of view, but the later is still riding its wave of (AI) enthusiasm.

---

## Installation tips

[Chapel installation](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html) with:

```
$ brew install chapel
```

..works like a charm (in Ubuntu 24 LTS), and takes away a lot of potential hassle with LLVM and other supplementary installations.

## String building with Chapel

Like with the other programming languages, also with Chapel this question comes up with my microbenchmark program: does it have a string builder or similar concept?

Answer: no (who cares about string building in a programming language originally designed for supercomputers?)

And so I did what I always initially do in a case like this: I just concatenate one string after the other to finally make one big string in the [masterloop](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel/random_streams_for_perf_stats.chpl):

```
... // #1
var bits_x:       string = "";
...
while i < END {
  ...
  var bits_x_str = "%016bu".format(x[i]);  // 1218 --> 0000010011000010
  bits_x += bits_x_str;
  ...
  i += 1;
}
...
```

No memory preallocation for _bits_x_ or anything similar is going on here.

However and somehow to be expected, this usually cannot be a solution for a fast computer program and so, like with other [C-like](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) languages, I experimented with memory preallocations, and finally got a working solution where the public Internet is not helpful to find a solution:

```
... // #2
const END: int = 62500;
const M1: int = END * 16;
...
var cPtr_big_string = allocate(uint(8), M1);
var big_string = bytes.createAdoptingBuffer(cPtr_big_string, length=M1, size=M1+1);
...
var byte_nbr: int = 0;
var i: int = 1;

while i < END {
  x[i] = ((A * x[i-1]) + C) % M;

  var small_str = "%016bu".format(x[i]);
  var small_str_bytes = small_str.bytes();

  byte_nbr = (i-1)*16;
  cPtr_big_string[byte_nbr] = small_str_bytes[0];
  cPtr_big_string[byte_nbr+1] = small_str_bytes[1];
  cPtr_big_string[byte_nbr+2] = small_str_bytes[2];
  cPtr_big_string[byte_nbr+3] = small_str_bytes[3];
  cPtr_big_string[byte_nbr+4] = small_str_bytes[4];
  cPtr_big_string[byte_nbr+5] = small_str_bytes[5];
  cPtr_big_string[byte_nbr+6] = small_str_bytes[6];
  cPtr_big_string[byte_nbr+7] = small_str_bytes[7];
  cPtr_big_string[byte_nbr+8] = small_str_bytes[8];
  cPtr_big_string[byte_nbr+9] = small_str_bytes[9];
  cPtr_big_string[byte_nbr+10] = small_str_bytes[10];
  cPtr_big_string[byte_nbr+11] = small_str_bytes[11];
  cPtr_big_string[byte_nbr+12] = small_str_bytes[12];
  cPtr_big_string[byte_nbr+13] = small_str_bytes[13];
  cPtr_big_string[byte_nbr+14] = small_str_bytes[14];
  cPtr_big_string[byte_nbr+15] = small_str_bytes[15];

  i += 1;
}
...
```

<br/>

By the way: when experimenting with my Python program I came to the conclusion that looping over 16 assignments can be even slower; so I generally don't do it to not waste time on experiments like this.

However and to my surprise, tactic #2 did not make a faster program in Chapel, maybe the only C-like language without a string builder concept where this tactic leads to an even slower solution.

The third tactic, which I eagerly (and imperatively if possible) apply in functional programming languages (and Perl 5) without a string builder concept, is to initially declare an array of strings in the right size, fill the array elements in the masterloop with the individual, small strings and finally convert this array into one big string, is also not a faster solution in Chapel, because it's the slowest of these three solutions.

<br/>

And yes, I compiled with switch _--fast_ on before measuring the rather slow program execution speed:

```
chpl random_streams_for_perf_stats.chpl --fast
```

This switch is essential for a faster program, at least with this microbenchmark program. I also played with other compiler options, see from _$ chpl --help_, but to no avail.

<br/>

##_end
