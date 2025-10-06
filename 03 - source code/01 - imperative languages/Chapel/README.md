# Chapel

https://chapel-lang.org/

Although Chapel still has it's rough edges, is still not big and still under development (lot's of "... is unstable and subject to change" or similar remarks in the manuals), this language seems to be a good candidate for **parallel computing** for me, next to [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go).

There have been many serious attempts for integrated languages for parallel computing before: https://chapel-lang.org/blog/posts/10myths-part2/

Let's see if this one will survive. [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#mojo) in the end also falls into this category from my point of view, but the later is still riding its wave of enthusiasm.

---

### Installation tips

Chapel version 2.4.0 is built on LLVM version 18.1.8 (at least in my case).

In case that a LLVM installation with version 18 has become missing, it can be (re-)installed like this: https://ubuntuhandbook.org/index.php/2023/09/how-to-install-clang-17-or-16-in-ubuntu-22-04-20-04/

Apparently, this procedure also applies to other LLVM versions, like 17, 19, 20 and so on.

```
$ wget https://apt.llvm.org/llvm.sh
$ chmod u+x llvm.sh
$ sudo ./llvm.sh 18  # 18 for LLVM version 18
$ sudo mkdir -p /etc/apt/keyrings  # dir /etc/apt/keyrings doesn't exist on my system
$ sudo mv /etc/apt/trusted.gpg.d/apt.llvm.org.asc /etc/apt/keyrings/
$ sudo nano /etc/apt/sources.list.d/archive_uri-http_apt_llvm_org_noble_*.list
```

With the last command I edited this line:

_deb http://apt.llvm.org/noble/ llvm-toolchain-noble-18 main_

into:

_deb [arch=amd64 signed-by=/etc/apt/keyrings/apt.llvm.org.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-18 main_

Press keys Ctrl+S to save the changed file, and keys Ctrl+X to exit the nano editor. At last do:

```
$ sudo apt update
$ clang-18 --version  # check installation
```

### String building with Chapel

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

However and somehow to be expected, this usually cannot be a solution for a fast computer program and so, like with other [C-like](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) languages, I experimented with memory preallocations, and finally got a working solution where the public Internet is not helpful to find a solution (which is one reason why I show this solution here):

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

However and to my surprise, tactic #2 did not make a faster program in Chapel, maybe the only C-like language without a string builder concept where this tactic leads to an even slower solution:

• mundane string concatenation (#1): ~244 milliseconds execution time
• with memory preallocation (#2): ~250 milliseconds execution time

The third tactic, which I eagerly (and imperatively if possible) apply in functional programming languages (and Perl 5) without a string builder concept, is to initially declare an array of strings in the right size, fill the array elements in the masterloop with the individual, small strings and finally convert this array into one big string, is also not a faster solution in Chapel, because it's the slowest with around 258 milliseconds.

However, it looks a little bit suspicious that three very different tactics for string building lead to so close execution times.

<br/>

##_end
