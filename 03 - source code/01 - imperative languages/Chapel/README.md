# Chapel

https://chapel-lang.org/

https://chapel-lang.org/download/

https://github.com/chapel-lang/chapel

---

Table of contents:

- [On parallel computing with Chapel](#on-parallel-computing-with-chapel)
- [Installation tips](#installation-tips)
- [Building Chapel from sources](#building-chapel-from-sources)
- [String building with Chapel](#string-building-with-chapel)
- [On compiler switches, portability and linking in Chapel](#on-compiler-switches-portability-and-linking-in-chapel)
- [AMD Ryzen 5 9600X testing system](#tbd)
- [x86-64 microarchitecture levels](#x86-64-microarchitecture-levels)
- [Integer to string conversions in Chapel](#integer-to-string-conversions-in-chapel)

<br/>

---

## On parallel computing with Chapel

Although Chapel still has it's rough edges (see for example [here](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/70%20-%20portability%20from%20Linux%20to%20Linux#other-omissions-from-above-list)), is still not big and still under development (lot's of "... is unstable and subject to change" or similar remarks in the manuals), this language seems to be a good candidate for parallel computing for me, next to [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go).

There have been many serious attempts for integrated languages for parallel computing before: https://chapel-lang.org/blog/posts/10myths-part2/

Let's see if this one will survive. [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#mojo) in the end also falls into this category from my point of view, but the later is still riding its wave of (AI) enthusiasm.

<br/>

## Installation tips

[Chapel installation](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html) with:

```
$ brew install chapel
```

..works like a charm (in Ubuntu 24 LTS), and takes away a lot of potential hassle with LLVM and other supplementary installations.

Remove it with: _$ brew uninstall chapel_

Though, you may also have a look at the [Chapel Quickstart Instructions](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html).

<br/>

### Building Chapel from sources

On 2026-06-15, I followed the [Using Chapel in its Preferred Configuration](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html#using-chapel-in-its-preferred-configuration) instructions, because I was curious if Chapel in its latest version 2.8 and with this kind of installation can make a faster executable. Hint: it's not the case compared to older version 2.6.

First, check your [LLVM compiler infrastructure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/25%20-%20LLVM%20compiler%20infrastructure#llvm-compiler-infrastructure).

From [Chapel Prerequisites](https://chapel-lang.org/docs/usingchapel/prereqs.html#chapel-prerequisites):

> LLVM versions 14 through 21 are currently supported.

So, I switched to LLVM version 21 and checked the clang compiler version, registered it and checked the LLVM configuration menu:

```
$ /usr/lib/llvm-21/bin/clang --version
Ubuntu clang version 21.1.8 (++20251221032922+2078da43e25a-1~exp1~20251221153059.70)
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/lib/llvm-21/bin
$ sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-21 21 \
--slave /usr/bin/clang++ clang++ /usr/bin/clang++-21 \
--slave /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-21  # command stops here!!
update-alternatives: using /usr/bin/clang-21 to provide /usr/bin/clang (clang) in auto mode  # this is 
$ sudo update-alternatives --config clang
...
* 0            /usr/bin/clang-21   21        auto mode
...
$ 
```

Install the clang compiler frontend and headers if not done yet (here in Ubuntu 24):

```
$ sudo apt-get update
...
$ sudo apt-get install clang-21 llvm-21-dev libclang-common-21-dev libclang-21-dev libclang-cpp21-dev libclang-cpp21
...
$ export CHPL_LLVM=system  # tell Chapel to use this LLVM installation, that is LLVM version 21
$ printenv CHPL_LLVM
system
$
```

Then continue with:

```
$ tar xzf chapel-2.8.0.tar.gz  # unpack the source release
$ cd chapel-2.8.0
$ source util/setchplenv.bash  # use Chapel in the preferred configuration
Setting CHPL_HOME to ~/scripts/Chapel/chapel-2.8.0
Updating PATH to include ~/scripts/Chapel/chapel-2.8.0/bin/linux64-x86_64
                     and ~/scripts/Chapel/chapel-2.8.0/util
Updating MANPATH to include ~/scripts/Chapel/chapel-2.8.0/man
$
$ make  # be patient here! This may take some time.
...
$
```

Then do the first tests:

```
$ chpl examples/hello3-datapar.chpl  # compile a sample program
$ ./hello3-datapar  # run the sample program
Hello, world! (from iteration 14 of 100)
Hello, world! (from iteration 39 of 100)
...
Hello, world! (from iteration 12 of 100)
Hello, world! (from iteration 13 of 100)
$
```

<br/>

## String building with Chapel

Like with the other programming languages, also with Chapel this question comes up with my microbenchmark program: does it have a string builder or similar concept?

Answer: no

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

Spoiler alert: see at [Integer to string conversions in Chapel](#integer-to-string-conversions-in-chapel) below why above exercises do not really move the needle in terms of execution speed.

<br/>

### On compiler switches, portability and linking in Chapel

I compiled with the [LLVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/25%20-%20LLVM%20compiler%20infrastructure#llvm-compiler-infrastructure) optimization switch _--fast_ before measuring the rather slow program execution speed (as of June 2026):

```
$ chpl random_streams_for_perf_stats.chpl --fast
$
```

This switch is essential for a faster program (see at [Optimization Options](https://chapel-lang.org/docs/technotes/llvm.html#optimization-options) for example), at least with this microbenchmark program. I also played with other compiler options, see from _$ chpl --help_, but to no avail (so far).

However, the _--fast_ switch has a profound downside:

- it doesn't always build a portable executable for a different Linux system because it may **crash** there:

```
$ ./random_streams_for_perf_stats_Chapel 
Illegal instruction        (core dumped) ./random_streams_for_perf_stats_Chapel
$ 
```

Compiling, and dynamically linking, without the _--fast_ switch, like this for example: _$ chpl random_streams_for_perf_stats.chpl -o random_streams_for_perf_stats_non-fast_, allows for good portability, but builds an executable, with the Chapel compiler version 2.8 built from sources (see above), where the execution time which is more than double!

<br/>

#### AMD Ryzen 5 9600X testing system

On 2026-11-14 however, I also ran the compiled Chapel program on an AMD Ryzen 5 9600X (with an ASRock B850M-X WiFi R2.0, BIOS version 06/24/2026 mainboard and Kingston KF556C40-16 DDR5 16GiB 4800 MHz memory and Crucial 2,5" SATA, MX500 CT500MX500SSD1Z, 500GB or Intenso 2,5" SATA III, High, 120GB, SSD's) system, and booting into:

- gaming [Bazzite OS](https://bazzite.gg/) 44.20261012.0 (Fedora based; Linux kernel version: 7.2.9-ogc3.1.fc44.x86_64, file system type: btrfs),
- [Ubuntu](https://ubuntu.com/) 26.04.1 LTS (Debian based; Linux kernel version: 7.0.0-42-generic, file system type: ext4),
- [Manjaro Linux](https://manjaro.org/) 26.1.3 (Arch Linux based; Linux kernel version: 7.2.9-1-MANJARO, file system type: btrfs),
- [Vanilla OS](https://vanillaos.org/) 3.0 (Debian based; Linux kernel version: 7.1.3+deb14-amd64, file system type: btrfs)

.. and, there were no problems!

Remember, I build executables on an [Intel Core i7-11700K](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments) system.

By the way: to get the file system type of the current working directory (.), you may use this command for example:

```
$ findmnt -T . -o SOURCE,FSTYPE,TARGET
SOURCE    FSTYPE TARGET
/dev/sdb2 ext4   /
$
```

<br/>

### x86-64 microarchitecture levels

Latest now, a pattern should have emerged:

- compiling with the _--fast_ switch may have something do with using AVX-512 instructions in the generated machine code,
- and compiling without it produces a slow executable, but provides much better portability from Linux system to Linux system.

So, here's the first question: how to check if a Linux executable is using AVX-512 instructions?

Google AI was so friendly to give me this answer:

```
$ objdump -d -C ./random_streams_for_perf_stats | awk '/^[0-9a-f]+ <.*>:/{fn=$0} /(^|[^a-zA-Z0-9_])(zmm[0-9]+|k[0-7])([^a-zA-Z0-9_]|$)/{print fn}' | uniq
00000000000378e0 <dsiReallocate_chpl4>:
000000000003eac0 <createCopyingBuffer_chpl>:
000000000005b330 <chpl_gen_main>:
$
```

Executable _random_streams_for_perf_stats_ uses AVX-512 instructions in three functions, that is in:

- the main program logic at _chpl_gen_main_, and
- Chapel's internal array memory management at a Chapel Domain Map (DSI) runtime function responsible for resizing distributed or local arrays, and
- a Chapel runtime helper that allocates and initializes temporary buffers when copying array slices or bulk data.

The [objdump(1)](https://www.man7.org/linux/man-pages/man1/objdump.1.html) command is for displaying information from object files.

<br/>

Here's another analysis:

```
$ objdump -d ./random_streams_for_perf_stats  | grep -E '\bzmm[0-9]+\b|\bk[0-7]\b' | head -n 20
   37f91:	62 f3 fd 08 1f ca 05 	vpcmpnltq %xmm2,%xmm0,%k1
   37fa0:	62 f1 fd 89 fb c1    	vpsubq %xmm1,%xmm0,%xmm0{%k1}{z}
   3f02f:	62 f3 ed 48 3a c3 01 	vinserti64x4 $0x1,%ymm3,%zmm2,%zmm0
   3f036:	62 b1 fe 48 7f 04 02 	vmovdqu64 %zmm0,(%rdx,%r8,1)
   5bbcf:	62 f2 fd 48 7c d7    	vpbroadcastq %rdi,%zmm2
   5bbd5:	62 f1 fd 48 6f 1d e1 	vmovdqa64 0xb17e1(%rip),%zmm3        # 10d3c0 <_IO_stdin_used+0x3c0>
   5bbdf:	62 f1 fd 48 6f 25 17 	vmovdqa64 0xb1817(%rip),%zmm4        # 10d400 <_IO_stdin_used+0x400>
   5bbe9:	62 f1 fd 48 6f 2d 4d 	vmovdqa64 0xb184d(%rip),%zmm5        # 10d440 <_IO_stdin_used+0x440>
   5bbf3:	62 f1 fd 48 6f 35 83 	vmovdqa64 0xb1883(%rip),%zmm6        # 10d480 <_IO_stdin_used+0x480>
   5bbfd:	62 f2 fd 48 59 3d a1 	vpbroadcastq 0xb14a1(%rip),%zmm7        # 10d0a8 <_IO_stdin_used+0xa8>
   5bc11:	62 53 3d 48 25 c0 ff 	vpternlogd $0xff,%zmm8,%zmm8,%zmm8
   5bc18:	62 72 7d 48 58 0d fa 	vpbroadcastd 0xb13fa(%rip),%zmm9        # 10d01c <_IO_stdin_used+0x1c>
   5bc22:	62 72 fd 48 59 15 84 	vpbroadcastq 0xb1484(%rip),%zmm10        # 10d0b0 <_IO_stdin_used+0xb0>
   5bc30:	62 f1 cd 48 d4 c6    	vpaddq %zmm6,%zmm6,%zmm0
   5bc36:	62 f1 d5 48 d4 cd    	vpaddq %zmm5,%zmm5,%zmm1
   5bc3c:	62 71 dd 48 d4 dc    	vpaddq %zmm4,%zmm4,%zmm11
   5bc42:	62 71 e5 48 d4 e3    	vpaddq %zmm3,%zmm3,%zmm12
   5bc48:	62 51 ed 48 d4 ec    	vpaddq %zmm12,%zmm2,%zmm13
   5bc4e:	62 51 ed 48 d4 f3    	vpaddq %zmm11,%zmm2,%zmm14
   5bc54:	62 71 ed 48 d4 f9    	vpaddq %zmm1,%zmm2,%zmm15
$
```

This output show:

- EVEX Prefix (62 ...): every instruction shown begins with the byte 0x62, which is the hardware encoding prefix exclusive to AVX-512
- 512-bit ZMM Registers: instructions like vmovdqa64, vpbroadcastq, vpaddq, and vpternlogd are operating directly on full 512-bit %zmm registers (%zmm0 through %zmm15)
- AVX-512VL & Opmask Registers: the first two lines (vpcmpnltq and vpsubq) use 128-bit %xmm registers combined with an opmask register (%k1) and zero-masking ({z}). This specifically indicates the AVX-512VL (Vector Length Extensions) subset

<br/>

In order to provide good portability from one Linux system to another, Google AI suggested compiling with activated switches _--ccflags="-march=x86-64-v3 -mno-avx512f"_:

```
$ chpl --fast --ccflags="-march=x86-64-v3 -mno-avx512f" random_streams_for_perf_stats.chpl -o random_streams_for_perf_stats_no-avx512f
$ 
```

Let's run the last _objdump_ command on the differently compiled executable:

```
$ objdump -d ./random_streams_for_perf_stats_no-avx512f | grep -E '\bzmm[0-9]+\b|\bk[0-7]\b' | head -n 20
$
```

No more AVX-512 instructions are in use apparently.

<br/>

The _-march=x86-64-v3_ compilation switch makes sure that a broad set of modern microarchitectures is supported, while switch _-mno-avx512f_ assures that no AVX-512 instructions are used for good portability.
However, after some playing with these two compiler switches I found out that switch _--ccflags="-march=x86-64-v3"_ alone is already sufficient to avoid AVX-512 instructions. Nevertheless, I recommend to use both to be on the very safe side.

Chapel computes the "architecture type of the current machine", if the _CHPL_HOST_ARCH_ environment variable is not (actively) set, and it should yield result "x86_64" in case of my testing system:

```
$ uname -m
x86_64
$ 
```

For more details, see official documentation chapters [CHPL_HOST_ARCH](https://chapel-lang.org/docs/usingchapel/chplenv.html#chpl-host-arch) and [CHPL_TARGET_ARCH](https://chapel-lang.org/docs/usingchapel/chplenv.html#chpl-target-arch).

So, using switch _-march=x86-64-v3_ makes sense to not only compile for the specific microarchitecture of the testing system, that is the current machine, in order to facilitate good portability.

<br/>

_--ccflags_ are to be understood as "Back-end C compiler flags" as seen from output of command: _$ chpl --help_

<br/>

The x86-64-v3 feature level is explained in detail at: [x86-64-v3](https://x86-64.net/v3/)

> Adds AVX, AVX2, FMA, BMI1, BMI2, F16C, LZCNT and MOVBE, the feature set of Intel’s Haswell (2013) and AMD’s Excavator (2015). Distributions are now moving their baseline to it.
>
> x86-64-v3 brings 256-bit SIMD: AVX for floating point and AVX2 for integers, fused multiply-add, and the BMI1 and BMI2 bit-manipulation instructions. It also requires the operating system to have enabled the AVX register state (OSXSAVE).

<br/>

However, another question emerges now: why is this new program with an execution time of about 246 milliseconds practically as fast as the old one with AVX-512 support?

To answer this question, (also) the differently compiled executable could be profiled like this for example (as suggested by Google AI):

```
$ sudo perf record -F 4000 -e cycles:u -- ./random_streams_for_perf_stats_no-avx512f \
# sample cpu instruction pointer 4k times per sec; measure user-space cpu clock cycles specifically, excluding Linux kernel overhead

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
[ perf record: Woken up 1 times to write data ]
[ perf record: Captured and wrote 0.053 MB perf.data (1315 samples) ]
$ sudo perf report --stdio --sort=symbol | head -n 50
...
# Overhead  Symbol                                   IPC   [IPC Coverage]
# ........  .......................................  ....................
#
    25.79%  [.] __memset_evex_unaligned_erms         -      -            
    22.04%  [.] qt_scheduler_get_thread              -      -            
     4.14%  [.] ifree                                -      -            
     3.77%  [.] chpl_je_mallocx                      -      -            
     3.07%  [.] doClear_chpl2                        -      -            
     2.11%  [.] writer_chpl                          -      -            
     2.11%  [.] qbuffer_append                       -      -            
     1.96%  [.] _buffered_get_memory_file_lock_held  -      -            
     1.64%  [.] doClear_chpl3                        -      -            
     1.48%  [.] chpl_do_format_read                  -      -            
     1.47%  [.] _qio_channel_final_flush_unlocked    -      -            
     1.40%  [.] chpl_do_format                       -      -            
     1.40%  [.] init_chpl111                         -      -            
     1.23%  [.] _qio_channel_init_file               -      -            
     1.22%  [.] chpl_je_dallocx                      -      -            
     0.99%  [.] qbuffer_iter_advance                 -      -            
     0.97%  [.] chpl_localeID_to_locale              -      -            
     0.91%  [.] init_chpl109                         -      -            
     0.82%  [.] arena_run_first_best_fit             -      -            
     0.82%  [.] chpl_gen_main                        -      -            
     ...
     0.41%  [.] arena_avail_insert                   -      -   
$
```

In other words: less than 1% of the cpu cycles are spent on the actual random number generation at _chpl_gen_main_!

<br/>

By the way: why did Intel drop AVX-512 support from the their desktop and mobile cpu's (but not their server cpu's)?

Because of Efficiency cores:

> Intel said it wasn't enabled due to the inclusion of two different architectures. The E cores didn't support it, even if the P cores did.

from [Intel kills Alder Lake AVX-512 support for good](https://www.pcgamer.com/intel-kills-alder-lake-avx-512-support-for-good/), Published 7 March 2022

<br/>

### Integer to string conversions in Chapel

After a while, Google AI got this idea to make a faster executable:

> In Chapel, calling "%016bu".format(x[i]) and "%04xu".format(x[i]) 62,500 times each (125,000 calls total) does not just format a few bytes in registers—every single .format() call in Chapel spins up an internal in-memory QIO channel (_buffered_get_memory_file_lock_held), parses the format string at runtime (chpl_do_format_read), allocates a heap string via jemalloc, and copies it out.

So, I implemented these two common user defined functions as suggested by Google AI: 

```
...
use CTypes;
...
const HEX_CHARS = "0123456789abcdef";

inline proc integer_to_bin_string(val: int): string {
  var buf: c_array(uint(8), 16);
  for param b in 0..15 do
    buf[b] = (48 + ((val >> (15 - b)) & 1)): uint(8);
  return try! string.createCopyingBuffer(c_ptrTo(buf[0]), 16, 16);
}

inline proc integer_to_hex_string(val: int): string {
  var buf: c_array(uint(8), 4);
  for param h in 0..3 do
    buf[h] = HEX_CHARS.byte((val >> ((3 - h) * 4)) & 0xf);
  return try! string.createCopyingBuffer(c_ptrTo(buf[0]), 4, 4);
}
...
```

..and Bang! This alone made a 15 times faster executable:

```
$ time ./random_streams_for_perf_stats_user_defined_functions_no-avx512f

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.015s
...
$
```

..which can be further accelerated by running it with only 1 Chapel task thread:

```
$ time CHPL_RT_NUM_THREADS_PER_LOCALE=1 ./random_streams_for_perf_stats_user_defined_functions_no-avx512f

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.012s
...
$
```

<br/>

Same like with C ([Keeping using idiomatic constructs](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#keeping-using-idiomatic-constructs)):

> However, replacing the _sprintf_ function in my C program would not serve the goals of this microbenchmarking well in my opinion. I try to use idiomatic constructs as long as they make sense and thus forego savings in speed or space here and there.
>
> I try to use user-defined functions only in cases where I cannot find a suitable solution from more or less official libraries, or when I'm not able to tap into 3rd party libraries within a reasonable amount of time...

..I'm consequently sticking with the official [FormattedIO](https://chapel-lang.org/docs/modules/standard/IO/FormattedIO.html#formattedio) solution from Chapel's Standard Library.

<br/>

Nevertheless, this case is another demonstration of how much influence inbuilt functions of a language can have on program execution speeds.

<br/>

##_end
