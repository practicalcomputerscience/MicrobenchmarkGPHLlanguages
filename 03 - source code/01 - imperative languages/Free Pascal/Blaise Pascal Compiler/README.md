# Blaise Pascal Compiler

https://github.com/graemeg/blaise

ARC = Automatic Reference Counting, as a memory management method to automatically manage the allocation and deallocation of objects. It tracks the number of strong references to each object and deallocates them when there are no more references, helping to prevent memory leaks: https://en.wikipedia.org/wiki/Automatic_Reference_Counting

---

Table of contents:

- [Installation tips](#installation-tips)
- [Microbenchmark program: speed part](#microbenchmark-program-speed-part)
- [Compiling for the QBE compiler backend](#compiling-for-the-qbe-compiler-backend)
- [Compiling and building via the QBE compiler backend automatically](#compiling-and-building-via-the-qbe-compiler-backend-automatically)
- [Full microbenchmark program](#full-microbenchmark-program)

<br/>

---

## Installation tips

I downloaded this tarball file with the pre-compiled binary _blaise-v0.13.0-linux-x86_64.tar.gz_ from here: https://github.com/graemeg/blaise/releases/tag/v0.13.0

I unpacked that tarball file and expanded my _~/.bashrc_ configuration file like this (activate it after changes with _$ source ~/.bashrc_):

```
export BLAISE_RTL_SRC="$HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/rtl-src"
export PATH="$HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64:$PATH"
```

Building the Blaise Pascal Compiler from sources takes more effort, because this compiler is using [PasBuild](https://github.com/graemeg/pasbuild#pasbuild), which needs a working Free Pascal Compiler,
if you don't install PasBuild's pre-compiled binary: https://github.com/graemeg/pasbuild#6-requirements (I didn't do it).

PasBuild is from the same author, Graeme Geldenhuys, as the Blaise Pascal Compiler.

<br/>

Building and running the _hello.pas_ example worked like this:

```
$ sudo mkdir /rtl  # do this only once
$ sudo chmod 777 /rtl  # do this only once
$ blaise --source ./blaise-v0.13.0-linux-x86_64/hello.pas --output hello
$ ./hello
Hello from Blaise!
$
```

<br/>

## Microbenchmark program: speed part

The original [random_streams_for_perf_stats.pp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats.pp) program for Object Free Pascal needed **significant amount of refactoring** to make it work for the Blaise Pascal Compiler,
because almost the whole established type system of Free Pascal is gone at this compiler!

So, I practically re-developed the microbenchmark program(s) from the ground up with lots of help from Google AI: [random_streams_for_perf_stats_blaise.pp](./random_streams_for_perf_stats_blaise.pp)

Building and running a standalone executable for Linux (still) takes some effort as these commands show:

```
$ sudo mkdir /rtl  # do this only once if not done yet
$ sudo chmod 777 /rtl  # do this only once if not done yet
$ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
--linker external \
--source random_streams_for_perf_stats_blaise.pp \
--output random_streams_for_perf_stats_blaise
$ time ./random_streams_for_perf_stats_blaise 

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.041s
...
$
```

It's essential to check all paths in your system like this for example: _--unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src_

There may be a more elegant way to make a standalone executable for Linux, but that has become my final workflow version with lots of help from Google AI again.

By the way: program [random_streams_for_perf_stats_blaise.pp](./random_streams_for_perf_stats_blaise.pp) makes use of the _TStringBuilder_ type for the big strings _bits_x_ and _bits_hex_,
where the individual, little strings only have to be appended:

```
    ...
    Integer_to_bin_string(x[i], bits_x_str);
    bits_x.Append(bits_x_str);

    Integer_to_hex_string(x[i], bits_hex_str);
    bits_hex.Append(bits_hex_str);
    ...
```

Changing them to the _String_ type with memory pre-allocation doesn't change the execution speed of the program in any statistically relevant way:

```
  ...
  SetLength(bits_x, (upper_limit - 1) * 16);  // memory pre-allocation
  SetLength(bits_hex, (upper_limit - 1) * 4);  // memory pre-allocation
  
  SetLength(bits_x_str, 16);  // memory pre-allocation
  SetLength(bits_hex_str, 4);  // memory pre-allocation

  WritePosBin := 0;
  WritePosHex := 0;
  ...
  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;

    Integer_to_bin_string(x[i], bits_x_str);
    for FD := 0 to 15 do
      bits_x[WritePosBin + FD] := bits_x_str[FD];
    WritePosBin := WritePosBin + 16;

    Integer_to_hex_string(x[i], bits_hex_str);
    for FD := 0 to 3 do
      bits_hex[WritePosHex + FD] := bits_hex_str[FD];     
    WritePosHex := WritePosHex + 4;
  end;
  ...
```

However and whenever possible, do memory pre-allocation with the inbuilt _SetLength()_ procedure at least for variables of the _String_ type when writing code for the Blaise Pascal Compiler.
According to my experiments, it's generally beneficial for the execution speed of an executable.

<br/>

## Compiling for the QBE compiler backend

https://c9x.me/compile/

With a program execution time of around 42 milliseconds, the Blaise Pascal Compiler built executable cannot impress executables built by Object Free Pascal or Free Pascal, Unleashed: [Microbenchmark program: speed part in different Pascal dialects and compiler modes](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal#microbenchmark-program-speed-part-in-different-pascal-dialects-and-compiler-modes)

However, the Blaise Pascal Compiler offers a second option, and that is to compile (compliant) Pascal source code into the Intermediate Language (IL or IR for Intermediate Representation) of the QBE compiler backend:

```
$ blaise 
...
Flags:
  ...
  --emit-ir         Print QBE IR to stdout and exit
  ...
$
```

From the [QBE Intermediate Language](https://c9x.me/compile/doc/il.html#Basic-Concepts) page:

> The intermediate language (IL) is a higher-level language than the machine's assembly language. It smoothes most of the irregularities of the underlying hardware and allows an infinite number of temporaries to be used. This higher abstraction level lets frontend programmers focus on language design issues.

<br/>

First step now is to change the first compilation command to this command, also to check if compiling to a new target still works with unmodified source code:

```
$ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
--source random_streams_for_perf_stats_blaise.pp \
--emit-ir > random_streams_for_perf_stats.qbe
$ head -n 5 random_streams_for_perf_stats.qbe
# Unit: Generics.Defaults

data $__cn_TObject = { w -1, w 7, w 7, b "TObject", b 0 }
export data $typeinfo_TObject = { l 0, l 0, l $__cn_TObject + 12, l 0, l 8, l $_FieldCleanup_TObject, l $vtable_TObject, l 0, l 0 }
data $__cn_TCustomAttribute = { w -1, w 16, w 16, b "TCustomAttribute", b 0 }
$
```

<br/>

Next step is to build, install and test the QBE compiler backend, which I did like this:

```
$ git clone git://c9x.me/qbe.git
Cloning into 'qbe'...
...
Resolving deltas: 100% (4282/4282), done.
$ cd qbe
$ make  # compiling
...
$ sudo make install
[sudo] password for ...
mkdir -p "/usr/local/bin"
install -m755 qbe "/usr/local/bin/qbe"
$ cd ..  # don't forget to return into your working directory!
$ qbe -h
qbe [OPTIONS] {file.ssa, -}
	-h          prints this help
	-o file     output to file
	-t <target> generate for a target among:
	            amd64_sysv (default), amd64_apple, amd64_win, arm64, arm64_apple, rv64
	-d <flags>  dump debug information
$
```

<br/>

As the second step we can hopefully now compile the source code in the QBE IL (_~.qbe_) into assembly code (_~.s_):

```
$ qbe -o random_streams_for_perf_stats.s random_streams_for_perf_stats.qbe
$ head -n 9 random_streams_for_perf_stats.s
.data
.balign 8
__cn_TObject:
	.int 4294967295
	.int 7
	.int 7
	.ascii "TObject"
	.byte 0
/* end data */
$
```

Now comes the crucial step: compiling the assembly code into object code (_~.o_) with the usual GNU C compiler (here in version 14.2.0: _$ gcc --version_), or practically any other C compiler:

```
$ gcc -c random_streams_for_perf_stats.s -o random_streams_for_perf_stats.o
$
```

I also tried the clang compiler (in Ubuntu clang version 23.0.0) to just generate an alternative object file:

```
$ clang -c random_streams_for_perf_stats.s -o random_streams_for_perf_stats_clang.o
$
```

By the way: if you habe problems with the clang compiler, you may have a look at page [LLVM compiler infrastructure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/25%20-%20LLVM%20compiler%20infrastructure/README.md#llvm-compiler-infrastructure).

<br/>

#### Linking the object file of the application with the runtime library of the Blaise Pascal Compiler

Before we can go on with application building, we have to bring the Blaise Pascal Compiler's runtime library (RTL) into a linkable state.

That means that we first have to compile the sources of the RTL:

- download tarball file _blaise-0.13.0.tar.gz_ at link _Source code (tar.gz)_ at the bottom of this page: https://github.com/graemeg/blaise/releases/tag/v0.13.0
- extract it and thus create new directory _./blaise-0.13.0_
- change into the scripts directory: _$ cd ./blaise-0.13.0/scripts_

Then I ran the RTL object building script like this with the aim to generate linkable RTL object files into new directory _./blaise-0.13.0/rtl_objects_:

```
$ ./build-rtl-objects.sh $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/blaise ../rtl_objects
../rtl_objects/rtl.platform.o
../rtl_objects/runtime.atomic.o
../rtl_objects/runtime.setjmp.o
../rtl_objects/runtime.utf8.o
../rtl_objects/runtime.mem.o
../rtl_objects/runtime.str.o
../rtl_objects/runtime.set.o
../rtl_objects/runtime.arc.o
../rtl_objects/runtime.weak.o
../rtl_objects/runtime.float.o
../rtl_objects/runtime.thread.o
../rtl_objects/runtime.exc.o
../rtl_objects/runtime.errno.linux.o
../rtl_objects/rtl.platform.layout.linux.o
../rtl_objects/rtl.platform.posix.o
$
```

So, in directory (as in my system) _$HOME/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects_ we should see many new object files:

```
$ ls -1 $HOME/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/*.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.layout.linux.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.posix.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.arc.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.atomic.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.errno.linux.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.exc.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.float.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.mem.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.setjmp.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.set.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.start.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.str.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.thread.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.utf8.o
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.weak.o
$
```

<br/>

Now comes the crucial step, that is linking the app, where I took **a suitable sublist** of above object file list to build the monster command as shown below.
I noticed that you cannot just take the complete list of generate RTL object files for convenience, because linking is not working then.

Do not forget to return to your working directory first:

```
$ cd ../..  # return to working directory
$ gcc -o random_streams_for_perf_stats_qbe \
random_streams_for_perf_stats.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.layout.linux.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.posix.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.arc.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.atomic.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.exc.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.float.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.mem.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.setjmp.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.str.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.utf8.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/runtime.weak.o
$
```

No linking error! So let's run the application and time measure it:

```
$ time ./random_streams_for_perf_stats_qbe

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.029s
...
$
```

29 milliseconds is about 30% less execution time than building with the Blaise Pascal Compiler directly!

<br/>

What about the (different) application object file _random_streams_for_perf_stats_clang.o_ as generated with the clang compiler?

```
$ clang -o random_streams_for_perf_stats_qbe_clang \
random_streams_for_perf_stats_clang.o \
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/rtl_objects/rtl.platform.layout.linux.o \
...  # same list of object files as above 
$ time ./random_streams_for_perf_stats_qbe_clang

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.028s
...
$
```

(Practically) same result.

<br/>

## Compiling and building via the QBE compiler backend automatically

But wait a minute! The help page of the Blaise Pascal Compiler says this among other things:

```
$ blaise 
...
Flags:
  ...
  --backend <id>    qbe (deprecated) | native (default)
  ...
$
```

However, this compilation and building command still works:

```
$ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
--backend qbe \
--source random_streams_for_perf_stats_blaise.pp \
--output random_streams_for_perf_stats_blaise_qbe
$
```

..and the built executable shows the same execution speed as explicitly going through the QBE toolchain: 

```
$ time ./random_streams_for_perf_stats_blaise_qbe

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.028s
...
$
```

<br/>

## Full microbenchmark program

Since I've implemented the complete microbenchmark program for the Free Pascal, Unleashed compiler ([random_bitstring_and_flexible_password_generator_unleashed.pp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_bitstring_and_flexible_password_generator_unleashed.pp)) as a "soft" migration, I also did that for the Blaise Pascal Compiler, again as a "harsh" migration: [random_bitstring_and_flexible_password_generator_blaise.pp](./random_bitstring_and_flexible_password_generator_blaise.pp)

Regular expression at the Blaise Pascal Compiler aren't ready yet for showtime according to my experiments: https://github.com/graemeg/blaise/blob/master/stdlib/src/main/pascal/text.regex.pas

So, I took the conservative, string-based way:

```
...
uses StrUtils;

  ...
  if WITH_SPECIAL_CHARS then
    begin
      char_set := '';
      for i := 33 to 126 do
        char_set := char_set + Chr(i);
    end
  else
    char_set := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

    ...
    if ContainsStr(char_set, char0b) then
      begin
        pw_chars.Append(char0b);
        inc(i);
      end;
    ...
```

<br/>

##_end
