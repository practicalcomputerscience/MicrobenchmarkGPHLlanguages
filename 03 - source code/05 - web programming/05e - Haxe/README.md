2026-05-12: work in progress

- full microbenchmark program in Haxe
- check remaining TBD's

<br/>

# Haxe

https://haxe.org/ (*)

https://en.wikipedia.org/wiki/Haxe

(deprecated) Neko VM: https://nekovm.org/

[Haxe REPL (Read-Eval-Print-Loop on JavaScript)](https://github.com/elsassph/haxe-repl)

- FL = (Open) Flash Library: https://en.wikipedia.org/wiki/OpenFL, https://www.openfl.org/, https://lib.haxe.org/p/OpenFL/9.5.1/, version 9.5.1 as of 2026-05-12 (***)
- JAR = Java Archive (file)
- JDK = Java Development Kit
- JVM = Java Virtual Machine
- HL = HashLink, a JIT (Just-In-Time) VM: https://hashlink.haxe.org/
- VM = Virtual Machine

---

Table of contents:

- [Idea of Haxe: from development of Adobe Flash games to cross-platform development for front-end and back-end](#idea-of-haxe-from-development-of-adobe-flash-games-to-cross-platform-development-for-front-end-and-back-end)
- [Installation and compilation tips](#installation-and-compilation-tips)
- [Interpretation of Haxe source code](#interpretation-of-haxe-source-code)
- [Neko and the Neko virtual machine (NekoVM) - deprecated!](#neko-and-the-neko-virtual-machine-nekovm---deprecated)
- [The new HashLink virtual machine](#the-new-hashlink-virtual-machine)
- [Producing a native executable from C code](#producing-a-native-executable-from-c-code)
- [Transpiling to Java Virtual Machine bytecode, "Java", and JavaScript](#transpiling-to-java-virtual-machine-bytecode-java-and-javascript)
  - [Java Virtual Machine bytecode](#java-virtual-machine-bytecode)
  - [Java](#java)
  - [JavaScript](#javascript)
- [Build and install HashLink from sources with SDL2](#build-and-install-hashlink-from-sources-with-sdl2)

<br/>

---

## Idea of Haxe: from development of Adobe Flash games to cross-platform development for front-end and back-end

Haxe originated in the French Motion-Twin ActionScript 2 Compiler (MTASC), written in the
[OCaml programming language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) (from France),
and was meant to faster produce applications for the Flash Player than the original Adobe Flash ActionScript compiler:

- [History](https://haxe.org/manual/introduction-haxe-history.html)
- [Haxe Interview](https://web.archive.org/web/20151208134720/http://ncannasse.fr/blog/haxe_interview)

However, Haxe then evolved to support the **OpenFL** (Open Flash Library for 2D development) (***). So, ActionScript 3.0 code needs transpilation into Haxe code to make it usable for OpenFL: https://github.com/openfl/AS3ConversionGuide/tree/master

Later, Adobe Flash (Professional) evolved into Adobe Animate: https://www.adobe.com/products/animate.html

Today, Haxe transpiles to a decent range of target languages, and "allows access to each platform's native capabilities" (*).

#### Lists of compiler targets

Here's a list of currently supported target languages: [What is Haxe?](https://haxe.org/manual/introduction-what-is-haxe.html) (+)

Just running command _$ haxe_ will also provide a list of compiler targets, or having a view at this documentation: [Compiler Targets](https://haxe.org/documentation/introduction/compiler-targets.html)

#### Haxe VM's

Haxe also has "its own VMs ([HashLink](https://hashlink.haxe.org/) and [NekoVM](https://nekovm.org/))", and the capability to "also run in interpreted mode" (*).

"Neko" is also the name of a high-level programming language ([Frequently Asked Questions about Neko](https://nekovm.org/faq/)), see also below at [Neko and the Neko virtual machine (NekoVM) - deprecated!](#neko-and-the-neko-virtual-machine-nekovm---deprecated)

#### Type system

While Haxe basically is a statically typed programming language, Neko is dynamically typed, though Haxe also allows for dynamic typing by the developer, see at [Types: Dynamic](https://haxe.org/manual/types-dynamic.html).

Neko, same as Haxe (+), has been published as version 1.0 in 2005 (https://nekovm.org/news/), and therefore is roughly a decade older than HashLink, which has been published as version 1.0 in 2016: https://github.com/HaxeFoundation/hashlink

<br/>

## Installation and compilation tips

I followed to some extent the instructions (for Ubuntu 24) of web page [Linux Software Packages](https://haxe.org/download/linux/):

```
$ sudo apt-get update
...
$ sudo apt-get install haxe
...
$ mkdir ~/haxelib && haxelib setup ~/haxelib
haxelib repository is now ~/haxelib
$ haxe --version
4.3.3
$ neko
NekoVM 2.3.0 (c)2005-2017 Haxe Foundation
  Usage : neko <file>
$ 
```

Haxelib is the package manager for Haxe. A list of all installed libraries can be obtained with command:

```
$ haxelib list
crypto: [1.3.0]
greeter: [0.1.0]
hashlink: git [dev:~/haxelib/hashlink/git/other/haxelib/]
haxelib: [4.2.0]
hlc-compiler: [0.3.0]
hx3compat: [1.1.0]
hx4compat: [1.0.0]
hxjava: [4.2.0]
locator: [0.5.0]
refactor: [4.4.2]
sinker: [0.6.0]
$
```

#### Error messaging

Then I started to develop Haxe source code file [RandomStreamsForPerfStats.hx](./RandomStreamsForPerfStats.hx) piece by piece with the help of the interpreter:

```
$ haxe --main RandomStreamsForPerfStats --interp
RandomStreamsForPerfStats.hx:71: characters 11-12 : Missing ;
$
```

..only to notice that the error messaging of Haxe is not helpful (at all). The error was in this line of source code:

```
io:println('\nbits_x_str_total = $bits_x_str_total');  // for testing
```

..instead of:

```
Sys.println('\nbits_x_str_total = $bits_x_str_total');  // for testing
```

Well, Haxe's compiler is written in OCaml: [Consequence of the Hindley-Milner type inference: uninformative error reporting at compilation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#consequence-of-the-hindley-milner-type-inference-uninformative-error-reporting-at-compilation)

<br/>

## Interpretation of Haxe source code

I would say that the easiest starting point to test Haxe source code is to interpret it.

_$ haxe --help_ says this about interpretation with the _--interp_ compiler switch: _interpret the program using internal macro system_

So, interpretation works like this for example:

```
$ haxe --main RandomStreamsForPerfStats --interp

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

Simmilar to this command is to use the _--run_ switch to "interpret a Haxe module with command line arguments":

```
$ haxe --run RandomStreamsForPerfStats
...
$
```

Both commands practically tally the same execution time of about 220 milliseconds.

<br/>

## Neko and the Neko virtual machine (NekoVM) - deprecated!

Bytecode, which is stored into file _RandomStreamsForPerfStats.n_, for the (old) NekoVM can be produced and executed like this:

```
$ haxe --neko RandomStreamsForPerfStats.n --main RandomStreamsForPerfStats
$ neko RandomStreamsForPerfStats.n
...
$
```

Executing the Neko bytecode takes about 900 milliseconds of program execution time!

Then I tested latest NekoVM version 2.4.1 (as of 2026-05-13) from here: https://nekovm.org/download/, only to notice that this NekoVM version needs even more execution time with about 1.6 seconds!

So, I will stick with older version 2.3.0, which also comes with Ubuntu command: _$ sudo apt-get install neko_.

<br/>

However, the NekoVM is [Deprecated as of 2021-09-09](https://github.com/HaxeFoundation/neko#deprecated-as-of-2021-09-09)! 

Which leaves me these open questions: what is going to happen to the "high-level dynamically typed" [Neko Programming Language](https://nekovm.org/), and also the [NekoML high-order functional language with type inference](https://nekovm.org/doc/nekoml/)?

<br/>

## The new HashLink virtual machine

Since my Ubuntu system (already) has "unmet dependencies" (see below at [Build and install HashLink from sources with SDL2 ](#build-and-install-hashlink-from-sources-with-sdl2) for more information), I turned to the Homebrew package manager (again) to install the newer HashLink JIT VM:

```
$ brew install hashlink
...
$ hl
HL/JIT 1.15.0 (c)2015-2025 Haxe Foundation
  Usage : hl [--debug <port>] [--debug-wait] <file>
$
```

Bytecode, which is stored into file _RandomStreamsForPerfStats.hl_, for the HashLinkVM can be produced and executed like this:

```
$ haxe --hl RandomStreamsForPerfStats.hl -main RandomStreamsForPerfStats
$ time hl RandomStreamsForPerfStats.hl

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.153s
user	0m0.143s
sys	0m0.011s
$
```

Around 150 milliseconds marks a significantly faster program execution time than using the (deprecated) NekoVM!

<br/>

### Producing a native executable from C code

With the help of Google AI, I also managed to compile a native executable from C code: [HashLink/C code](https://haxe.org/manual/target-hl-c-compilation.html#hashlink/c-code)

But I didn't succeed with the Homebrew based installation.

First, I downloaded and extracted file _hashlink-1.15.tar.gz_ from here: https://github.com/HaxeFoundation/hashlink/releases/tag/1.15

So, I got a HashLink source code subdirectory named _./hashlink-1.15_ in my Haxe project directory, where the Haxe source code file _RandomStreamsForPerfStats.hx_ is located.

Then, I generated C source code file _main.c_ among 99 other files (_$ find ./out -type f | wc -l_), located in subdirectory _./out_:

```
$ haxe --main RandomStreamsForPerfStats --hl out/main.c
Code generated in out/main.c
Set -D hlgen.makefile for automatic native compilation
$
```

Switch _-D hlgen.makefile_ doesn't work here, because a couple of resources must be set manually.

The successful compilation command was then this monster:

```
$ gcc -O3 -o RandomStreamsForPerfStats -std=c11 -I out -I ./hashlink-1.15/src out/main.c \
> -L ./hashlink-1.15 -Wl,-rpath,./hashlink-1.15 -lhl
$
```

..where switch _lhl_ tells the linker to search for HashLink library file _libhl.so_, which is located in subdirectory _./hashlink-1.15_ (but unfortunately not file _libhl.a_ for static linking).

Switch _-L_ searches for more libraries in given path _./hashlink-1.15_.

Include switches _-I out -I ./hashlink-1.15/src_ search for header files in the given subdirectories.

_-Wl,-rpath,./hashlink-1.15_ passes the runtime path (rpath) flags to the linker, so the executable can locate the shared library _~.so_ at runtime without needing the value of environment variable _LD_LIBRARY_PATH_.

The native executable:

```
$ time ./RandomStreamsForPerfStats

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.142s
user	0m0.127s
sys	0m0.014s
$
```

..runs a little bit faster than the bytecode for the HashLink virtual machine.

A downside of this solution is the fact that Linux program _RandomStreamsForPerfStats_ depends on shared library _libhl.so_ (of size 741.448 bytes):

```
$ ldd RandomStreamsForPerfStats
	linux-vdso.so.1 (0x00007b29552f6000)
	libhl.so => ./hashlink-1.15/libhl.so (0x00007b29551fa000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007b2954e00000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007b29550fc000)
	/lib64/ld-linux-x86-64.so.2 (0x00007b29552f8000)
$
```

..which means that executing this program in a different location or Linux system needs this shared library always relatively stored as _./hashlink-1.15/libhl.so_.

<br/>

## Transpiling to Java Virtual Machine bytecode, "Java", and JavaScript


#### Java Virtual Machine bytecode

First, install the _hxjava_ library:

```
$ haxelib install hxjava
...
Installing hxjava...
  Current version is now 4.2.0
Done
$
```

Then check the JDK version to not run into version conflicts. If needed, change it with: _$ sudo update-alternatives --config java_

```
$ java --version
openjdk 25.0.2 2026-01-20
...
$
```

Compile a JAR file and execute it:

```
$ haxe --main RandomStreamsForPerfStats --jvm RandomStreamsForPerfStats.jar
$ time java -jar ./RandomStreamsForPerfStats.jar

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.085s
$
```

85 milliseconds of execution time is hot! 

<br/>

#### "Java"

Command _haxe --java_:

```
$ haxe --java bin --main RandomStreamsForPerfStats
haxelib run hxjava hxjava_build.txt --haxe-version 4303 --feature-level 1 --out bin/RandomStreamsForPerfStats
javac "-sourcepath" "src" "-d" "obj" "-g:none" "@cmd"
$ time java -jar ./bin/RandomStreamsForPerfStats.jar
...
real	0m0.079s
...
$ 
```

..created numerous JVM resources starting in (example) subdirectory _./bin_, that is exactly 75 files! (_$ find ./bin -type f | wc -l_), though it's apparently allowing faster program execution.

So, I ran the _multitime_ command with the usual 20 program runs to better measure execution times:

- "JVM": real mean = 81 ms, std.dev. = 0.001 ms
- "Java": real mean = 78 ms, std.dev. = 0.001 ms

Not much difference, but noticable.

<br/>

A Java source code file named _RandomStreamsForPerfStats.java_ was created in subdirectory _./bin/src/haxe/root_, though it cannot be just run with the _java_ command **without** the numerous other resources:

```
// Generated by Haxe 4.3.3
package haxe.root;

import haxe.root.*;

@SuppressWarnings(value={"rawtypes", "unchecked"})

public class RandomStreamsForPerfStats extends haxe.lang.HxObject
{
  ...
}
```

(Though, running _$ java ./bin/src/haxe/root/RandomStreamsForPerfStats.java_ works here, but takes about 1.8 seconds!)

So, Haxe itself is not capable of directly transpiling [RandomStreamsForPerfStats.hx](./RandomStreamsForPerfStats.hx) into something like _RandomStreamsForPerfStats.java_ in **one** source code file.

There's a conversion tool called [refactor](https://github.com/yar3333/haxe-refactor#refactor), which is using scripts (_$ haxelib run refactor <script>_) to basically apply a "massive search&replace in files". However, a script like _haxe_to_java_ doesn't exist there.

It's somehow interesting to see that [Source-to-source compilation](https://en.wikipedia.org/wiki/Source-to-source_compiler) with target language **Java** without creating a whole bunch of Java resources apparently became a domain of AI coding tools in recent times.

I did exactly this now with resulting Java source code file [RandomStreamsForPerfStats.java](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Java/RandomStreamsForPerfStats.java), so that nobody can say that I didn't implement the (speed part) of the microbenchmark program in Java: 😉

```
$ javac RandomStreamsForPerfStats.java
$ jar cfev RandomStreamsForPerfStats.jar RandomStreamsForPerfStats RandomStreamsForPerfStats.class
# switch e is essential, see from here: https://docs.oracle.com/javase/tutorial/deployment/jar/appman.html
$ time java -jar RandomStreamsForPerfStats.jar

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.052s
...
$
```

Well, JVM bytecode generated from an original Java source code file and packed into a very slim JAR file:

```
$ jar tf RandomStreamsForPerfStats.jar
META-INF/
META-INF/MANIFEST.MF
RandomStreamsForPerfStats.class
$ 
```

..apparently is a very efficient resource for computing.

<br/>

#### JavaScript

Just naively cross-compiling the Haxe source code to JavaScript source code, which is using a system resource:

```
import sys.io.File;
```

..isn't working:

```
$ haxe --main RandomStreamsForPerfStats --js RandomStreamsForPerfStats.js
RandomStreamsForPerfStats.hx:35: characters 8-19 : You cannot access the sys package while targeting js (for sys.io.File)
$
```

Google AI was so friendly to help me out here: the "trick" is to first install the [Extern type definitions for Node.JS](https://lib.haxe.org/p/hxnodejs/):

```
$ haxelib install hxnodejs
Downloading hxnodejs-12,2,0.zip...
Download complete: 0.24KB in 0.1s (1.3KB/s)
Download complete: 225.99KB in 0.2s (813.4KB/s)
Installing hxnodejs...
  Current version is now 12.2.0
Done
$
```

..and then to add the _-lib hxnodejs_ switch to the Haxe compilation command, so the compiler recognizes _Node.js_ core modules:

```
$ haxe -lib hxnodejs -main RandomStreamsForPerfStats --js RandomStreamsForPerfStats.js
$ time node ./RandomStreamsForPerfStats.js

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.057s
user	0m0.055s
sys	0m0.013s
$
```

This compilation command generated a [JavaScript program](./RandomStreamsForPerfStats.js)

- which is not really meant to be human readable, and
- which is with 57 milliseconds of program execution time (on Node.js v24.13.0) only a bit slower than the JavaScript version with 42 milliseconds, which has been transpiled from the Groovy and then the TypeScript source code with the help of "Big AI": [Why is the TypeScript variant slower than the equivalent JavaScript variant?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05a%20-%20web%20languages%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#why-is-the-typescript-variant-slower-than-the-equivalent-javascript-variant)

<br/>

---

### Build and install HashLink from sources with SDL2 

SDL stands for [Simple DirectMedia Layer](https://www.libsdl.org/) in its version 2, the version on which HashLink depends. SDL version 2.30.8 is the last SDL2 subversion I've found: https://github.com/libsdl-org/SDL/releases/tag/release-2.30.8 (++)

The usual command in Ubuntu to install SDL2 resources, that is _$ sudo apt install libsdl2-dev_, failed with me ("unmet dependencies") in two Ubuntu 24.04.3 LTS systems.

There are more resources needed for HashLink, but they shouldn't make so much trouble according to my experience. See from here at [Building on Linux/OSX](https://github.com/HaxeFoundation/hashlink/#building-on-linuxosx):

```
$ sudo apt update
$ sudo apt-get install libpng-dev libturbojpeg-dev libvorbis-dev libopenal-dev libglu1-mesa-dev libmbedtls-dev libuv1-dev libsqlite3-dev
...
$
```

<br/>

The next step is to build and install the SDL2 resources. I downloaded file _SDL-release-2.30.8.tar.gz_ from GitHub at (++), extracted it, and changed into its subdirectory with: _$ cd ./SDL-release-2.30.8_

There I did the common triple jump of: _$ ./configure; make; sudo make install_

 At this point, it's a good idea to check the system-wide installed SDL2 resources, if everything went well at the prior command:

```
$ sdl2-config --version
2.30.8
$ pkg-config --modversion sdl2
2.30.8
$ 
```

A simple C program for testing could hopefully now be compiled and executed without errors:

```
$ cat ./SDL2_test.c
#include <SDL.h>

int main(void){
  SDL_Init(SDL_INIT_VIDEO);
  SDL_Quit();
  return 0;
}
$ gcc $(sdl2-config --cflags) SDL2_test.c $(sdl2-config --libs) -o SDL2_test
$ ./SDL2_test
$
```

<br/>

If all above packages have been installed successfully, the next steps are to download, build from sources, install and verify HaskLink (HL):

```
$ git clone https://github.com/HaxeFoundation/hashlink.git
$ cd hashlink
$ make
...
$ sudo make install  # install the hl binary system-wide
...
$ hl  # verify
HL/JIT 1.16.0 (c)2015-2025 Haxe Foundation
  Usage : hl [--debug <port>] [--debug-wait] <file>
$
```

HL version 1.16.0 is a later version than the one of my testing system with 1.15.0 which has been installed with Homebrew; see above at [The new HashLink virtual machine](the-new-hashlink-virtual-machine).

However, I will keep the older Homebrew version as my official version, since version 1.16.0 looks like a Nightly Build Pre-release as of 2026-05-14: https://github.com/HaxeFoundation/hashlink/releases

<br/>

##_end
