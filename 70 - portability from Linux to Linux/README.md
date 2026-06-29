2025-11-05: TBD: finally, re-check this portability list on a fresh Vanilla OS installation (for example), the target system, with a **very different, but "modern" Linux kernel version** than the testing system (that is _6.14.0-37-generic #37~24.04.1-Ubuntu_ as of 2026-02-07)

<br/>

# Portability of programs

Here's a list of programming languages where standalone executables can be compiled by default that **may** run **without any extra installations** on another, arbitrary ("basic") Linux machine.

You can have a view on the dependencies of a Linux executable with the [ldd](https://www.man7.org/linux/man-pages/man1/ldd.1.html) command to see what shared objects, that are shared libraries, including virtual ones, are required on the target Linux system: _$ ldd \<executable file name\>_

<br/>

programming language | dependencies on shared objects (shared libraries)? | dynamic executable? (ldd command) | static linking supported when building? | lower execution speed in average
-- | -- | -- | -- | --
Ada (GNAT), but with good portability | yes | yes
C | yes, but with good portability | yes | complicated, but possible theoretically
C++ | yes, but with good portability | yes | complicated, but possible theoretically
C3 | yes, but with good portability | yes
Chez Scheme (only the "speed part" tested) | yes, but with good portability when a working standalone executable can be built on the source system | yes
Common Lisp (SBCL) | yes, but with good portability | yes
Crystal | yes | yes | yes; switch _--static_ | -12%
Curry (KiCS2) | yes, but with good portability | yes
D | yes | yes | GDC: yes; LDC2: yes; using gdc switch _-static-libphobos_ to get rid off dependency on _libgphobos.so.4_ | -15% with _-static-libphobos_
Eiffel (Liberty) | yes tbd | yes
Fortran | yes tbd | yes
Gambit Scheme (only the "speed part" tested) | yes tbd | yes
Go | no | "not a dynamic executable"
Inko | yes, but with good portability | yes
Nim | yes, but with good portability | yes
OCaml | yes, but with good portability | yes
Odin | yes, but with good portability | yes
Roc | no | "statically linked"
Rust | yes, but with good portability | yes | yes, building with switch _--target=x86_64-unknown-linux-musl_ | Expect 3 to 4 times higher exe speed with switch _--target=x86_64-unknown-linux-musl_ !
Standard ML (MLton) | yes, but with good portability | yes
Swift | yes, and which usually blocks portability | yes, and always | static linking only for stdlib with switch _--static-swift-stdlib_; supports good portability | -10%
V | yes, but with good portability | yes
Zig | no | "not a dynamic executable"

<br/>

See for the Curry (KiCS2) program for example:

```
$ ldd ./random_bitstring_and_flexible_password_generator 
	linux-vdso.so.1 (0x00007ffd755e8000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007416a99ec000)
	libtinfo.so.6 => /lib/x86_64-linux-gnu/libtinfo.so.6 (0x00007416a99b8000)
	libgmp.so.10 => /lib/x86_64-linux-gnu/libgmp.so.10 (0x00007416a9934000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007416a9600000)
	/lib64/ld-linux-x86-64.so.2 (0x00007416a9ae9000)
$
```

For the "virtual ELF dynamic shared object" (ELF = Executable and Linkable Format) _linux-vdso.so.1_ read more from here: [Standalone (or self-contained) executable (which may not be portable so easily)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05b%20-%20Dart%20on%20the%20Dart%20virtual%20machine#standalone-or-self-contained-executable-which-may-not-be-portable-so-easily)

Or for the Zig program for example:

```
$ ldd random_bitstring_and_flexible_password_generator 
	not a dynamic executable
$ 
```

..and for the Roc program:

```
$ ldd random_bitstring_and_flexible_password_generator 
	statically linked
$
``` 

<br/>

As of 2026-02-07, I haven't taken extra efforts to compile to statically linked executables where potentially possible. So, above table is a "default building" table, which usually means building for dynamic linking.

As of 2026-06-29, I noticed that **practical** and complete static linking is possible at [Crystal](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Crystal#static-linking-in-crystal) and partly at [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#static-linking-in-swift).

<br/>

See from here at [Execution speed in the land of Dylan](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dylan#execution-speed-in-the-land-of-dylan): it looks like that a Linux executable with a lot of dependencies on shared objects ("dynamic linking") is also not the fastest program to execute.

<br/>

[GraalVM based](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm):

programming language | dependencies on shared objects (shared libraries)? | dynamic executable? (ldd command)
-- | -- | --
Ballerina (only the "speed part" tested) | yes | yes
Clojure (only the "speed part" tested) | yes | yes
Kotlin (only the "speed part" tested) | yes | yes
Scala (only the "speed part" tested) | yes | yes

<br/>

## This list does not contain

- program versions which are exceeding the [1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#languages-that-were-too-slow), with the exception of Curry
- program versions which depend on the Java Virtual Machine (JVM), like Clojure, Groovy, Kotlin, Scala, Ballerina and Java, where portability between operating systems, including Windows, is anyway strived for
- program versions which depend on the .NET ecosystem, like [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#installation-tips), where portability between operating systems, including Windows, is anyway strived for
- intepreted languages ("scripting languages") like Lua, Perl 5, PowerShell, Python, Ruby, Smalltalk or Tcl which usually depend on some prior language related installation
- "web programming languages" like JavaScript, TypeScript, CoffeeScript, AssemblyScript, PHP and Haxe, which usually depend on some prior language related installation when running at the "backend"
- Julia, which in one form or another needs to bring its runtime along with the original source code or along with a precompiled program for anything more demanding than "Hello, World!"

<br/>

## Other omissions from above list

- Chapel is a difficult case: [On compiler switches, portability and linking in Chapel](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#on-compiler-switches-portability-and-linking-in-chapel)
- [COBOL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/COBOL#cobol), here GnuCOBOL, which needs at least one shared library being installed (that is _libcob.so.4_)
- Dart, which isn't so easily portable to another, "foreign" Linux system; see at [Standalone (or self-contained) executable (which may not be portable so easily)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dart#standalone-or-self-contained-executable-which-may-not-be-portable-so-easily)
- [Dylan](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dylan#dylan), here Open Dylan, where a compiled application depends on numerous application specific and Open Dylan specific shared libraries: [Porting a simple Dylan application](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dylan#porting-a-simple-dylan-application)
- [FreeBASIC](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/FreeBASIC#installation-tips), which needs at least one shared library being installed for perfect execution (though the program probably will work)
- [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming#mercury): shared libraries: _libmer_std.so, libmer_rt.so, libgc.so_ - and what else? - are needed on the target system. I think its easier to install Mercury on the target system to have all needed libraries available before copying them one by one from source system to target system
- [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#installation-tips), which needs at least one shared library being installed
- [SWI Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#swi-prolog), which needs at least one shared library being installed
- [Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/README.md#size-of-executables): the Bigloo and CHICKEN executables need shared libraries being installed; the Racket executable needs Racket being installed
- while a Clozure Common Lisp ([CCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ccl)) executable is also portable, same like a Steel Bank Common Lisp [SBCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#sbcl) executable, an Embeddable Common Lisp [ECL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ecl) executable is not, because it needs to have access to at least one shared library

<br/>

##_end
