Take the conclusions here with a grain of salt. In the end, all programming languages are ecosystems, where most probably some parts have been written in one or more other programming languages.

<br/>

# Language dependencies

After implementing the microbenchmark program in [Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin),
I tumbled over this sentence, when I started to read about [Nim](https://nim-lang.org/), another "statically typed compiled systems programming language":

> The Nim compiler needs a C compiler in order to compile software.

Then I had this question:

> How can a high-level programming language truly succeed an older high-level programming language, when it depends on it?

Thus, every time the underlying high-level programming language makes a change,
there's more or less the latent danger that the dependent high-level programming language may be up for a change too, also indirectly for the source code written in the dependent language.

Here's an interesting article on self-hosting and "dogfooding" in compiler building from 2022 (*): [Goodbye to the C++ Implementation of Zig](https://ziglang.org/news/goodbye-cpp/#:~:text=How%20we%20used%20WebAssembly%20to%20annihilate%2080%2C000,one%2C%20written%20in%20250%2C000%20lines%20of%20Zig.)

<br/>

So, I started to compile another table, where I try to list some main dependencies:

dependent programming language | underlying programming language or environment | comment
--- | --- | ---
Ada | C for GCC (GNU Compiler Collection), that is the _gcc_ compiler, or LLVM, that is the _clang_ compiler frontend usually, back end compilation families | GNAT = GNU Ada Development Environment; for GCC see: https://ftp.gnu.org/gnu/gcc/
C | C for GCC or LLVM, with both requiring a working C++ compiler version and having numerous other dependencies | only C++ is self-hosting nowadays, not even C anymore with GCC and LLVM (**)
C# | the C# compiler, _csc.exe_ or named _Roslyn_, is self-hosting nowadays; Microsoft's .NET Framework (CLR = Common Language Runtime) is then used to run the compiled Common Intermediate Language (CIL) code by Just-In-Time (JIT) compilation into native machine code | see at [Roslyn Compiler](https://github.com/dotnet/roslyn/tree/main/docs/compilers#roslyn-compiler)
C3 | C for LLVM | see at [Compiling on Ubuntu 24.04 LTS](https://github.com/c3lang/c3c#compiling-on-ubuntu-2404-lts)
Chapel | C++ for LLVM and clang | see at [Building From Source](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html#building-from-source)
Clojure | Clojure's core for the JVM is written exclusively in [Java](https://github.com/clojure/clojure/tree/master/src/jvm/clojure), while the [core Clojure language](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj) is then exclusively written in Clojure, so it's partly self-hosting | the Java compiler itself is completely self-hosting since the late 90ies at least, see below at [Java](#java)
Common Lisp (SBCL) | self-hosting; an ANSI-compliant Common Lisp implementation is needed for compilation | see at https://www.sbcl.org/getting.html
Crystal | bootstrapping by using an older version of the Crystal compiler; otherwise self-hosting since 2013; LLVM is still needed; the Crystal compiler was originally written in Ruby | see at https://crystal-lang.org/install/from_sources/
Fortran (GNU) | C for gcc | for GCC see: https://ftp.gnu.org/gnu/gcc/
FreeBASIC | self-hosting with the help of the [GNU Binutils](https://www.gnu.org/software/binutils/) | see at https://www.freebasic.net/
Gleam | Rust and Erlang (BEAM) | see at https://gleam.run/getting-started/installing/#installing-gleam; the Erlang compiler _erlc_ is written in Erlang, a language which started "as a modified prolog": https://www.erlang.org/faq/academic.html#idp33045264
Go | self-hosting since 2015; the Rust compiler was originally written in C | see at [Installing Go from source](https://go.dev/doc/install/source)
Inko | Rust for LLVM | see at [Installation](https://docs.inko-lang.org/manual/main/setup/installation/)
Julia | C and C++ for gcc or LLVM, plus flisp Scheme (is it this one? https://github.com/fjames86/flisp) | see at [Required Build Tools and External Libraries](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/build/build.md#required-build-tools-and-external-libraries) and [Design discussion and developer documentation](https://github.com/JuliaLang/julia/blob/master/JuliaSyntax/docs/src/design.md#design-discussion-and-developer-documentation)
Koka | Haskell and Stack for developing Haskell projects | see at [Build from Source](https://github.com/koka-lang/koka?tab=readme-ov-file#build-from-source); the Glasgow Haskell Compiler (GHC) is nowadays self-hosting; see below at [Haskell](#haskell)
Kotlin | compiler _kotlinc_ is written in a mixture of Java and Kotlin and uses a "staged, JVM‑based bootstrap pipeline: an older, trusted compiler builds the newer compiler" (MS Bing AI); LLVM multiplatform | see at [JetBrains/kotlin](https://github.com/JetBrains/kotlin/)
Lua | implemented in pure ISO C; Lua also compiles as C++ | see at https://www.lua.org/download.html
LuaJIT | C for gcc or LLVM | see at https://github.com/LuaJIT/LuaJIT/tree/v2.1
LunarML | Standard ML (MLton) | see at [Building and Installing](https://github.com/minoki/LunarML#building-and-installing)
Mercury | bootstrapping with C for gcc for an initial installation; Mercury is then self-hosting for more advanced library grades | see at [Bootstrapping a Mercury environment](https://github.com/Mercury-Language/mercury/blob/master/Documentation/README.bootstrap)
Mojo | C++ for the MLIR (Multi-Level Intermediate Representation) compiler framework | see at https://mlir.llvm.org/
OCaml | bootstrapping in a staged approach, where C has been compiled to bytecode for booting; then gradually self-hosting | see at https://github.com/ocaml/ocaml/tree/trunk/boot
Odin | C and mostly C++ for clang | see at [3.3 Others (Unix)](https://odin-lang.org/docs/install/#others-unix)
Oz | The Mozart 2 bootstrapping process uses Scala and the simple build tool (sbt); otherwise self-hosting | see at [Mozart-Oz bootstrap compiler](https://github.com/mozart/mozart2/tree/master/bootcompiler#mozart-oz-bootstrap-compiler)
Perl 5 | C for gcc or clang | see _README.linux_ in [Perl Source](https://www.cpan.org/src/README.html)
Picat | C and C++ for gcc and g++, respectively | see _README_ from sources at page [Download](https://picat-lang.org/download.html)
PowerShell | C# is mainly used for the command-line interface _pwsh_ ("PowerShell Core" to run PowerShell scripts in Linux); see from here: https://openhub.net/p/powershell/analyses/latest/languages_summary | see also for example [Program.cs](https://github.com/PowerShell/PowerShell/blob/master/src/powershell/Program.cs)
Prolog, SWI | C for gcc or clang | see at https://github.com/SWI-Prolog/swipl-devel
Python | C for gcc or clang for CPython, which is the reference implementation for Python | see for example from [README.rst](https://github.com/python/cpython/blob/main/README.rst)
Raku | Rakudo compiler: C for a C compiler, and a Perl 5 installation; bootstrapping also includes the help of NQP ("Not Quite Perl") files; multiplatform | see at [Build requirements (Installing from source)](https://github.com/rakudo/rakudo/blob/main/INSTALL.md#build-requirements-installing-from-source)
Roc | Rust and later additionally Zig | see at [Building the new Roc compiler from source](https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#building-the-new-roc-compiler-from-source)
Rust | bootstrapping with a current beta release of the _rustc_ compiler; otherwise self-hosting since 2011; the Rust compiler was originally written in OCaml | see at [Bootstrapping the compiler](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/intro.html)
Scala | bootstrapping by using an older version of a Scala compiler since 2015: [We got liftoff!](https://dotty.epfl.ch/blog/2015/10/23/dotty-compiler-bootstraps.html); otherwise self-hosting (that is using Scala 3 resources); multiplatform | SJSIR = Scala JavaScript Intermediate Representation
Scheme, Bigloo | Bigloo Scheme initially starts from a bootstrap version of it; then gradually self-hosting also with C code for gcc | see at [Bigloo Installation Notice](https://github.com/manuel-serrano/bigloo/blob/master/INSTALL.md#bigloo-installation-notice) and [configure](https://github.com/manuel-serrano/bigloo/blob/master/configure)
Scheme, Chez | bootstrapping with C for gcc or clang; otherwise self-hosting | see at [BUILDING](https://github.com/cisco/ChezScheme/blob/main/BUILDING)
Scheme, CHICKEN | C for gcc or clang | see at [Installing CHICKEN](https://wiki.call-cc.org/man/5/Getting%20started#installing-chicken)
Scheme, Gambit | C for gcc | see at [INSTALL](https://github.com/gambit/gambit/blob/master/INSTALL.txt)
Scheme, Racket | using Chez Scheme as its core compiler and runtime system | see at [1.5 Implementations](https://docs.racket-lang.org/reference/implementations.html#(part._implementations))
Standard ML (MLton) | C for gcc or clang | see at [Build and Install (from source)](https://github.com/MLton/mlton#build-and-install-from-source)
Swift | bootstrapping with C++ for LLVM and clang; otherwise self-hosting | see at [Swift implemented in Swift](https://github.com/swiftlang/swift/tree/main/SwiftCompilerSources#swift-implemented-in-swift); SIL = Swift Intermediate Language
V | C for gcc or clang or Tiny C Compiler (TCC) | see at [TCC](https://repo.or.cz/w/tinycc.git) and https://download.savannah.gnu.org/releases/tinycc/
Wolfram Language | the Wolfram System is "..written in C/C++, Java, and the Wolfram Language.." | see at [The Software Engineering of the Wolfram System](https://reference.wolfram.com/language/tutorial/TheInternalsOfTheWolframSystem.html#28134)
wren | C for gcc (in Linux) to compile wren's virtual machine (vm); uses [libuv](https://libuv.org/), like some others (like Bigloo Scheme), for asynchronous i⁠/o | see at GitHub: [vm](https://github.com/wren-lang/wren/tree/main/src/vm); [wren.mk](https://github.com/wren-lang/wren/blob/93dac9132773c5bc0bbe92df5ccbff14da9d25a6/util/wren.mk)
Zig | partly self-hosting nowadays with the help of C and WebAssembly (Wasm) for operating system abstraction with WASI (WebAssembly System Interface), where "a minimal WASI interpreter implementation that is built from C source" is provided; the Zig compiler was originally written in C++ | see (*) from above

<br/>

So, by looking at above list, these languages may be seen as "very original" languages, though their related runtime systems, if present, may often depend (still) on C and/or derivatives:

- C++ (**) (but not C nowadays, where even the Tiny C Compiler looks for another C compiler when configuring it for its making in a Linux system, and that C compiler may be very well _gcc_ in this Linux system)
- C# and .NET
- Chrystal
- Common Lisp
- Erlang
- Go
- Haskell
- Java
- Rust
- Scala

<br/>

#### Black Duck Open Hub

The Black Duck Open Hub website may provide some high-level insights into the usage of programming languages of open-source software projects, for example:

- Perl: https://openhub.net/p/perl/analyses/latest/languages_summary
- Glasgow Haskell Compiler: https://openhub.net/p/ghc/analyses/latest/languages_summary

<br/>

#### Java

One of the earlier Java compilers and started in 1998, and which later became mainstream, has already been written exclusively in Java. That was the **GJ (Generic Java) compiler** (https://homepages.inf.ed.ac.uk/wadler/gj/Distribution/#gjc), also developed by Martin Odersky among others, the main inventor of Scala: https://lampwww.epfl.ch/gj/

From [Introduction to Java](https://www.realjavaonline.com/java_introduction/introductiontojava.php):

> In 1993, Arthur Van Hoff joined Sun Microsystems. He implemented Java compiler in Java which Gosling had previously implemented in C language. 

<br/>

#### Haskell

While the Glasgow Haskell Compiler ([GHC](https://www.haskell.org/ghc/)) is self-hosting since many years ([1.1. Obtaining GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/intro.html#obtaining-ghc)), Haskell's runtime system ([RTS](https://gitlab.haskell.org/ghc/ghc/-/tree/master/rts)) is not. That is written in a mixture of mostly C and a derivative of [C--](https://www.cs.tufts.edu/~nr/c--/), which is Cmm: [GHC Commentary: What the hell is a .cmm file?](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/cmm):

<br/>

##_end


