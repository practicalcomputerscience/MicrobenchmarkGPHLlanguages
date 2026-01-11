2026-01-10: work in progress; look for TBD's

# Language dependencies

After implementing the microbenchmark program in [Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin),
I tumbled over this sentence, when I started to read about [Nim](https://nim-lang.org/), another "statically typed compiled systems programming language":

> The Nim compiler needs a C compiler in order to compile software.

Then I had this question:

> How can a high-level programming language truly succeed an older high-level programming language, when it depends on it?

Thus, every time the underlying high-level programming language makes a change,
there's more or less the latent danger that the dependent high-level programming language may be up for a change too, also indirectly for the source code written in the dependent language.

Here's an interesting article on self-hosting and "dogfooding" in compiler building from 2022: [Goodbye to the C++ Implementation of Zig](https://ziglang.org/news/goodbye-cpp/#:~:text=How%20we%20used%20WebAssembly%20to%20annihilate%2080%2C000,one%2C%20written%20in%20250%2C000%20lines%20of%20Zig.)

<br/>

So, I started to compile another table, where I try to list more or less close dependencies:

dependent programming language | underlying programming language or environment | comment
--- | --- | ---
Ada | C for GCC (GNU Compiler Collection), that is the _gcc_ compiler, or LLVM, that is the _clang_ compiler frontend usually, back end compilation families | GNAT = GNU Ada Development Environment; for GCC: https://ftp.gnu.org/gnu/gcc/
C | C for GCC or LLVM, with both requiring a working C++ compiler version and having numerous other dependencies | only C++ is self-hosting nowadays, not even C anymore with GCC and LLVM (**)
C# | the C# compiler, _csc.exe_ or named _Roslyn_, is self-hosting nowadays; Microsoft's .NET Framework (CLR = Common Language Runtime) is then used to run the compiled Common Intermediate Language (CIL) code by Just-In-Time (JIT) compilation into native machine code | 
C3 | C for LLVM | 
Chapel | C++ for LLVM and clang | 
Clojure | the Clojure compiler (_compile-clj_) is written in Java; JVM (Java Virtual Machine) to run generated JVM bytecode | the Java compiler _javac_ is nowadays self-hosting, while the first Java compilers have been written in C
Common Lisp (SBCL) | self-hosting; an ANSI-compliant Common Lisp implementation is needed for compilation | https://www.sbcl.org/getting.html
Crystal | bootstrapping by using an older version of the Crystal compiler; otherwise self-hosting since 2013; LLVM is still needed; the Crystal compiler was originally written in Ruby | https://crystal-lang.org/install/from_sources/
Fortran (GNU) | C for gcc | GCC: https://ftp.gnu.org/gnu/gcc/
FreeBASIC | C for gcc | 
Gleam | Rust and Erlang (BEAM) | https://gleam.run/getting-started/installing/#installing-gleam
Go | self-hosting since 2015; the Rust compiler was originally written in C |
Inko | Rust for LLVM | 
Julia | C and C++ for gcc or LLVM, plus flisp Scheme (is it this one? https://github.com/fjames86/flisp) | [Required Build Tools and External Libraries](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/build/build.md#required-build-tools-and-external-libraries), [Design discussion and developer documentation](https://github.com/JuliaLang/julia/blob/master/JuliaSyntax/docs/src/design.md#design-discussion-and-developer-documentation)
Koka | Haskell and Stack for developing Haskell projects | [Build from Source](https://github.com/koka-lang/koka?tab=readme-ov-file#build-from-source); the Glasgow Haskell Compiler (GHC) is nowadays self-hosting, where a new version is compiled using a previous version
Kotlin | TBD; JVM (Java Virtual Machine); LLVM multiplatform | Android: source code compiles to JVM bytecode, iOS: source code compiles to ARM64 machine code via LLVM for example
Lua | implemented in pure ISO C; Lua also compiles as C++ | 
LuaJIT | C for gcc or LLVM | https://github.com/LuaJIT/LuaJIT/tree/v2.1
LunarML | Standard ML (MLton) | 
Mercury | bootstrapping with C for gcc for an initial installation; Mercury is then self-hosting for more advanced library grades | 
Mojo | C++ for the MLIR (Multi-Level Intermediate Representation) compiler framework | https://mlir.llvm.org/
OCaml | bootstrapping in a staged approach, where C has been compiled to bytecode for booting; then gradually self-hosting | https://github.com/ocaml/ocaml/tree/trunk/boot
Oz | The Mozart 2 bootstrapping process uses Scala and the simple build tool (sbt); otherwise self-hosting | [Mozart-Oz bootstrap compiler](https://github.com/mozart/mozart2/tree/master/bootcompiler#mozart-oz-bootstrap-compiler)
Perl 5 | C for gcc or clang |
Picat | C and C++ for gcc and g++, respectively |
PowerShell | script is compiled into PowerShell bytecode, which is then interpreted by the Common Language Runtime, often involving lots of overhead with PowerShell's object-heavy architecture | this doesn't explain the language: TBD C#? <<<<<<<<<<<<<<<<<
Prolog, SWI | C for gcc or clang | https://github.com/SWI-Prolog/swipl-devel
Python | TBD |
Raku | Rakudo compiler: C for a C compiler, and a Perl 5 installation; bootstrapping also includes the help of NQP ("Not Quite Perl") files; multiplatform | [Build requirements (Installing from source)](https://github.com/rakudo/rakudo/blob/main/INSTALL.md#build-requirements-installing-from-source)
Roc | Rust and later additionally Zig |
Rust | bootstrapping with a current beta release of the _rustc_ compiler; otherwise self-hosting since 2011; the Rust compiler was originally written in OCaml | [Bootstrapping the compiler](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/intro.html)
Scala | bootstrapping by using an older version of a Scala compiler; otherwise self-hosting (that is using Scala 3 resources); JVM (Java Virtual Machine) to run generated JVM bytecode, multiplatform | SJSIR = Scala JavaScript Intermediate Representation
Scheme, Bigloo | TBD | 
Scheme, Chez | bootstrapping with C for gcc or clang; otherwise self-hosting |
Scheme, CHICKEN | TBD | 
Scheme, Gambit | TBD | 
Scheme, Racket | runs on the Chez Scheme runtime system; Chez Scheme: TBD |
Standard ML (MLton) | C for gcc or clang | 
Swift | bootstrapping with C++ for LLVM and clang; otherwise self-hosting | [Swift implemented in Swift](https://github.com/swiftlang/swift/tree/main/SwiftCompilerSources#swift-implemented-in-swift); SIL = Swift Intermediate Language
V | C for gcc or clang or Tiny C Compiler (TCC) | [TCC](https://repo.or.cz/w/tinycc.git); https://download.savannah.gnu.org/releases/tinycc/
Wolfram Language | TBD | 
wren | C for gcc (in Linux) to compile wren's virtual machine (vm); uses [libuv](https://libuv.org/), like some others, for asynchronous i⁠/o | GitHub: [vm](https://github.com/wren-lang/wren/tree/main/src/vm); [wren.mk](https://github.com/wren-lang/wren/blob/93dac9132773c5bc0bbe92df5ccbff14da9d25a6/util/wren.mk)
Zig | partly self-hosting nowadays with the help of C and WebAssembly (Wasm) for operating system abstraction (while Wasm typically depends on C, C++, Rust or Go); the Zig compiler was originally written in C++ | see (*) from above

<br/>

So, by looking at above list, only these languages are "very original" ecosystems, that modern programming languages cannot do without:

- C++ (**) (but not C nowadays, where even the Tiny C Compiler looks for another C compiler when configuring it for making (in a Linux system); and that C compiler may be very well _gcc_ in this Linux system)
- C# and .NET
- Go
- Haskell
- Java
- Rust
- Scala
- ...
- ...
- ...
- TBD
- ...

<br/>

##_end


