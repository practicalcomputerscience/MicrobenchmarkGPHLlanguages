2026-01-10: work in progress; experimental

# Language dependencies

After implementing the microbenchmark program in [Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin),
I tumbled over this sentence, when I started to read about [Nim](https://nim-lang.org/), another "statically typed compiled systems programming language":

> The Nim compiler needs a C compiler in order to compile software.

Then I had this question:

> How can a high-level programming language truly succeed an older high-level programming language, when it depends on it?

Thus, every time the underlying high-level programming language makes a change,
there's more or less the latent danger that the dependent high-level programming language may be up for a change too, also indirectly for the source code written in the dependent language.

Other sources: TBD

<br/>

So, I started to compile another table, where I try to list more or less close dependencies:

dependent programming language | underlying programming language or environment | comment
--- | --- | ---
Ada | C for GCC (GNU Compiler Collection) or LLVM back end compilation families | GNAT = GNU Ada Development Environment 
C | C for GCC or LLVM, with both requiring a working C++ compiler and having numerous other dependencies | only C++ is self-hosting nowadays, not even C anymore with GCC and LLVM
C# | the C# compiler, _csc.exe_ or named _Roslyn_, is self-hosting nowadays; Microsoft's .NET Framework (CLR = Common Language Runtime) is then used to run the compiled and efficient Common Intermediate Language (CIL) code by Just-In-Time (JIT) compilation into native machine code | 
C3 | C for LLVM | 
Chapel | C++ for LLVM and clang | 
Clojure | the Clojure compiler (_compile-clj_) is written in Java; the JVM (Java Virtual Machine) to run JVM bytecode | 
Common Lisp (SBCL) | self-hosting; an ANSI-compliant Common Lisp implementation is needed for compilation | https://www.sbcl.org/getting.html
Crystal | self-hosting since 2013; LLVM still needed; the Crystal compiler was written originally in Ruby | https://crystal-lang.org/install/from_sources/
Fortran (GNU) | C? for GCC | TBD ??
FreeBASIC | C for GCC | 
Gleam | Rust and Erlang (BEAM) | https://gleam.run/getting-started/installing/#installing-gleam
Go | self-hosting since 2015 | 
Inko | Rust for LLVM | 
Julia | C and C++ for GCC or LLVM, plus flisp Scheme (is it this one? https://github.com/fjames86/flisp) | [Required Build Tools and External Libraries](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/build/build.md#required-build-tools-and-external-libraries), [Design discussion and developer documentation](https://github.com/JuliaLang/julia/blob/master/JuliaSyntax/docs/src/design.md#design-discussion-and-developer-documentation)
Koka | Haskell and Stack for developing Haskell projects | [Build from Source](https://github.com/koka-lang/koka?tab=readme-ov-file#build-from-source)
Kotlin | JVM (Java Virtual Machine); LLVM multiplatform | Android: source code compiles to JVM bytecode; iOS: compiles to ARM64 machine code via LLVM for example: TBD is is not explaining the compiler language
Lua | implemented in pure ISO C; Lua also compiles as C++ | 
LuaJIT | GCC or LLVM | TBD: git: C and/or C++ code?
LunarML | Standard ML (MLton) | 
Mercury | GCC initially; more advanced library grades: self-hosting | 
Mojo |  -- |
OCaml |  -- |
Oz |  -- |
Perl 5 |  -- |
Picat |  -- |
PowerShell | script is compiled into PowerShell bytecode, which is then interpreted by the Common Language Runtime, often involving lots of overhead with PowerShell's object-heavy architecture | this doesn't explain the language: TBD C#?
Prolog, SWI | -- |
Python | -- |
Raku | -- | 
Roc | Rust and later additionally Zig |
Rust | self-hosting since 2011; the Rust compiler was written originally in OCaml | [Bootstrapping the compiler](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/intro.html)
Scala | JVM (Java Virtual Machine) to run JVM bytecode, multiplatform |
Scheme, Bigloo | -- | 
Scheme, Chez | initially, C for GCC or clang; otherwise self-hosting | 
Scheme, CHICKEN | -- | 
Scheme, Gambit | -- | 
Scheme, Racket | runs on Chez Scheme runtime system | --
Standard ML (MLton) | C for GCC or clang | 
Swift | -- | 
V | -- | 
wren | -- | 
Zig | -- | 

<br/>

TBD

<br/>

##_end


