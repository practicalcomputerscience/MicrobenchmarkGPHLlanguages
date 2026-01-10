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
Ada | C: GCC (GNU Compiler Collection) or LLVM back end compilation families | GNAT = GNU Ada Development Environment 
C | C: GCC or LLVM, with both requiring a working C++ compiler and having numerous other dependencies | only C++ is self-hosting nowadays, not even C anymore
C# | Microsoft .NET Framework (CLR = Common Language Runtime) | 
C3 | C and LLVM compiler infrastructure | 
Chapel | C++, LLVM and clang | 
Clojure | the Clojure compiler (_compile-clj_) is written in Java | 
Common Lisp (SBCL) | self-hosting | https://www.sbcl.org/getting.html
Crystal | Crystal and LLVM | https://crystal-lang.org/install/from_sources/
Fortran (GNU) | GCC | 
FreeBASIC | C and GCC | 
Gleam | Rust and Erlang (BEAM) | https://gleam.run/getting-started/installing/#installing-gleam
Go | self-hosting since many years | 
Inko | Rust and LLVM | 
Julia | C and C++: GCC (GNU Compiler Collection) or LLVM back end compilation families, plus flisp Scheme (is it this one? https://github.com/fjames86/flisp) | [Required Build Tools and External Libraries](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/build/build.md#required-build-tools-and-external-libraries), [Design discussion and developer documentation](https://github.com/JuliaLang/julia/blob/master/JuliaSyntax/docs/src/design.md#design-discussion-and-developer-documentation)
Koka | -- | 
Kotlin | JVM (Java Virtual Machine), LLVM multiplatform | Android: source code compiles to JVM bytecode; iOS: compiles to ARM64 machine code via LLVM for example
Lua | -- | 
LuaJIT | -- | 
LunarML | -- | 
Mercury | GCC initially; more advanced library grades: self-hosting | 
Mojo |  -- |
OCaml |  -- |
Oz |  -- |
Perl 5 |  -- |
Picat |  -- |
PowerShell |  Microsoft .NET Framework (CLR) | --
Prolog, SWI | -- |
Python | -- |
Raku | -- | 
Roc | Rust and Zig |
Rust | self-hosting nowadays  | [Bootstrapping the compiler](https://rustc-dev-guide.rust-lang.org/building/bootstrapping/intro.html)
Scala | JVM (Java Virtual Machine), multiplatform  |
Scheme, Bigloo | -- | 
Scheme, Chez | -- | 
Scheme, CHICKEN | -- | 
Scheme, Gambit | -- | 
Scheme, Racket | Chez Scheme | --
Standard ML (MLton) | -- | 
Swift | -- | 
V | -- | 
wren | -- | 
Zig | -- | 

<br/>

##_end


