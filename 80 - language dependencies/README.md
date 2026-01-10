2026-01-10: work in progress; experimental

# Language dependencies

After implementing the microbenchmark program in [Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin),
I tumbled over this sentence when I started to read about [Nim](https://nim-lang.org/), another "statically typed compiled systems programming language":

> The Nim compiler needs a C compiler in order to compile software.

Then I had this question:

> How can a high-level programming language truly succeed an older high-level programming language, when it depends on it?

Thus, every time the underlying high-level programming language makes a change,
there's more or less the latent danger that the dependent high-level programming language may be up for a change too, also indirectly for the source code written in the dependent language.

Other sources: TBD

<br/>

So, I start to compile another table, where I try to list more or less close dependencies:

dependent programming language | underlying programming language or environment | comment
--- | --- | ---
Ada | C: GCC or LLVM back end compilation families | GNAT = GNU Ada Development Environment 
C | C: GCC or LLVM, with both requiring a working C++ compiler and having numerous other dependencies | so, only C++ is "self-hosting" nowadays
C# | Microsoft .NET Framework (CLR = Common Language Runtime) | 
C3 | LLVM | 
Chapel | -- | 
Clojure | JVM (Java Virtual Machine) | 
Common Lisp (SBCL) | -- | 
Crystal | -- | 
Fortran (GNU) | -- | 
FreeBASIC | -- | 
Gleam | Erlang (BEAM) | --
Go | -- | 
Inko | -- | 
Julia | -- | 
Koka | -- | 
Kotlin | JVM (Java Virtual Machine), LLVM multiplatform | on Android, source code becomes JVM bytecode; on iOS, it becomes ARM64 machine code via LLVM for example
Lua | -- | 
LuaJIT | -- | 
LunarML | -- | 
Mercury |  self-hosting | 
Mojo |  -- |
OCaml |  -- |
Oz |  -- |
Perl 5 |  -- |
Picat |  -- |
PowerShell |  Microsoft .NET Framework (CLR) | --
Prolog, SWI | -- |
Python | -- |
Raku | -- | 
Roc | Rust and Zig | TBD
Rust | -- | TBD
Scala | JVM (Java Virtual Machine), multiplatform  | TBD
Scheme, Bigloo | -- | TBD
Scheme, Chez | -- | TBD
Scheme, CHICKEN | -- | TBD
Scheme, Gambit | -- | TBD
Scheme, Racket | Chez Scheme | --
Standard ML (MLton) | -- | TBD
Swift | -- | TBD
V | -- | TBD
wren | -- | TBD
Zig | -- | TBD

<br/>

##_end


