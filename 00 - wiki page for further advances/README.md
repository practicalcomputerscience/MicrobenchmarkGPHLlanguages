# Wiki page for further advances

2025-10-20: all major advances starting with scripting language [wren](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/wren#wren) and all major changes of existing documentation will be noted here

---

2026-01-19: done: [Ruby](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#ruby)

2026-01-15: done: [C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B#c)

2026-01-13: done: [Nim](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Nim#nim)

2026-01-12: cleaned up the "masterloop" in the [PowerShell](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/PowerShell#powershell) programs a bit to have a more logical and efficient program

2026-01-09: done: [Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin)

2026-01-06: done: [Fortran](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Fortran#fortran)

2026-01-05: change of mind: I plan to implement the microbenchmark program, if possible for me, at last in [J](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/J#j), because I'm afraid that it will take me a long time, longer than a week for example. Although, my first baby steps in J worked. Before, I just shortly played with [ELI - A System for Programming with Arrays](https://fastarray.appspot.com/index.html), but didn't get it working. This language (from McGill University: https://www.sable.mcgill.ca/~hanfeng.c/eli/) hasn't been updated since 2015. The only question then is what to do with The Nial Language: https://www.nial-array-language.org/, as another array-oriented language.

2026-01-04: done: [Julia](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Julia#julia) (which is not a contender to Mojo or Chapel)

2025-12-31: full microbenchmark program in [Bigloo Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo#bigloo-scheme). It highlights the differences to [Racket Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket#racket-scheme).

2025-12-25: cleanup work almost done; cf. [Bigloo Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo#bigloo-scheme)

2025-12-16: I'm not continuing with LFE (Lisp Flavoured Erlang: https://lfe.io/about/): even already compiled escripts are being re-compiled when running them again (_$ rebar3 lfe run-escript_); thus, LFE programs always have a slow start. I'm happy with [Gleam](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM#gleam) as a (performant) language on Erlang's virtual machine and will keep it like that.

2025-12-13: update to latest Mojo version 0.26.1.0... with the Pixi package manager. The old Magic package manager, a fork of Pixi, is now deprecated: https://docs.modular.com/pixi; some slight, formalistic source code changes to get rid of new warnings: [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo)

2025-12-10: [Oz](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Oz#oz): "speed part" of microbenchmark done, though not the rest because that has become practically impossible with this dead programming language

2025-11-22: done: [SWI Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#microbenchmark-program-in-swi-prolog) as a representative for the still numerous and actively maintained Prolog systems

2025-11-16: done: [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury), a logic and functional language in a purely declarative style: my microbenchmark program is working correctly and swiftly in this language. However and apparently, implementing solutions for more than simple Constraint Satisfaction Problems is (almost) impossible in this language.

2025-11-01: done: [Wolfram Language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Wolfram%20Language#wolfram-language): too slow, but working

2025-10-29: done: [Gleam](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM#gleam), statically typed, functional programming on Erlang's BEAM virtual machine

2025-10-25: done: [Futhark](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Futhark#futhark): not a general-purpose language, though I did a little demo

2025-10-24: done: [wren](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/wren#wren): too slow, but working

---

<br/>

2026-01-15: so far, I haven't touched yet these corners of programming languages:

- D (with GCC?) as a "natural" alternative to C++
- Carbon Language: An experimental successor to C++: https://docs.carbon-lang.dev/#why-build-carbon
- Eiffel: https://www.eiffel.org/
- https://haxe.org/: strictly-typed programming language
- Smalltalk as the U.S.'s start into object-oriented programming
- Dylan: https://opendylan.org/download/: _Dylan is a direct descendant of Scheme and CLOS (without the Lisp syntax).._ (CLOS = Common Lisp Object System)
- Pure Programming Language is a functional programming language:
- &nbsp;&nbsp;https://github.com/agraef/pure-lang/wiki
- &nbsp;&nbsp;https://agraef.github.io/pure-lang/
- Hy is a Lisp dialect that's embedded in Python: https://hylang.org/
- **Stack-oriented programming languages**: https://en.wikipedia.org/wiki/Forth_(programming_language)
- &nbsp;&nbsp;2025: _Implementing Forth in Go and C_: https://eli.thegreenplace.net/2025/implementing-forth-in-go-and-c/
- Haskell?!?
- **[Array-oriented languages](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages#array-oriented-languages)**; maybe:
- &nbsp;&nbsp;(GNU Octave (https://octave.org/): 2025-11-01: I will skip this MATLAB clone (https://www.rath.org/matlab-is-a-terrible-programming-language.html), since I successfully implemented the microbenchmark program in [Wolfram Language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Wolfram%20Language#wolfram-language), though I had to put that language onto my list of too slow languages, to my regret by the way.)
- &nbsp;&nbsp;The Nial Language: https://www.nial-array-language.org/
- &nbsp;&nbsp;**J** 

<br/>

##_ end
