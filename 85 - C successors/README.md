# Potential "C successors": which one to take?

<br/>

Typically, a "C successor" (+) is capable of supporting **"system programming"**, at best being also a "portable assembly language", while still be capable of supporting "application programming":

<br/>

language | upside | downside | comment
-- | -- | -- | --
[C3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C3#c3) | a soft evolution of C | tiny ecosystem; some functionalities still need to be expanded | 
[Checked C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#checked-c)| a backward‑compatible extension of C | tiny ecosystem | 
[Nim](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Nim#nim) | concise, "Pythonic" source code | still a small ecosystem; doesn't have an "elevator pitch selling point"; documentation of advanced concepts still needs to be improved | community broke apart, look for "nimskull"
[Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin) | focus on data‑oriented programming: "Odin is the C alternative for the Joy of Programming." | tiny ecosystem; some functionalities still need to be expanded; documentation needs to be improved | 
[Rust](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Rust#rust) | big ecosystem; has now two important references: Linux kernel and Linux Core Utilities | a demanding language, influenced by functional programming | may not be well suitable for programming of high-performance computer games, even though a couple of frameworks and engines exist: [Leaving Rust gamedev after 3 years](https://loglog.games/blog/leaving-rust-gamedev/) from 2024
[Zig](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig#zig) | growing ecosystem | still a fast moving target | 

<br/>

(+) why a "C successor"? Confer [Overview slides - Part 2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/01%20-%20presentation%20slides#part-2): "Its string and character model is outdated in the 21st century." -- this is only my little contribution, plus the fact that function family _(s)printf_ apparently isn't fully standardized, as my (accidental) tapping into [Checked C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#checked-c) has brought to light.

This long specification paper [Extending C with bounds safety and improved type safety, Version 0.9](https://github.com/microsoft/checkedc/releases/download/CheckedC-Clang-12.0.1-rel3/checkedc-v0.9.pdf), (Draft as of September 14, 2021, Checked C Technical Report Number 1, Author: David Tarditi, Microsoft), offers good insights into the deficits of established C in a modern programming world:

> The C programming language .. allows programmers to use pointers directly. A pointer is an address of a location in memory. Programs may do arithmetic on pointers, dereference them to read
memory, or assign through them to modify memory. The ability to use pointers directly makes C well-suited for low-level system programming that is “close to the hardware” and allows programmers
to write efficient programs. ..

> (Pointers and the unification of arrays and pointers) are a source of reliability and security problems in modern software. This is because pointers and array indices are not bounds checked in C and related languages such as C++. ..

<br/>

Even though Google's [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go) claims that:

> Go is a systems programming language...

from: https://go.dev/wiki/GoForCPPProgrammers

.."normal" Go **with its own runtime** is not suitable for system programming like C with its total kernel/driver/bare-metal control.

So, for doing **low-level system programming** a Go program may very well has to tap into the C ecosystem.

<br/>

[V](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/V#v-programming-language) has been primarily designed as a high-level programming language, not as a low-level system programming language. The default way to manage memory in V is to use its garbage collection. 

<br/>

tbd
 
<br/>

##_end
