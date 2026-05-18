2026-05-14: work in progress

TBD: full microbenchmark program in Java

# Java

The source code of the Java version of the microbenchmark program was mostly transpiled with the help of "Big AI" from its [Haxe](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#haxe) source code.

This was not a very efficient transpilation, although the initially resulting Java program was working fully correctly after some (minor) corrections with additional prompts.

However, and as this background at ["Java"](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#java) tries to explain,
the idea here was not to find the best source language to transpile into the most efficient Java source code, while still keeping the result idiomatic.

The original, "Big AI" based transpilation from one Haxe source code file into only **one** Java source code file just showed that Haxe is still a language "without (too) many batteries included" with its 3 user defined functions, while Groovy needs none:

language | Source Lines Of code (manually improved) | program execution speed
--- | --- | ---
Haxe    | 134  | 152 milliseconds with bytecode for the (modern) HashLink virtual machine
Groovy  | 99   | 341 milliseconds with an uberJAR file for the Java virtual machine

<br/>

##_end
