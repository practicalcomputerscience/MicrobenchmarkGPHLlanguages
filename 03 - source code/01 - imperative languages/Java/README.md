2026-05-18: work in progress: tbd

# Java

The source code of the Java version of the microbenchmark program was mostly transpiled with the help of "Big AI" from its [Haxe](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#haxe) source code.

This was not a very efficient transpilation, although the initially resulting Java program was working fully correctly after some (minor) corrections with additional prompts.

However, and as this background at ["Java"](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#java) tries to explain,
the idea here was not to find the best source language to transpile into the most efficient Java source code, while still keeping the result idiomatic.

The original, "Big AI" based transpilation from one Haxe source code file into only **one** Java source code file showed that Haxe is still a language "without (too) many batteries included" with its 3 user defined functions, while [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy), my source language for numerous transpilations, needs none, same like Java:

language | Source Lines Of code (manually improved) | number of user defined functions | program execution time | comment
--- | --- | --- | --- | ---
Haxe    | 134  | 3 | 152 milliseconds with bytecode for the (modern) HashLink virtual machine | language for "cross-platform development"
Groovy  | 99   | 0 | 341 milliseconds with an uberJAR file for the Java virtual machine | my preferred source language for transpilations
Java    | TBD  | 0 | tbd milliseconds with an uberJAR file for the Java virtual machine | development target: having only one source code file in target language Java

<br/>

##_end
