# The future of transpiling

There's a deeper sense why I wrote a little bit more about this exotic corner of programming languages into which I accidentally tumbled: [Transpiling from Standard ML to Lua and JavaScript with LunarML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml)

## Microsoft's efforts with transpilation

In March of this year Microsoft announced: [A 10x Faster TypeScript](https://devblogs.microsoft.com/typescript/typescript-native-port/), where Microsoft takes Google's **Go** programming language (https://github.com/microsoft/typescript-go) to transpile TypeScript source code into JavaScript source code.

So far, this has been done with "self-hosting", that is that the TypeScript compiler is implemented in TypeScript, which also is JIT (Just-In-Time) compiled into JavaScript while doing its compilation work.

The choice of Go obviously was a surprise move for many observers - does it mean in my understanding that Microsoft is apparently no longer developing its own [statically typed programming language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Koka#koka) which can be compiled into standalone executables for a variety of platforms; at least not with "full force".

This news was my incentive to "transpile" my [original little Python program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main?tab=readme-ov-file#what-this-repository-is-about), the language I usually use to test an idea and in which I just quickly implemented a simple PRNG (pseudo-random number generator), plus extras, into other languages, including Go, which has been on my "long list" for language testing since 2023.

<br/>

##_end
