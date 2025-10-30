# Languages that didn't make it to my list

..and which I started at least to test with my microbenchmark program.

Table of contents:

- [Hack](#hack)
- [Pony](#pony)
- [Toit](#toit)
- [Futhark](#futhark)
- [Old computer programming languages learning new tricks](#old-computer-programming-languages-learning-new-tricks)
- [Languages that were too slow](#languages-that-were-too-slow)
- [Koka](#koka)
- [Raku](#raku)
- [wren](#wren)

<br/>

### Hack

https://hacklang.org/

When I was about to run _Hello World!_ in Hack, facebook's effort for a new general purpose, high-level programming language and even with it's own HipHop Virtual Machine (HHVM): https://en.wikipedia.org/wiki/HHVM:

```
use namespace HH\Lib\IO;

<<__EntryPoint>>
async function main(): Awaitable<void> {
  await IO\request_output()->writeAllAsync("Hello World!\n");
}
```

..I learned that also this language didn't survive because PHP had its comeback: https://medium.com/@thoughtsfromryan/the-rapid-rise-and-fall-of-facebooks-hack-and-hhvm-7eeea401b04

### Pony

With Pony (https://www.ponylang.io/) I had to give up further development because as of May 2025 I was not able to implement the user dialog as mentioned here at: [Reading user input from the keyboard into a string](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#reading-user-input-from-the-keyboard-into-a-string).

I was not able to properly read a user input as a string (in the end) from the keyboard, even with investing a decent amount of time with searching it's open source code and it's examples. It's the only language so far I had to give up for maybe my own deficits. Though, this non-glamorous task turned out - unintentionally at first - to become an excellent opportunity to learn the real nature of a (new) programming language - even in the year 2025!

### Toit

https://toitlang.org/

Like with Pony, the supporting company went out of active business (with Pony it already happened years ago) - though, same like Pony, it's creators still maintain the language on
GitHub: https://github.com/toitlang/toit

This is a real pity from my point of view since the syntax of Toit would make a modern, up-to-date syntax for ubiquitous Python! However, with Toit as of May 2025 it's not
possible to the access the (Linux) file system, which - unintentionally again - is just an essential part of my microbenchmark program.

### Futhark

[Futhark](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Futhark#futhark) is "a data-parallel functional programming language", but not a general purpose one.

<br/>

## Old computer programming languages learning new tricks

The Hack example shows that also big corporations with decent ecosystems can fail with a new general purpose, high-level programming language.

Think of the fate of the **Modula-3** language of the once mighty and profitable Digital Equipment Corporation, together with Olivetti, for example: https://www.modula3.org/quotes/: this language is more dead now than **Self**, which just got an update last year: https://selflanguage.org/

> I wish the whole world were programming in Modula-3.

Joseph M. Newcomer, 1997

Why did Self became somehow famous? It allegedly was the first (major) language to introduce _Traits_ into the world of object-oriented programming, and now (almost) everybody has them, even OCaml: https://en.wikipedia.org/wiki/Trait_(computer_programming)

Apparently, almost nothing can be as dangerous to the popularity of a new programming language than old languages learning new tricks, see also the example of Java versus Scala: 

04 Mar 2025: **The Scala effect: Java’s Evolution Inspired by Scala**: https://blog.lunatech.com/posts/2025-02-28-the-scala-effect

> One of its biggest influences has been Scala — a language that brought functional programming, immutability, and expressive syntax to the JVM. Over the years, Java has steadily incorporated many features that were first introduced in Scala, from lambda expressions and pattern matching to records and data-oriented programming. 

<br/>

## Languages that were too slow

These languages have (substantially) exceeded [The 1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main?tab=readme-ov-file#the-1-second-execution-time-limit):

### Koka

Although my [Koka](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Koka#koka) program works correctly, though I didn't polish it, it's just super-slow with an execution time of over 5 seconds.

### Raku

Although my [Raku](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Raku%20(Perl%206)#raku) program works correctly, though I didn't polish it, it's just super-slow with an execution time of over 5 seconds.

### Wolfram Language

Although my [WolframScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Wolfram%20Language/random_streams_for_perf_stats.wls) proram works correctly, it takes about 1.5 seconds to run (without a string builder) and is thus not falling under my 1 second execution time limit. The idea here was that the Wolfram Language could also be seen as an array-oriented language. Anyway, it's a general purpose, high-level programming language. Here's a [Summary of New Features in 14](https://reference.wolfram.com/language/guide/SummaryOfNewFeaturesIn14.html), which may give an impression of the many features this language has already accumulated.

### wren

Although my [wren](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/wren#wren) program works correctly, it's just super-slow with an execution time of over 30 seconds without a string builder or similar concept.

<br/>

##_end
