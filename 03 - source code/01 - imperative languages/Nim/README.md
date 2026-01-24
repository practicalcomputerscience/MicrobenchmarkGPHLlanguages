# Nim

https://nim-lang.org/

---

Table of contents:

- [On how to do demanding string building in Nim](#on-how-to-do-demanding-string-building-in-nim)
- [Nim's Memory Management](#nims-memory-management)
- [Why is Nim still not very popular?](#why-is-nim-still-not-very-popular)

<br/>

---

## On how to do demanding string building in Nim

Based on other microbenchmarks, like this one for example: https://github.com/zupat/related_post_gen, I had somehow higher expectations for the execution speed of my microbenchmark program in a Nim implementation.

The first times with around 50 milliseconds that came in, already with compiler switch _-d:release_ activated
and using Nim's [string builder](https://nim-lang.org/1.2.14/system.html#newStringOfCap%2CNatural),
were not bad, but a bit muted compared to my expectations.

So, I started to experiment again, specifically after I discovered Nim's [rope data type](https://nim-lang.org/docs/ropes.html), which "can represent very long strings efficiently; in particular, concatenation is done in O(1) instead of O(n)."

Here's a table with indicative execution times from only one run to just get an overview:

test # | construct | compiler switches | exe time in milliseconds | comment
--- | --- | --- | --- | ---
1 | _newStringOfCap_ | _-d:release_ | ~50 | Nim's string builder
2 | _newStringOfCap_ | _-d:danger_ | ~47 | this compiler switch "turns off all runtime checks and turns on the optimizer"
3 | _rope_ | _-d:release_ | ~41 |
4 | _rope_ | _-d:danger_ | ~39 | so, the _-d:danger_ switch can apparently squeeze out some extra exe time
5 | _seq[string] = @[]_ | _-d:release_ | ~53 | starting with an empty array, a container type under the _openArray_ umbrella; then adding the individual strings
6 | _proc integerToBinString()_ | -d:danger | ~30 | replacing built-in function _fmt"{x[i]:016b}"_ with a user defined function

I guess it's highly probable that replacing the other built-in function _fmt"{x[i]:04x}"_ with a user defined function would again lower the execution time.

However, since I'm not doing it in my official [C version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#keeping-using-idiomatic-constructs), I'm also not doing it here beyond some experimentation.

All and all, I think test #3, that is using only the _-d:release_ compiler switch, using ropes instead of the not overly helpful string builder here, and using only built-in functions for the integer to string conversions, is a fair compromise. On the other hand, this setup will put the Nim program just behind [Common Lisp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments), a functional programming language, in terms of execution speed! (as of 2026-01-12)

Additionally, I think that it's also interesting to implement another concept than using the language's string builder, specifically if the alternative concept offers even more efficient string concatenation (though, potentially only for specific use cases).

<br/>

## Nim's Memory Management

It should be noted that Nim by default is a **garbage-collected** programming language: [Nim's Memory Management](https://nim-lang.org/1.4.8/gc.html), something I didn't play with.

However, [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go) is also a garbage-collected programming language, and the Go version has an execution time of under 16 milliseconds.

And [Chrystal](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Crystal#crystal) too ([Avoiding memory allocations](https://crystal-lang.org/reference/1.19/guides/performance.html#avoiding-memory-allocations)), with the Chrystal version of the "speed part" of this microbenchmark program featuring a sensational execution time of under 8 milliseconds.

<br/>

## Why is Nim still not very popular?

Being on the market since 2008, I had this question, and just saw that I'm not the only one to ask it:

- 2023: [Ask HN: Why did Nim not catch on like Rust did?](https://news.ycombinator.com/item?id=36475744)
- 2024: [What are the biggest issues with Nim right now?](https://www.reddit.com/r/nim/comments/1dkze0x/what_are_the_biggest_issues_with_nim_right_now/)

Bold points why Nim is still a niche language are discussed. However, I still have two more structural points:

- Nim is still basically a one person language development (in Germany), same like Odin in UK. What happens with Nim, if Andreas Rumpf is no more with Nim? Compare this situation to the fate of the originally one-person-language Clojure, started by Rich Hickey, and which has been finally saved by selling it to a (bigger) corporation (in Brazil). Chrystal (since 2014) and Mojo (since 2023) right from start have been backed by companies (in Buenos Aires, Argentina and L.A., U.S., respectively). Roc has a (small) U.S. team as backing for example
- I think that European one-person-programming languages generally have a hard time. A European "university language" with superior concept, and backed by no less than four employed professors, has gone into oblivion, that is Oz. Don't tell me that this was the original match plan. Or think of the abondonment of [The Tiny C Compiler (TCC)](https://www.bellard.org/tcc/) ("TinyCC is about 9 times faster than GCC.") by [Fabrice Bellard](https://en.wikipedia.org/wiki/Fabrice_Bellard) from France, and with a last official release from 2017

=> who wants to invest serious time and money in such a high-risk endeavor?

Yes, a foundation could solve this (allegedly, this has been declined by the founder). Roc has one for example: https://www.roc-lang.org/foundation

But how do you pull this off in a fragmented European landscape?

<br/>

Just to know:

- [futhark](https://github.com/PMunch/futhark) "aims to allow you to simply import C header files directly into Nim, and allow you to use them like you would from C without any manual intervention."
- [nimpy](https://scinim.github.io/getting-started/external_language_integration/nim_with_py.html) "to integrate Python code with Nim"

<br/>

##_end
