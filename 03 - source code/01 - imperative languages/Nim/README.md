2026-01-12: work in progress

# Nim

https://nim-lang.org/

---

### Demanding string building in Nim

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

TBD







<br/>



##_end
