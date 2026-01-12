2026-01-12: work in progress

# Nim

https://nim-lang.org/

---

Based on other microbenchmarks, like this one for example: https://github.com/zupat/related_post_gen, I had somehow higher expectations for the execution speed of my microbenchmark program in a Nim implementation.

The first times with around 50 milliseconds that came in, already with compiler switch _-d:release_ activated
and using Nim's [string builder](https://nim-lang.org/1.2.14/system.html#newStringOfCap%2CNatural),
were not bad, but a bit muted compared to my expectations.

So, I started to experiment again, specifically after I discovered Nim's [rope data type](https://nim-lang.org/docs/ropes.html#rope_1), which "can represent very long strings efficiently; in particular, concatenation is done in O(1) instead of O(n)."

Here's a table with indicative execution times from only one run to just get an overview:

construct | compiler switches | exe time in milliseconds | comment
--- | --- | --- | ---
_newStringOfCap_ | _-d:release_ | ~50 | Nim's string builder
_newStringOfCap_ | _-d:danger_ | ~47 | this switch "turns off all runtime checks and turns on the optimizer"
_rope_ | _-d:release_ | ~41 |
_rope_ | _-d:danger_ | ~39 | so, the _-d:danger_ can apparently squeeze out some extra exe time
_seq[string] = @[]_ | _-d:release_ | ~53 | starting with an empty array, a container type under the _openArray_ umbrella; then adding the individual strings
_proc integerToBinString()_ | -d:danger | ~30 | replacing built-in function _fmt"{x[i]:016b}"_ with a user defined function

TBD







<br/>



##_end
