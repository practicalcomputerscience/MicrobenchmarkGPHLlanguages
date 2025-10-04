# Languages that didn't make it to my list

..and which I started to test with my microbenchmark program.

<br/>

## Hack

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

In other words: also big corporations with a decent ecosystems can fail with a new general purpose, high-level programming language!

Or think of the fate of the **Modula-3** language of the once mighty and profitable Digital Equipment Corporation, together with Olivetti: https://www.modula3.org/quotes/: this language is more dead now than **Self**, which just got an update last year: https://selflanguage.org/

Why did Self became somehow famous? It allegedly was the first (major) language to introduce _Traits_ into the world of object-oriented programming, and now (almost) everybody has them, even OCaml: https://en.wikipedia.org/wiki/Trait_(computer_programming)

---

## Pony

With Pony (https://www.ponylang.io/) I had to give up further development because as of May 2025 I was not able to implement the user dialog as mentioned here at: [Reading user input from the keyboard into a string](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#reading-user-input-from-the-keyboard-into-a-string).

I was not able to properly read a user input as a string (in the end) from the keyboard, even with investing a decent amount of time with searching it's open source code and it's examples. It's the only language so far I had to give up for maybe my own deficits. Though, this non-glamorous task turned out - unintentionally at first - to become an excellent opportunity to learn the real nature of a (new) programming language - even in the year 2025!

---

## Toit

https://toitlang.org/

Like with Pony, the supporting company went out of active business (with Pony it already happened years ago) - though, same like Pony, it's creators still maintain the language on
GitHub: https://github.com/toitlang/toit

This is a real pity from my point of view since the syntax of Toit would make a modern, up-to-date syntax for ubiquitous Python! However, with Toit as of May 2025 it's not
possible to the access the (Linux) file system, which - unintentionally again - is just an essential part of my microbenchmark program.

<br/>

##_end
