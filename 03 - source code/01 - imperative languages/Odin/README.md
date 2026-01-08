# Odin

https://odin-lang.org/

<br/>

---

## String concatenation in Odin

Doing string concatenation in the ["C-style"](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) (see _bits_x := make([]u8, M1)_ below) is also working in Odin, bot doesn't render a faster program according to my tests:

```
import "core:fmt"  // println, eprintln, printf
import "core:math/rand"
...
main :: proc() {
  END : int : 62500  // 62500 for exactly 1M binary digits: constant
  M1  : int : END * 16
  ...
  m : int : 65521  // = 2^16 - 15
  a : int : 17364
  c : int : 0
  file_bits_x :: "random_bitstring.bin"
  ...
  x := make([]int, END)
  defer delete(x)
  x[0] = int(rand.int_max(m - 1))  // range is: [1...max]

  bits_x := make([]u8, M1)  // <--- "C-style"
  defer delete(bits_x)
  ...
  byte_nbr: int
  bits_x_str := "0000000000000000"
  ...
  for i in 1..<END {  // i is a variable
      x[i] = ((a * x[i-1]) + c) % m;

      bits_x_str = fmt.tprintf("%016b", x[i])   // t for temporary

      byte_nbr = (i-1)*16
      for j in 0..<16 {
        bits_x[byte_nbr + j] = bits_x_str[j]
      }
      ...
  }

  written := os.write_entire_file(file_bits_x, bits_x)
	if written {
		fmt.printf("Bit stream has been written to disk under name:  %v", file_bits_x)
	}
  ...
}
```

Above solution is not implemented, but this [Odin program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin/random_streams_for_perf_stats.odin) with a **string builder**, a concept which is also used numerous times in [Odin's GitHub repository](https://github.com/odin-lang); look there for _strings.builder_make()_.

So, using a string builder for bigger string concatenation jobs is apparently the idiomatic way in Odin.

One way or the other, Odin has a place in the Top 10 of languages in terms of execution speed: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

<br/>

## Characters are Runes in Odin

Odin doesn't feature the concept of "characters", like in C, anymore, though the documentation uses this term throughout: [Overview](https://odin-lang.org/docs/overview/)

Technically, Odin is using **runes**, see from this example, where _character_ is of type _rune_:

> When iterating a string, the characters will be runes and not bytes. _for in_ assumes the string is encoded as UTF-8.

```
str: string = "Some text"
for character in str {
	assert(type_of(character) == rune)
	fmt.println(character)
}
```

This means that working with individual characters, and finally their (Unicode) **codepoints**, is rune related. So, there's no longer something similar to the familiar _(char)65_ (= 'A') method in C.

And working with runes has (still) its challenges from my point of view; though being started in [2016](https://odin-lang.org/docs/faq/#what-is-the-history-of-the-project), this language is still young.

This becomes clearer at this code snippet from the full [microbenchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin/random_bitstring_and_flexible_password_generator.odin), where the available character set, which I strive to be of the string type throughout the different languages, is being composed:

```
  ...
  char_set_sb := strings.builder_make()
  defer strings.builder_destroy(&char_set_sb)
  if WITH_SPECIAL_CHARS {
      for codepoint in 33..< 127 {
          char: rune = rune(codepoint)  // first, convert a codepoint into a rune: this is the trick here!!
          // this is not featured in the https://odin-lang.org/docs/overview page,
          // but can be found in the GitHub repository! For example here:
          // https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/text/scanner/scanner.odin#L296
          s := fmt.tprintf("%r", char)  // then, convert this rune into a string
          strings.write_string(&char_set_sb, s)  // finally, append this string to a string builder
      }
  } else {
      strings.write_string(&char_set_sb, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
  }
  char_set := strings.to_string(char_set_sb)  // convert string builder into a string
  ...
```

Again, the Odin string builder comes into play (_char_set_sb_), which shows how important, even with little, dynamic strings, it is.

Both (important) concepts, runes and string builder, should be more documented in this language [Overview](https://odin-lang.org/docs/overview/) page in my opinion.

<br/>

##_end
