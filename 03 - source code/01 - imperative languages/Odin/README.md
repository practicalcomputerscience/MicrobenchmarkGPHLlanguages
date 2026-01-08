2025-01-08: work in progress

# Odin

https://odin-lang.org/

<br/>

---

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

One way or the other, Odin has a place in the Top 10 of languages in terms of execution speed: [Master diagram with most program environments](github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

TBD

<br/>

##_end
