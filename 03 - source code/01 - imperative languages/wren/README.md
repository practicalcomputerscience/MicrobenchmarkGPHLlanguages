# wren

https://wren.io/

---

[Lua](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Lua#lua) brought me to wren.

However, MS Copilot Search just told me that wren has a _StringBuilder_ (_import "string" for StringBuilder; var sb = StringBuilder.new()_) for my prompt: "fast string concatenation in wren".

[wren](https://github.com/wren-lang/wren) does not have a string builder. This means that plain string concatenation is used here, which is very slow with one program run taking over 30 seconds:

```
...
var bits_x       = ""
...
for (i in 1..END) {  // with .. END is exclusive
  x_new = ((a * x[i-1]) + c) % m
  ...
  bits_x_str = Integer_to_bin_string.call(x_new)
  ...
  bits_x = bits_x + bits_x_str
  ..
}
...
```

This means two things:

- putting wren on this [list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#languages-that-didnt-make-it-to-my-list)
- the [gaming world](https://generalistprogrammer.com/tutorials/lua-game-development-scripting-complete-guide-2025) will continue to use [Lua](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Lua#lua)

Though, technically this script works OK.

A _switch_ statement in this language would also be very nice, then the big _if-(then)-else_ cascade in user defined function [Integer_to_hex_string](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/wren/random_streams_for_perf_stats.wren) would look much better.

##_end
