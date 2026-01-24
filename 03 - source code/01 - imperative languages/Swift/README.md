# Swift

https://www.swift.org/

https://github.com/swiftlang/swift

<br/>

Swift may explain why you cannot make money with a (new) programming language apparently, only at its boundaries, that is when connecting to the outside world.

Or put differently: why is Apple's Swift (finally) [open source](https://github.com/swiftlang), but not [SwiftUI](https://swiftpackageregistry.com/OpenSwiftUIProject/OpenSwiftUI)?

---

<br/>

## Installation tips

Command _$ swift build -c release_ is not compiling to a standalone executable for Linux. The compiled program depends on one **shared library** (_libswiftCore_) at least, which is provided if Swift is installed on the target machine.

<br/>

## On string padding in Swift

As of January 2026, Swift apparently still misses an integrated function to convert an integer number into its representation as a string with base 2 or 16 or whatever - **including padding** on the left hand side, so something like the _std::format_to(bits_x_str, "{:016b}", x[i]);_ function in [C++20](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B/random_streams_for_perf_stats.cpp).

After more experiments in January 2026, this is still my fastest [solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift/random_streams_for_perf_stats.swift) with an "inlined" for-loop for string padding (be aware that statistically 50% of the generated strings won't enter this for-loop because they don't need padding):

```
...
for i in 1..<END {
    x.append((a*x[i-1] + c) % m)
    ...

    bits_x_str     = String(x[i], radix: 2)               // no padding
    ...
    
    for _ in 0..<(16 - bits_x_str.count) {
      bits_x_str = "0" + bits_x_str
    }
    ...
    bits_x += bits_x_str
    ...
}
...
```

<br/>

##_end
