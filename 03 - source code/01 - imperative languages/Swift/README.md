# Swift

https://www.swift.org/

https://github.com/swiftlang/swift

<br/>

Swift may explain why you cannot make money with a (new) programming language apparently, only at its boundaries, that is when connecting to the outside world.

Or put differently: why is Apple's Swift (finally) [open source](https://github.com/swiftlang), but not [SwiftUI](https://swiftpackageregistry.com/OpenSwiftUIProject/OpenSwiftUI)?

<br/>

NS = [NeXTSTEP](https://www.linkedin.com/posts/mansi-tanna15_swift-xcode-iosdevelopment-share-7222941614715883520-Tic7/)

> The NS prefix is seen in many foundational classes from the Objective-C era, such as NSString, NSArray, and NSObject. Even though Swift introduces its own native types (like String), you'll often interact with NS-prefixed classes when bridging between Swift and Objective-C or using certain APIs.

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

### Static linking in Swift

Static linking with bulding into another subdirectory can be done like this for example: _$ swift build -c release --build-path ./stat_linking --static-swift-stdlib_.

Switch _--static-swift-stdlib_ only links the Swift Standard Library statically! So, the ldd command will still show some external dependencies:

```
$ ldd ./stat_linking/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
	linux-vdso.so.1 (0x0000775bce2f9000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x0000775bcd517000)
	libstdc++.so.6 => /lib/x86_64-linux-gnu/libstdc++.so.6 (0x0000775bcd200000)
	libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x0000775bce2ac000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x0000775bcce00000)
	/lib64/ld-linux-x86-64.so.2 (0x0000775bce2fb000)
$
```

Though, their number is drastically reduced (from 20 down to 6).

"Statically linking" builds a program with about -10% less execution time (at 32°C ambient temperature as of 2026-06-29!):

```
$ multitime -n 10 ./stat_linking/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
...
===> multitime results
1: ./stat_linking/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
            Mean        Std.Dev.    Min         Median      Max
real        0.028       0.004       0.026       0.026       0.040
...
$
```

..compared to default switch _--no-static-swift-stdlib_:

```
$ multitime -n 10 ./.build/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats 
...
===> multitime results
1: ./.build/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
            Mean        Std.Dev.    Min         Median      Max
real        0.031       0.001       0.030       0.031       0.034
...
$
```

<br/>

##_end
