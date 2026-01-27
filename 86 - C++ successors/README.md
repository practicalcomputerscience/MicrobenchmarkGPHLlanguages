# Potential "C++ successors": which one to take?

In reference to page [Potential "C successors": which one to take?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/85%20-%20C%20successors#potential-c-successors-which-one-to-take), here's a rough collection of potential "C++ successors":

- Carbon (see below)
- D
- Go
- Eiffel
- Mojo
- Odin
- Pony
- Rust
- Zig

So far, only Go and Rust have become "big" already; Mojo and Zig will probably become bigger in my opinion.

And as I mentioned [before](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/01%20-%20presentation%20slides#overview-slides),
"C++ will be around for still many years to come...", also thanks to its ongoing evolution.

---

<br/>

## Carbon

[Carbon](https://docs.carbon-lang.dev/) is indeed, as of 2026-01-27, in an experimental state as a potential successor to C++: https://github.com/carbon-language/carbon-lang/tree/trunk

For example, official example [hello_world.carbon](https://github.com/carbon-language/carbon-lang/blob/trunk/examples/hello_world.carbon) doesn't work at the moment, because the [Core.PrintStr](https://github.com/carbon-language/carbon-lang/blob/9f6e84cc022aa60fdfab08b6862104626253fbc1/core/io.carbon#L17) function is not implemented yet:

```
...
import Core library "io";

fn Run() {
  Core.PrintStr("Hello world!\n");
}
```

However, printing integer numbers works; so this example from [Getting started](https://docs.carbon-lang.dev/#getting-started) works:

```
$ echo "import Core library \"io\"; fn Run() { Core.Print(42); }" > forty_two.carbon
$ carbon compile --output=forty_two.o forty_two.carbon
$ carbon link --output=forty_two forty_two.o
$ ./forty_two
42
$
```

However, with the help of C++ the "Hello world!" program can be saved:

```
import Core library "io";
import Cpp library "<iostream>";

fn Run() {
  // Core.PrintStr("Hello world!\n");
  Cpp.std.cout << "Hello world!\n";
}
```

Compile and link it like this:

```
$ carbon compile hello_world2.carbon --output=hello_world2.o
$ carbon -v link hello_world2.o --output=hello_world2  # -v for verbose
$ ./hello_world2
Hello world!
$
```

<br/>

##_end
