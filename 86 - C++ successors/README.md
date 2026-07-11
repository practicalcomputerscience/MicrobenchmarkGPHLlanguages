# Potential "C++ successors": which one to take?

In reference to page [Potential "C successors": which one to take?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/85%20-%20C%20successors#potential-c-successors-which-one-to-take), here's a rough collection of potential "C++ successors":

<br/>

language | comment
-- | --
Carbon | see below
[D](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/D#d) | never lifted off: one, small, single vendor ecosystem
[Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go) | 
[Eiffel](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01a%20-%20object-oriented%20languages/Eiffel#eiffel) | same like with Smalltalk: not everything is an object: [The world as processes and events](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/05%20-%20the%20world%20as%20processes%20and%20events#the-world-as-processes-and-events)
[Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3) | Java also (almost) killed Modula-3 (along with Smalltalk): https://mcjones.org/dustydecks/archives/2025/10/25/1496/
[Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#mojo) | 
[Oberon-2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Oberon#oberon) | in the 90ies touted as a [hi-performance alternative to C++](https://www.modulaware.com/mdltws.htm), Oberon-2 is almost dead now, suffocated by a fragmented ecosystem. The further development of [Modula-2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2#modula-2), to not fall further behind the huge wave of object-oriented programming for enterprise level software development in the late 80ies, fell into two branches, an industrial one with Modula-2+ and then Modula-3, and academic one with Oberon, Oberon-2, Active Oberon, ... 
[Odin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Odin#odin) | one, small, single vendor ecosystem
[Pony](https://www.ponylang.io/) | in the year 2026, this language is still not ready for general purpose programming
[Rust](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Rust#rust) | in 2026 the picture becomes clearer from my point of view: Rust is eating much more into C's ecosystem than into C++'s
[V](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/V#v-programming-language) | one, small, single vendor ecosystem
[Zig](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig#zig) | 

<br/>

So far, only Go and Rust have become big already; Mojo and Zig will probably become bigger in my opinion.

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
