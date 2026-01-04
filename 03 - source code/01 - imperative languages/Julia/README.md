# Julia

https://julialang.org/

Julia Micro-Benchmarks: https://julialang.org/benchmarks/

<br/>

Table of contents:

- [Microbenchmark program in Julia](#microbenchmark-program-in-julia)
- [Julia and Python](#julia-and-python)
- [Building a "standalone" app from a Julia program](#building-a-standalone-app-from-a-julia-program)
- [Further experiments for a "standalone" app](further-experiments-for-a-"standalone"-app)

<br/>

---

## Microbenchmark program in Julia

Fast string building, even with the help of [IOBuffer](https://docs.julialang.org/en/v1/base/io-network/#Base.IOBuffer), isn't the strong side of Julia apparently; even when initially setting the size of an _IOBuffer_ (see below).

After some experimentation, I found out that the conventional approach, that is initially defining an array of strings of fixed size and finally concatenating these strings into one big string, is (also) the best solution in Julia.

This solution with _IOBuffer_ didn't make a faster program; it's execution speed is about the same:

```
...
END = 62501  # 62501 for exactly 1M binary digits; const END has no speed effect here
M1  = END*16 - 16
...
# this type can be used as a string builder:
bits_x   = IOBuffer(UInt8[], read=true, write=true, maxsize=M1)
...
for i in 2:END
    h = i - 1
    x[i] = (a*x[h] + c) % m

    bits_x_str0 = string(x[i], base=2)
    bits_x_str  = lpad(bits_x_str0, 16, "0")  # padding: https://www.jlhub.com/julia/manual/en/function/lpad
    write(bits_x, bits_x_str)
    ...
end
...
```

(above solution is not implemented, but this: [Julia program](./random_streams_for_perf_stats.jl))

So, execution speed with the conventional solution is about 170 milliseconds, but only with optimization level switch _-O0_, that's a bit slower than the [Python solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py) with the help of _StringIO_ with about 140 milliseconds.

Beware of the direction of the optimization level switch, where _-O0_, and not _-O3_, yields the fastest program execution time (with this microbenchmark program at least):

```
$ time julia -O0 random_streams_for_perf_stats.jl
...
real	0m0.173s
...
$ 
```

### Julia and Python

I gave transpiler [py2many](https://github.com/py2many/py2many) a chance to transpile my original [Python program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_bitstring_and_flexible_password_generator.py) quickly into a Julia program. But this was a dud. Then, I manually transpiled everything piece by piece from Python.

Yes, there's a certain nearness beteen both languages, but one must be careful with **indices** in Julia, which are starting with 1, and not 0 like in Python. This sounds more trivial than it actually is from my point of view, because this feature also affects comparisons, which should be re-tested carefully after a Python to Julia transpilation.

<br/>

### Building a "standalone" app from a Julia program

TL;DR: building a "standalone" executable in Linux is possible with Julia, but Julia is not (yet) really made for this kind of task. And such an app, at least how I've done it, is not really standalone and distributable within one executable file.

After some experimentation in vain, I discovered this very helpful article: [5. Creating Packages](https://engee.com/helpcenter/stable/en/julia/Pkg/creating-packages.html)

However, it was only this prompt: "Julia UndefVarError: 'julia_main' not defined, standalone application for Linux" for Google AI, which gave me essential information. I've also roughly provided the workflow in the [app related source code file](./random_streams_for_perf_stats_app.jl), and which I present below.

Everything starts with correctly defining a suitable entry point. This is done with function _julia_main()::Cint_

```
module random_streams_for_perf_stats_app

    import Random

    function julia_main()::Cint
        try
            # Your application logic here
            real_main()
        catch
            Base.invokelatest(Base.display_error, Base.catch_stack())
            return 1 # Return non-zero on error
        end
        return 0 # Return 0 for success
    end

    function real_main()
        END = 62501  # 62501 for exactly 1M binary digits; const END has no speed effect here
        ...
    end  # function real_main()
end  # module
```

The whole app is wrapped into a module, here named _random_streams_for_perf_stats_app_.

Very important: correctly handling the dependencies of imported modules, here module _Random_ with its _rand()_ function

Function _julia_main()::Cint_ then tries to call function _real_main()_. Actually, original program [random_streams_for_perf_stats.jl](./random_streams_for_perf_stats.jl), with the exception of _using Random_, has been put into the _real_main()_ function.

That's it for the configuration. Everything else can from now on be done within the Julia REPL (Read-Eval-Print Loop) and package manager _pkg_, which is invoked from the Julia REPL.

<br/>

So, let's start the Julia REPL:

```
$ julia
julia>
```

At the Julia prompt, activate the package manager. This is done with entering the _]_ character (and may need some exercise). 

At the package manager, we now generate a little directory structure with the name of our app, and which includes a "Hello, World!" source code example (however without the needed  _julia_main()::Cint_ function), and finally add the _Random_ dependency:

```
julia> ]  # entering the package manager
(@v1.12) pkg> 
(@v1.12) pkg> generate random_streams_for_perf_stats_app
(@v1.12) pkg> activate ./random_streams_for_perf_stats_app
(@v1.12) pkg> add Random
(@v1.12) pkg>
```

Now, we leave the package manager by pressing the [CTRL] and [C] keys:

```
(@v1.12) pkg> [CTRL+C]
julia>
```

In the meanwhile, copy source code file _random_streams_for_perf_stats_app.jl_ into subdirectory _./random_streams_for_perf_stats_app/src/_, where it replaces the generated "Hello, World!" source code example.

Back in in the Julia REPL (started like before from the same working directory), we can now test this application by calling the _julia_main()_ function: 

```
julia> import random_streams_for_perf_stats_app
julia> random_streams_for_perf_stats_app.julia_main()

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
0

julia>
```

<br/>

An important aspect of software development is **changing source code**. There are helpers in Julia, like [Revise](https://timholy.github.io/Revise.jl/stable/#Introduction-to-Revise) for example, and which I didn't test, but this is a weak point of Julia REPL from my point of view. The easiest way I found was just starting over with the Julia REPL after **every change of source code**. So, basic software development in Julia may better start outside of the Julia REPL with using the "normal" _$ julia \<~.jl\>_ compiler first.

<br/>

Inside the Julia REPL, we change into project directory _random_streams_for_perf_stats_app_, where we install the [PackageCompiler](https://julialang.github.io/PackageCompiler.jl/dev/) (see also from here: https://github.com/JuliaLang/PackageCompiler.jl):

```
julia> cd("./random_streams_for_perf_stats_app")
julia> using Pkg
julia> Pkg.activate(".")
  Activating project at `~/scripts/Julia/random_streams_for_perf_stats_app`

julia> Pkg.add("PackageCompiler")
...
julia> 
```

Now, we compile the standalone executable, mostly by calling the [create_app](https://julialang.github.io/PackageCompiler.jl/dev/refs.html#PackageCompiler.create_app) function of the _PackageCompiler_ with some arguments:

```
julia> using PackageCompiler
julia> create_app(
".", # project folder
"build/random_streams_for_perf_stats_app"; # output folder
force=true
)
...
```

This building will take minutes, so be patient:

```
...
  Total library file size: 284.981 MiB
✔ [04m:25s] PackageCompiler: creating compiler sysimage (incremental=false)
✔ [02m:09s] PackageCompiler: compiling fresh sysimage (incremental=false)
Precompiling Pkg finished.
  31 dependencies successfully precompiled in 16 seconds
Precompiling packages finished.
  6 dependencies successfully precompiled in 2 seconds. 31 already precompiled.
✔ [02m:51s] PackageCompiler: compiling nonincremental system image

julia>
```

Finally, we leave the Julia REPL, and test the app from the Linux shell, here starting from the working directory:
```
julia> [CTRL+Z]
$
$ ./random_streams_for_perf_stats_app/build/random_streams_for_perf_stats_app/bin/random_streams_for_perf_stats_app

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

Voilà!

<br/>

However, the execution time here is around sobering 300 milliseconds, which is way higher than just using the julia compiler resulting in around 170 milliseconds!

My suspicion is that all this precompilation isn't as effective as _julia's_ just-in-time compilation.

<br/>

Otherwise, this compilation created 85 files in 8 folders in the generated _build_ subdirectory tree, which has a staggering size of 1.2GB!

The most important and generated project configuration files are:

- _Manifest.toml_
- _Project.toml_

With the workflow as described above, there's no need to manually modify these two files.

"Standalone" executable _./random_streams_for_perf_stats_app/build/random_streams_for_perf_stats_app/bin/random_streams_for_perf_stats_app_ cannot be just copied to another Linux system to work there, because it has many dependencies, that is shared object files, located in the _build_ subdirectory tree. So, to make program _random_streams_for_perf_stats_app_ work in another Linux system, just copy the whole _build_ subdirectory to it.

<br/>

### Further experiments for a "standalone" app

Here's a recent article about making "standalone" executables in Julia, which makes use of the [JuliaC](https://github.com/JuliaLang/JuliaC.jl?tab=readme-ov-file#juliac) package:

November 4, 2025: [Julia 1.12 brings progress on standalone binaries and more](https://lwn.net/Articles/1044280/)

See also from here: [New --trim feature](https://julialang.org/blog/2025/10/julia-1.12-highlights/#new_--trim_feature) (*)

Basically, the procedure for making the _AppProject_ with JuliaC as described in (*) is working according to my test, but the microbenchmark program, and there only the [speed part](./random_streams_for_perf_stats.jl), is too much for it.

The _juliac_ compiler cannot take function _lpad()_, and others, without needed modifications, modifications which are unknown to me. See from (*) how the _println()_ function has been expanded to:

```
    println(Core.stdout, "Hello World!")
```

However, when I omit the _--trim_ feature, which is of course the whole point here, the modified program (see at (*)) can be compiled successfully, but executing it will cause a fatal error.

<br/>

There are also other helpers, like the [StaticCompiler](https://github.com/tshort/StaticCompiler.jl#staticcompiler), which is also not working for the microbenchmark program.

It also needs modified function calls, like this one for example:

```
    ...
    println(c"Hello, World!")
    ...
```

..with an essential _c_ qualifier (_println("Hello, World!")_ is not working here according to my experiments), and which probably signifies that the following string should be treated as a character string.

However, this "Example 1: Basic Hello World" from [Julia, my love!](https://joel.id/julia-my-love/) from 23 Nov 2025 worked with me; the rest not so much.

What's notable in this article is chapter "Benchmark results (1000x1000 matrices):". It says that "Benchmark: Matrix Multiplication" runs for 1435 milliseconds in the "Compiled Julia" version (that's probably with using the _StaticCompiler_) on that testing system, but 1425 milliseconds with the "Julia (runtime)", which is the "normal" _julia_ compiler I guess.

<br/>

##_end
