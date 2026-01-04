# Julia

https://julialang.org/

Julia Micro-Benchmarks: https://julialang.org/benchmarks/

<br/>

Table of contents:

- [Microbenchmark program in Julia](#microbenchmark-program-in-julia)
- [Julia and Python](#julia-and-python)
- [Building a "standalone" app from a Julia program](#building-a-standalone-app-from-a-julia-program)

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

TL;DR: building a standalone executable in Linux is possible with Julia, but Julia is not really made for this kind of task. And such an app, at least how I've done it, is not really standalone and distributable within one executable file.

After some experimentation in vain, I discovered this very helpful article: [5. Creating Packages](https://engee.com/helpcenter/stable/en/julia/Pkg/creating-packages.html)

However, is was only this prompt: "Julia UndefVarError: 'julia_main' not defined, standalone application for Linux" for Google AI, which gave me essential information. I've also roughly provided the workflow in the [app related source code file](./random_streams_for_perf_stats_app.jl), and which I present below.

Everything starts with correctly preparing the source code and defining a suitable entry point. This is done with function _julia_main()::Cint_

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

Actually, that's it for the configuration. Everything else can now be done with the Julia REPL (Read-Eval-Print Loop) and package manager _pkg_, which is invoked from the Julia REPL.

<br/>

So, let's start the Julia REPL:

```
$ julia
julia>
```

At the Julia prompt, activate the package manager. This is done with entering the _]_ character (and may need some exercise). 

At the package manager, we now generate a little directory structure with the name of our app, and which includes a "Hello, World!" source code example (however without the needed  _julia_main()::Cint_ function), and finally add the Random dependency:

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

An important aspect of software development is **changing source code**. There are helpers in Julia ([TBD] or [TBD] for example, which I didn't test), but this is a weak point of Julia from my point of view. The easiest way I found was just starting over with the Julia REPL after **every change of source code**. So, basic software development in Julia may better start outside of the Julia REPL with using the "normal" _$ julia \<~.jl\>_ compiler first.




<br/>

TBD



##_end
