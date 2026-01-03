# Julia

https://julialang.org/

Julia Micro-Benchmarks: https://julialang.org/benchmarks/

<br/>

---

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

##_end
