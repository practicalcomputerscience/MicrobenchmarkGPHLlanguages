# Python

https://www.python.org/

---

<br/>

## On Python environments

> [!IMPORTANT]
> (Usually) don't run Python scripts "naked" with the Python installation that has been shipped with your Linux operating system!

That is the operating system's Python, not yours.

So, the first development job should be to create a dedicated virtual [Python environment](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#python-environments) before starting Python coding.

<br/>

### Version experiments and just-in-time (JIT) compilation in Python

Both execution time measurements happen in the same (Ubuntu 24 LTS) system, within different virtual environments with different Python 3 versions:

Python version | mean of _multitime -n 20_ command | comment
--- | --- | ---
3.12.3 | 141 milliseconds | my official version and environment
3.14.3 | 182 milliseconds | executed with: _$ multitime -n 20 ./bin/python3 ./random_streams_for_perf_stats.py_

<br/>

One of the bigger changes from Python 3.12 to Python 3.14 was the gradual introduction of JIT compilation: [Binary releases for the experimental just-in-time compiler](https://docs.python.org/3/whatsnew/3.14.html#whatsnew314-jit-compiler).

However, see from: [The best new features and fixes in Python 3.14](https://www.infoworld.com/article/3975624/the-best-new-features-and-fixes-in-python-3-14.html) from Oct 7, 2025:

> [!IMPORTANT]
> Note that the JIT is not yet available in a free-threaded build. Right now you can have one or the other enabled, but not both.

This command, depending on your environment, let's you check if a Python version is free-threaded (in version 3.12 there was no feature like that), which is the case here:

```
$ $HOME/.pyenv/versions/3.14.3/bin/python3.14 -c "import sys; print(sys._is_gil_enabled())"
True
$ 
```

That's the end of my Python version 3.14.3 experiments with JIT. I'm not going to build a non-free-threaded version 3.14.3, when JIT is still officially marked as experimental (which can be seen for example when building version 3.14.3 from sources with LLVM version 19).

<br/>

Otherwise, it looks like that heavy string concatenation still runs faster in single-threaded Python version 3.12.3 than free-threaded version 3.14.3.

There are other benchmarks which show the same phenomenon: https://www.phoronix.com/review/python-314-benchmarks/3

<br/>

##_end
