# Python

https://www.python.org/

---

Table of contents:

- [On Python environments](#on-python-environments)
- [Version experiments and just-in-time (JIT) compilation in Python](#version-experiments-and-just-in-time-jit-compilation-in-python)
- [SPy: statically compilable Python](#spy-statically-compilable-python)

<br/>

---

## On Python environments

> [!IMPORTANT]
> (Usually) don't run Python scripts "naked" with the Python installation that has been shipped with your Linux operating system!

That is the operating system's Python, not yours.

So, the first development job should be to create a dedicated virtual [Python environment](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#python-environments) before starting Python coding.

<br/>

### Version experiments and just-in-time (JIT) compilation in Python

Both execution time measurements happen in the same (Ubuntu 24 LTS) system with different Python 3 versions:

Python version | mean of _multitime -n 20_ command | comment
--- | --- | ---
3.12.3 | 142 milliseconds | my official version and environment (with python3 linked to python 3.12): _$ multitime -n 20 python3.12 ./random_streams_for_perf_stats.py_
3.14.3 | 179 milliseconds | executed with: _$ multitime -n 20 $HOME/.pyenv/versions/3.14.3/bin/python3.14 ./random_streams_for_perf_stats.py_

<br/>

One of the bigger changes from Python 3.12 to Python 3.14 was the gradual introduction of JIT compilation: [Binary releases for the experimental just-in-time compiler](https://docs.python.org/3/whatsnew/3.14.html#whatsnew314-jit-compiler).

However, see from: [The best new features and fixes in Python 3.14](https://www.infoworld.com/article/3975624/the-best-new-features-and-fixes-in-python-3-14.html) from Oct 7, 2025:

> [!IMPORTANT]
> Note that the JIT is not yet available in a free-threaded build. Right now you can have one or the other enabled, but not both.

Here are two checks for your Python's current capabilities, here non-free-threaded and no JIT:

```
$ $HOME/.pyenv/versions/3.14.3/bin/python3.14 -c "import sys; print(sys._is_gil_enabled())"
True
$ $HOME/.pyenv/versions/3.14.3/bin/python3.14 -c "import sys; print(sys._jit.is_enabled())"
False
$ 
```

That's the end of my Python version 3.14.3 experiments with JIT. I'm not going to build a non-free-threaded version 3.14.3 from sources, when JIT is still officially marked as experimental (which can be seen for example when building version 3.14.3 from sources with LLVM version 19).

<br/>

Otherwise, it looks like that heavy string concatenation still runs faster in single-threaded Python version 3.12.3 than single-threaded version 3.14.3.

There are other benchmarks which show the same phenomenon (though, I don't know the exact Python 3.14 configuration here, other than older version 3.14.0 has been used): https://www.phoronix.com/review/python-314-benchmarks/3

<br/>

Building a JIT-capable version of Python 3.14.3 would take this:

first, install LLVM version 19 if not done yet:

```
$ sudo apt install llvm-19 llvm-19-dev clang-19
...
$ ./configure --enable-experimental-jit=yes-off --enable-optimizations --with-lto
...
$
```

_--enable-experimental-jit=yes-off_ is for a JIT configuration which is not on by default.

I'm not really sure about switch _--with-lto_, though it should not block any JIT capability. It's for link time optimization, where compiling (_make_) would take much more time than without this switch. In this case, do the make with all CPU cores support:

```
$ make -j$(nproc)  # with nproc from the GNU core utilities
...
$ sudo make install
...
$
```

The new Python executable should then be found here: _/usr/local/bin/python3.14_

Then do:

```
$ export PYTHON_JIT=1
$ /usr/local/bin/python3.14 -c "import sys; print(sys._jit.is_enabled())"
True  # JIT should be on now
$
```

<br/>

### SPy: statically compilable Python

> SPy is a variant of Python specifically designed to be statically compilable while retaining a lot of the "useful" dynamic parts of Python.

from: https://github.com/spylang/spy

<br/>

[Inside SPy🥸, part 1: Motivations and Goals](https://antocuni.eu/2025/10/29/inside-spy-part-1-motivations-and-goals/?utm_source=www.pythonweekly.com&utm_medium=newsletter&utm_campaign=python-weekly-issue-720-october-30-2025&_bhlid=5644a7fa95afb3696ed9c70acf4712f2ef0bf95f#inside-spy-part-1-motivations-and-goals) from Oct 29, 2025:

> Thanks to my work on PyPy, I came to the conclusion that Python is fundamentally impossible to optimize to the level of performance which I aim for. ... The first problem is that Python is extremely dynamic.

<br/>

##_end
