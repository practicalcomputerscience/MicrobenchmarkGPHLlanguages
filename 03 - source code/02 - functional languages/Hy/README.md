# Hy

https://hylang.org/

https://github.com/hylang/hy

https://pypi.org/project/hy/

<br/>

What [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#clojure) aims to be for the Java Virtual Machine, Hy aims to be for Python. (*)

<br/>

[Why Hy?](https://hylang.org/hy/doc/v1.0.0/whyhy)

> ..named after the insect order Hymenoptera, since Paul Tagliamonte was studying swarm behavior when he created the language..

---

Table of contents:

- [Installation tips](#installation-tips)
- [Program factorial.hy for terminal input and output](#program-factorialhy-for-terminal-input-and-output)
- [Microbenchmark program in Hy](#microbenchmark-program-in-hy)

<br/>

---

## Installation tips

In Ubuntu 24 LTS at least, just installing Hy like officially advised (*) as: _$ pip3 install --user hy_ is not working! ("error: externally-managed-environment")

This has to do with Ubuntu 24's nearness to its default Python 3.12 installation. In this case it's always best to create a dedicated virtual Python environment, see at [Python environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#python-environments).

(In Ubuntu 24 LTS, installing Hy system-wide with _$ sudo apt install hy_ will get you a working, but very old Hy version with 0.28.0!)

So, in Ubuntu 24 LTS, do this:

```
$ cd ./Python/_virtual_envs
$ python3 -m venv ./Hy
$ cd Hy
$ source ./bin/activate
(Hy) $ pip install hy
Collecting hy
  Using cached hy-1.3.0-py3-none-any.whl
Collecting funcparserlib~=1.0 (from hy)
  Using cached funcparserlib-1.0.1-py2.py3-none-any.whl.metadata (7.1 kB)
Using cached funcparserlib-1.0.1-py2.py3-none-any.whl (17 kB)
Installing collected packages: funcparserlib, hy
Successfully installed funcparserlib-1.0.1 hy-1.3.0
(Hy) $ hy --version
hy 1.3.0
(Hy) $ hy  # just entering a little REPL test
Hy 1.3.0 (Dogs Should Be Raw) using CPython(main) 3.12.3 on Linux
=> (quit)  # ..and quitting
(Hy) $ python3 --version  # also check the Python version
Python 3.12.3
(Hy) $ deactivate  # finally, leave the virtual Python environment
$
```

<br/>

Let's make the all important "Hello, world from Hy!" source code file test:

```
(Hy) $ echo '(print "Hello, world from Hy!")' > hello_world.hy
(Hy) $ hy -m hello_world  # run Hy module hello_world
Hello, world from Hy!
(Hy) $
```

> [!IMPORTANT]
> In Ubuntu 24 LTS at least, you must run your Hy source code file without its file extension .hy! Hy expects a module name, not a file name!

(otherwise it would cause an error like this:
```
(Hy) $ hy hello_world.hy
Traceback (most recent call last):
  File "~/scripts/Python/_virtual_envs/Hy/bin/hy", line 8, in <module>
    sys.exit(hy_main())
             ^^^^^^^^^
  File "<frozen runpy>", line 285, in run_path
ValueError: too many values to unpack (expected 1)
(Hy) $
```

<br/>

By the way: installed Python versions can be quickly looked up like this usually:

```
$ ls /usr/bin/python* -l
lrwxrwxrwx 1 root root      10 Nov 12  2025 /usr/bin/python3 -> python3.12
-rwxr-xr-x 1 root root 6639992 Mar  3 10:26 /usr/bin/python3.11
-rwxr-xr-x 1 root root 8020928 Mar 23 20:04 /usr/bin/python3.12
lrwxrwxrwx 1 root root      34 Mar 23 20:04 /usr/bin/python3.12-config -> x86_64-linux-gnu-python3.12-config
-rwxr-xr-x 1 root root 6949608 Feb  4 10:28 /usr/bin/python3.14
lrwxrwxrwx 1 root root      17 Nov 12  2025 /usr/bin/python3-config -> python3.12-config
$ 
```

<br/>

## Program factorial.hy for terminal input and output

Appraisal (*) makes hope that transpiling the [Clojure solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj) into a Hy solution should work rather smoothly.

But first make a little Hy program named _factorial.hy_, with some help from "Big AI", which tests input and output operations on the terminal, often a critical thing with functional programming:

```
(import sys)

(defn factorial [n]
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(while True
  (try
    (setv user_input (input "Enter an integer n >= 1: "))  ; USER INPUT FROM THE TERMINAL
    (setv n (int user_input))  ; try to turn input string into an integer number

    (if (< n 1)
        (print "Call program with an integer number >= 1")
        (do
          (print "factorial(" (str n) ") = " (str (factorial n)))  ; CONCATENATED OUTPUT TO THE TERMINAL
          (break)))
    (except [e Exception]
      (print "Call program with an integer number >= 1"))))
```

Let's run _factorial.hy_:

```
(Hy) $ hy -m factorial  # again, leave .hy away!
Enter an integer n >= 1: 5
factorial(5) = 120
(Hy) $ 
```

<br/>

## Microbenchmark program in Hy

With about 85 milliseconds (_(Hy) $ multitime -n 20 hy -m random_streams_for_perf_stats_) of execution time, the Hy program runs substantially faster than its counterpart in [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj) with about 420 milliseconds in an uberJAR file in the OpenJDK Runtime Environment version 25, and using Java's _StringBuilder_ class.

And also faster than its [Python counterpart](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py), also in a Python 3.12.3 virtual environment, with about 139 milliseconds!

Using Python's string builder _StringIO()_ in the Hy script wasn't really improving the program's execution speed.

Notable: running Hy script _random_streams_for_perf_stats.hy_ for example like this: _$ /usr/bin/python3.12 -m hy -m random_streams_for_perf_stats_ yields a significantly higher execution time of about 187 milliseconds!

And the reason is this: at least on my Ubuntu 24 LTS system, running _$ /usr/bin/python3.xx -m hy -m <Hy module>_ is automatically using this very old Hy version already mentioned above:

```
$ python3 -c "import hy; print(hy.__version__)"
0.28.0
$
```

So, use a modern Hy version in a dedicated virtual Python environment!

<br/>

## tbd

Hy command option _--spy_ works only on the REPL, where it transpiles Hy input into "equivalent Python code before executing each piece of Hy code": https://github.com/hylang/hy/blob/master/docs/cli.rst:

```
$ hy --spy 
import hy
------------------------------
Hy 1.3.0 (Dogs Should Be Raw) using CPython(main) 3.12.3 on Linux
=> (defn factorial [n]
...   (if (= n 0)
...     1
...     (* n (factorial (- n 1)))))
def factorial(n):
    return 1 if n == 0 else n * factorial(n - 1)
None
------------------------------
=> 
```




<br/>

##_end
