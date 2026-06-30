2026-06-30: work in progress: tbd

<br/>

# Hy

https://hylang.org/

https://github.com/hylang/hy

<br/>

What [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#clojure) aims to be for the Java Virtual Machine, Hy aims to be for Python. (*)

<br/>

## Installation tips

In Ubuntu 24 LTS at least, just installing Hy like officially advised (*) as: _$ pip3 install --user hy_ is not working! (directly into Ubuntu's sensitive Python installation even...)

And this has to do with Ubuntu 24's nearness to its default Python 3.12 installation.

After elaborate experimentation, I can say:

- latest Hy version 1.3.0 (as of 2026-06-30) is working fine with Python version 3.12, and also Python version 3.11 (but not Python version 3.14 for example)
- the easiest path to a working Hy installation in an Ubuntu Linux system is just installing it like usual, by default into directory _/usr/bin/_:

```
$ sudo apt install hy
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
The following NEW packages will be installed:
  hy
0 upgraded, 1 newly installed, 0 to remove and 7 not upgraded.
Need to get 8,890 B of archives.
After this operation, 33.8 kB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu noble/universe amd64 hy all 0.28.0-1 [8,890 B]
Fetched 8,890 B in 0s (106 kB/s)
Selecting previously unselected package hy.
(Reading database ... 367591 files and directories currently installed.)
Preparing to unpack .../archives/hy_0.28.0-1_all.deb ...
Unpacking hy (0.28.0-1) ...
Setting up hy (0.28.0-1) ...
update-alternatives: using /usr/bin/hy3 to provide /usr/bin/hy (hy) in auto mode
Processing triggers for man-db (2.12.0-4build2) ...
$ whereis hy  # installation OK?
hy: /usr/bin/hy /usr/share/man/man1/hy.1.gz
$ hy  # just entering a little REPL test
Hy 0.28.0 using CPython(main) 3.12.3 on Linux
=> (quit)  # ..and exiting
$ 
```

<br/>

Finally, let's make the all important "Hello, world from Hy!" source code file test:

```
$ echo '(print "Hello, world from Hy!")' > hello_world.hy
$ hy hello_world.hy
Hello, world from Hy!
$
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

### Program factorial.hy for terminal input and output

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
$ hy factorial.hy
Enter an integer n >= 1: 5
factorial(5) = 120
$ 
```

<br/>

### Microbenchmark program in Hy

Interestingly, with about 190 milliseconds (_multitime -n 20 hy random_streams_for_perf_stats.hy_) of execution time, the Hy program runs substantially faster than its counterpart in [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj) with about 420 milliseconds in an uberJAR file in the OpenJDK Runtime Environment version 25. And this while the Clojure program is using Java's _StringBuilder_ class, while Python's string builder _StringIO()_ isn't improving the Hy program's execution speed.




tbd



<br/>

##_end
