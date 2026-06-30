2026-06-30: work in progress: tbd


# Hy

https://hylang.org/

https://github.com/hylang/hy

Hy is a Lisp dialect that's embedded in Python.

<br/>

## Installation tips

In Ubuntu 24 LTS at least, just installing Hy like officially advised as: _$ pip3 install --user hy_ is not working! (directly into Ubuntu's sensitive Python installation even...)

And the first reason is that Ubuntu 24's native Python version 3.12 is just not compatible with latest Hy version 1.3.0:

```
$ hy ./hello_world.hy
Traceback (most recent call last):
  File "~/.local/bin/hy", line 6, in <module>
    sys.exit(hy_main())
             ^^^^^^^^^
  File "<frozen runpy>", line 285, in run_path
ValueError: too many values to unpack (expected 1)
$
```

<br/>

So, this means that I have to install **Python version 3.11** additionally in my Ubuntu 24 LTS system:

```
$ sudo add-apt-repository ppa:deadsnakes/ppa
...
$ sudo apt update
...
$ sudo apt install python3.11
...
$ python3.11 --version  # the just installed Python version
Python 3.11.15
$ python3 --version  # Ubuntu 24's Python version
Python 3.12.3
$ python --version  # also Ubuntu 24's Python version
Python 3.12.3
$
```

<br/>

Now back to step one with command: _$ pip3 install --user hy_

If this command isn't installing the Hy interpreter in directory _$HOME/.local/bin_, then because this directory may be owned by the root user,
something which has to be changed:

```
$ ls ~/.local/bin -l
total 121200
-rwxr-xr-x 1 root   root        3477 Dec  4  2025 gcore
-rwxr-xr-x 1 root   root   118142048 Dec  4  2025 gdb
-rwxr-xr-x 1 root   root        4587 Dec  4  2025 gdb-add-index
-rwxr-xr-x 1 root   root     5934352 Dec  4  2025 gdbserver
drwxr-xr-x 2 root   root        4096 Feb 18 01:26 uv
$
```

So, I changed ownership of this directory, while hoping not to break other things:

```
$ sudo chown -R "$USER":"$USER" ~/.local/bin
$ chmod u+rwx ~/.local/bin
```

Only now, I could install Hy as advised:

```
$ python3 -m pip install --user hy
Collecting hy
  Using cached hy-1.3.0-py3-none-any.whl
Collecting funcparserlib~=1.0 (from hy)
  Using cached funcparserlib-1.0.1-py2.py3-none-any.whl.metadata (7.1 kB)
Using cached funcparserlib-1.0.1-py2.py3-none-any.whl (17 kB)
Installing collected packages: funcparserlib, hy
Successfully installed funcparserlib-1.0.1 hy-1.3.0
$ hy  # entering the Hy REPL as a test
Hy 1.3.0 (Dogs Should Be Raw) using CPython(main) 3.12.3 on Linux  # command hy uses Ubuntu's Python installation!
=> (quit)  # quit the Hy REPL
$
```

Finally, let's make the all important "Hello, world from Hy!" test, now directly with Python 3.11:

```
$ echo '(print "Hello, world from Hy!")' > hello_world.hy
$ python3.11 -m hy hello_world.hy
Hello, world from Hy!
$
```

Not working as advertised but working. (You may try command _$ python3.12 -m hy hello_world.hy_, but probably you can guess the result..)

<br/>






tbd


<br/>

##_end
