2026-07-01: work in progress: tbd

<br/>

# Forth

here in the [Gforth](https://gforth.org/) implementation.

<br/>

## Installation tips

After some experimentation, I noticed that I need a working Gforth implementation to build the latest version of Gforth! So, I did like this:

```
$ sudo apt  install gforth
...
$ gforth --version
gforth 0.7.3
$
```

Do _$ make clean >/dev/null 2>&1 || true_, when you have messed up a build before.

That's good enough to build latest version 0.7.9 from sources in tarball file _gforth.tar.xz_ from here: https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/current/

After unzipping that file, I followed instructions as given in _./gforth/gforth-0.7.9_20260610/INSTALL.md_:

```
$ cd ./gforth/gforth-0.7.9_20260610
$ BUILD_FROM=tarball
$ source ./install-deps.sh
...
The following packages have unmet dependencies:
...
$ 
```

I was still missing some packages, which I installed like this, and further ignored above warnings (for those packages in their newest form anyway):

```
$ sudo apt install libtool libtool-bin swig
...
$
```

Then I noticed that I had to do more installations:

```
$ sudo ./install-swig.sh
...
make[1]: Entering directory '~/scripts/Forth/gforth/gforth-0.7.9_20260610/swig/CCache'
Installing ccache-swig
Installing /usr/local/bin/ccache-swig-forth
/usr/bin/install -c -d /usr/local/bin
/usr/bin/install -c -m 755 ccache-swig /usr/local/bin/`echo ccache-swig | sed 's&$&-forth&'`
make[1]: Leaving directory '~/scripts/Forth/gforth/gforth-0.7.9_20260610/swig/CCache'
Installation complete
$
```

Only then I could run the _configure_ command correctly, and start building with _make_:

```
$ ./configure 
...

*** Config summary: everything fine ***
$ make
...
make[1]: Leaving directory '~/scripts/Forth/gforth/gforth-0.7.9_20260610/doc'
EMACS start-gforth.el
/bin/sh: 1: no: not found
make[1]: Entering directory '~/scripts/Forth/gforth/gforth-0.7.9_20260610'
make[1]: Leaving directory '~/scripts/Forth/gforth/gforth-0.7.9_20260610'
*** Check successful ***
*** no performance problems ***
    in gforth-fast
*** no performance problems ***
    in gforth
*** no performance problems ***
    in libgforth-fast
*** no performance problems ***
    in libgforth
$ gforth --version
gforth 0.7.9_20260610 amd64
$ 
```

Voilà! Gforth in its latest version!

<br/>

### Advanced exercise: factorial

I highly recommend to first have a look into the official [Forth Tutorial](https://net2o.de/gforth/Tutorial.html) before doing anything more meaningful than "Hello, world!" in Forth:

```
$ gforth
Gforth 0.7.3, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
cr ." Hello, world from Gforth!" cr 
Hello, world from Gforth!
 ok
bye
$
```

Otherwise, there's a high chance that you can't really understand any "Big AI" transpiled or generated Gforth code (unless you are the coding genius of your neighborhood, of course).

tbd












##_end
