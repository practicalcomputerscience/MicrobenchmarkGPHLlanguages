# Gambit Scheme

https://gambitscheme.org/

---

Gambit Scheme is quasi the Canadian branch of Scheme and is maintained since many years by Marc Feeley from Université de Montréal: https://diro.umontreal.ca/english/department-directory/professors/professor/in/in14344/sg/Marc%20Feeley/

It's the basis of at least another Scheme dialect, that is [Gerbil Scheme](https://cons.io/).

<br/>

## Installation tips

Get tarball file _gambit-4.9.7.tar.gz_ (as of December 2025) from here: https://github.com/gambit/gambit/tags, extract it and change into extracted directory _./gambit-4.9.7_

There do:

```
$ ./configure --enable-single-host  # only this option worked OK with my system
$ make  # this may take some time
$ make check
$ make modules  # optional, but I did this too; this may also take some time
$ sudo make install
```

At last, fix your _~/.bashrc_ config file for pathes to subdirectories _./gsi_ (for the REPL) and _./gsc_ (for the compiler) in extracted directory _./gambit-4.9.7_.

Then, have a little test:

```
$ gsc -v
v4.9.7 20250713105902 x86_64-pc-linux-gnu "./configure '--enable-single-host'"
$
```

<br/>

##_end
