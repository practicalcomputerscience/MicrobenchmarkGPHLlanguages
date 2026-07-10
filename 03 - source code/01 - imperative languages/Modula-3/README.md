2026-07-09: work in progress

<br/>

# Modula-3

Here in the still actively maintained **Critical Mass Modula-3** implementation: https://github.com/modula3/cm3

<br/>

## Installation tips

> [!WARNING]
> After some experimentation, I can say that initially anything else than following these official instructions: [Getting Started: Linux](https://github.com/modula3/cm3/wiki/Getting-Started%3A-Linux) for older version d5.11.4 is a hot mess! (unless you are an expert of course)

So, this is what I did to get a working CM3 system:

```
$ sudo apt-get install build-essential cmake python3  # installing prerequisites
...
$ sudo apt-get install libglu1-mesa-dev xorg-dev  # prerequisites for the full system, which is being built here
...
$ curl -LO https://github.com/modula3/cm3/releases/download/d5.11.4/cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
...
$ tar xf cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
$
```

Only this tarball file has the whole infrastructure included:

```
$ ls cm3-dist-AMD64_LINUX-d5.11.4
bootstrap        COPYRIGHT-COLUMBIA  COPYRIGHTS       examples                     m3-games    m3overrides  m3-ui           README-unicode-summary
caltech-other    COPYRIGHT-DEC       COPYRIGHT-XEROX  getting-started-windows.txt  m3-lectern  m3-pkgtools  m3-win          scratch
caltech-parser   COPYRIGHT-INTEL     docs             m3-comm                      m3-libs     m3-scheme    m3-www          scripts
COPYRIGHT-BSD    COPYRIGHT-JDP       elego            m3-db                        m3-mail     m3-sys       README          tools
COPYRIGHT-CMASS  COPYRIGHT-PURDUE    ESC              m3-demo                      m3-obliq    m3-tools     README-unicode
$
```

Continue with:

```
$ mkdir build
$ cd build
$ ../cm3-dist-AMD64_LINUX-d5.11.4/scripts/concierge.py install --prefix $HOME/cm3 all
...  # this may take a while!
$
```

Then I expanded my _~/.bashrc_ configuration file with line: _export PATH="$HOME/cm3/bin:$PATH"_, and activated it with _$ source ~/.bashrc_ to make a version test:

```
$ cm3 --version
Critical Mass Modula-3 version d5.11.4
  GitInfo: unknown
  last updated: 2021-10-07
  compiled: 2026-07- 9 22:12:03
  configuration: ~/cm3/bin/cm3.cfg
  host: AMD64_LINUX
  target: AMD64_LINUX

$
```

Super-important is this information: _configuration: ~/cm3/bin/cm3.cfg_ (~ denotes the home directory of the user)

If this file is missing, you got a problem and should start all over again!

<br/>

I created individual projects for both programs, the "speed part" and the complete microbenchmark. So, rename the source code and make files accordingly inside their project directories (both files go into the same root project directory):

- random_streams_for_perf_stats_Main.m3 --> Main.m3
- random_streams_for_perf_stats_m3makefile --> m3makefile
- random_bitstring_and_flexible_password_generator_Main.m3 --> Main.m3
- random_bitstring_and_flexible_password_generator_m3makefile --> m3makefile

tbd

<br/>

##_end
