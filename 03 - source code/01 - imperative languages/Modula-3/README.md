2026-07-09: work in progress

<br/>

# Modula-3

Here in the still actively maintained **Critical Mass Modula-3** implementation: https://github.com/modula3/cm3

<br/>

## Installation tips

After some experimentation, I can say that these instructions: [Getting Started: Linux](https://github.com/modula3/cm3/wiki/Getting-Started%3A-Linux), only work with that older version d5.11.4.

Later bootstrap versions, like now _cm3-boot-AMD64_LINUX-d5.12.0.tar.xz_ (as of 2026-07-09) from here: https://github.com/modula3/cm3/releases/tag/d5.12.0, work without a Python installation script,
but with the cmake tool.

So, do it like this:

```
$ sudo apt-get install build-essential cmake python3  # installing prerequisites
...
$ sudo apt-get install libglu1-mesa-dev xorg-dev  # prerequisites for the full system, which is being built here
...
$ tar xf cm3-boot-AMD64_LINUX-d5.12.0.tar.xz  # unpack tarball file; this command creates directory ./bootstrap without any version info
$ mkdir build
$ cd build
$ cmake -DCMAKE_INSTALL_PREFIX=$HOME/cm3 ../bootstrap
-- The C compiler identification is GNU 13.3.0
-- The CXX compiler identification is GNU 13.3.0
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Check for working C compiler: /usr/bin/cc - skipped
-- Detecting C compile features
-- Detecting C compile features - done
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Performing Test CMAKE_HAVE_LIBC_PTHREAD
-- Performing Test CMAKE_HAVE_LIBC_PTHREAD - Success
-- Found Threads: TRUE  
-- Configuring done (0.3s)
-- Generating done (0.1s)
-- Build files have been written to: ~/scripts/Modula-3/build
$ cmake --build .  
...  # this may take a while!
[100%] Built target cm3
[100%] Building CXX object mklib/CMakeFiles/mklib.dir/Main.m3.cpp.o
[100%] Building CXX object mklib/CMakeFiles/mklib.dir/_m3main.cpp.o
[100%] Linking CXX executable mklib
[100%] Built target mklib
$ cmake --install .
-- Install configuration: ""
-- Installing: ~/cm3/bin/cm3
-- Installing: ~/cm3/bin/mklib
$ 
```

Then I expanded my _~/.bashrc_ configuration file with line: _export PATH="$HOME/cm3/bin:$PATH"_, and activated it with _$ source ~/.bashrc_ to make a version test:

```
$ cm3 --version
Critical Mass Modula-3 version d5.11.9
  GitInfo: https://github.com/modula3/cm3/commit/121eb47b561061d7b6f8aa845d237d919a971ced
           https://github.com/modula3/cm3/commits/master
           remote: https://github.com/modula3/cm3
           revision: 121eb47b561061d7b6f8aa845d237d919a971ced
           branch: master
  last updated: 2022-02-05
  compiled: 2026-07- 9 21:25:17
  configuration: ~/cm3/bin/cm3.cfg
  host: AMD64_LINUX
  target: AMD64_LINUX

$
```

Not a typo. This is actually version d5.11.9. Anyway, Critical Mass Modula-3 should be in a form to work with it.

<br/>




tbd



<br/>

##_end
