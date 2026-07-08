2026-07-09: work in progress

<br/>

# Modula-3

Here in the still actively maintained **Critical Mass Modula-3** implementation: https://github.com/modula3/cm3

<br/>

## Installation tips

I've been following these instructions: [Getting Started: Linux](https://github.com/modula3/cm3/wiki/Getting-Started%3A-Linux), and have done it like this:

```
$ sudo apt-get install build-essential cmake python3  # installing prerequisites
...
$ sudo apt-get install libglu1-mesa-dev xorg-dev  # prerequisites for the full system, which is being built here
...
$ curl -LO https://github.com/modula3/cm3/releases/download/d5.11.4/cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
...
$ tar xf cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
$ mkdir build
$ cd build
$ ../cm3-dist-AMD64_LINUX-d5.11.4/scripts/concierge.py install --prefix $HOME/cm3 all
...  # this may take a while!
$ cm3 --version
Critical Mass Modula-3 version d5.11.4
  GitInfo: unknown
  last updated: 2021-10-07
  compiled: 2026-07- 9 00:10:50
  configuration: /home/booser/cm3/bin/cm3.cfg
  host: AMD64_LINUX
  target: AMD64_LINUX

$
```

~/.bashrc: _export PATH="$HOME/cm3/bin:$PATH"_

tbd

<br/>

Upgrading to latest version xxx (as of 2026-07-09):

tbd




##_end
