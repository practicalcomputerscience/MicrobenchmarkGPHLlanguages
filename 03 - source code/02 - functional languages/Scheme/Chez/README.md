2025-12-22: work in progress

# Chez Scheme

https://cisco.github.io/ChezScheme/

See some more information from here: [Chez Scheme (CS)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/README.md#chez-scheme-cs)

pb = portable bytecode

<br/>

## Installation tips

See from file _IMPLEMENTATION.md_:

> Chez Scheme is a bootstrapped compiler, meaning you need a Chez Scheme compiler to build a Chez Scheme compiler.

Get tarball file csv10.3.0.tar.gz (as of December 2025) from here: https://github.com/cisco/ChezScheme/releases, extract it and change into extracted directory _./csv10.3.0_

There do:

```
$ ./configure
Configuring for ta6le
$ make
$ sudo make install
$ petite --version  # test version info
10.3.0
$
```

TBD

<br/>

##_end
