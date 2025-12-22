# CHICKEN Scheme

https://www.call-cc.org/

---

## Installation tips

Get tarball file _chicken-5.4.0.tar.gz_ (as of December 2025) from here: https://code.call-cc.org/, extract it and change into extracted directory _./chicken-5.4.0_

There do:

```
$ make  # this may take some time
$ sudo make install
$ sudo ./chicken-install srfi-152  # this library is needed for this script; this may also take some time
$ csc -version  # test version info
CHICKEN
(c) 2008-2022, The CHICKEN Team
(c) 2000-2007, Felix L. Winkelmann
Version 5.4.0 (rev 1a1d1495)
linux-unix-gnu-x86-64 [ 64bit dload ptables ]

$
```

<br/>

Otherweise, CHICKEN Scheme compiles to very small binary executables, for example like this:

```
$ csc -O5 ./random_streams_for_perf_stats.scm
```

..with switch _-O5_ for best opimization: [Basic command-line options](https://wiki.call-cc.org/man/5/Using%20the%20compiler#basic-command-line-options)

<br/>

Enter the REPL (Read-Eval-Print Loop) like this: _$ csi_ and give command _(exit)_ to exit it.

<br/>

srfi-152 is the streamlined string library [SRFI 152](https://api.call-cc.org/5/doc/srfi-152) to use function [string-concatenate](https://api.call-cc.org/5/doc/srfi-152#def:string-concatenate).

<br/>

##_end
