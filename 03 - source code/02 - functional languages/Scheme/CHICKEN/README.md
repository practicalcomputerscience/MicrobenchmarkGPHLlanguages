# CHICKEN Scheme

https://www.call-cc.org/

---

CHICKEN Scheme compiles to very small binary executables, for example like this:

```
$ csc -O5 ./random_streams_for_perf_stats.scm
```

..with switch _-O5_ for best opimization: [Basic command-line options](https://wiki.call-cc.org/man/5/Using%20the%20compiler#basic-command-line-options)

<br/>

Enter the REPL (Read-Eval-Print Loop) like this: _$ csi_ and give command _(exit)_ to exit it.

<br/>

Installing the needed streamlined string library [SRFI 152](https://api.call-cc.org/5/doc/srfi-152) to use function [string-concatenate](https://api.call-cc.org/5/doc/srfi-152#def:string-concatenate) can be done like this for example:

```
$ cd ./CHICKEN_Scheme/chicken-5.4.0
$ sudo ./chicken-install srfi-152
```

..and may take its time for installation.

##_end
