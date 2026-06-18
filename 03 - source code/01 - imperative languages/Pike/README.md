# Pike 

https://pike.lysator.liu.se/

https://github.com/pikelang/Pike

I saw Pike from Sweden in this benchmark: [Performance comparison of programming languages](https://github.com/trizen/language-benchmarks) from 2021.

Pike is a dynamic programming language like Python, Ruby, PHP, Lua, or Perl, which means that "key aspects such as type checking, method binding, and code execution are resolved at runtime rather than at compile time" (grokipedia.com).

<br/>

Since building Pike from latest sources (https://pike.lysator.liu.se/download/pub/pike/beta/9.0.13/) (as of 2026-06-18) failed with me,
I just installed the last stable version like this (in Ubuntu 24 LTS):

```
$ sudo apt install pike8.0-full
...
$ pike -v
Pike v8.0 release 1738 Copyright © 1994-2022 Linköping University
Pike comes with ABSOLUTELY NO WARRANTY; This is free software and you are
welcome to redistribute it under certain conditions; read the files
COPYING and COPYRIGHT in the Pike distribution for more details.
$
```

However, with this installation, Pike comes without support of PCRE's (Perl-compatible regular expressions), and thus without support of POSIX character classes like for example _[[:alnum:]]_:

```
$ cat pike_pcre_test.pike
import Regexp;
write("%O\n", indices(Regexp));
$ pike pike_pcre_test.pike
({ /* 3 elements */
    "`()",
    "SimpleRegexp",
    "_SimpleRegexp"
})
```

<br/>

Though this language, and its precursors, has been around for more than 25 years ([The history of Pike](https://pike.lysator.liu.se/about/history/)), its documentation still has gaps (as of 2026-06-18),
for example at [Modules](https://pike.lysator.liu.se/docs/tut/modules/index.md): _... Unfortunately this tutorial file does not exist yet!_

Therefore, you may also have a look into this very old documentation from 2001: https://fredrik.hubbe.net/pike/tutorial/tutorial.html

<br/>

##_end
