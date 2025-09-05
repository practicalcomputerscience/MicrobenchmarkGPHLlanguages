2025-08-31: heavy work in progress

With [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure) arguably being the premier Lisp- or Scheme-like language on the JVM, this question came to my mind:

- what is the premier Scheme dialect on the JVM?

I think this has been for many years:

## Kawa Scheme

https://www.gnu.org/software/kawa/index.html

However, I couldn't compile its sources because the _make_ command stopped with two errors after running the _./autogen.sh_ and _./configure_ commands with my usual and up-to-date OpenJDK version 21.0.7: https://www.gnu.org/software/kawa/Source-distribution.html

### Other efforts

Over 20 years ago **Bigloo** still supported compiling Scheme source code into bytecode for the JVM and still(!) includes related documentation, which can be opened after installation with: _$ man 1 bigloo_ -- this is the same documentation like this: https://www.systutorials.com/docs/linux/man/1-bigloo/

However, with a modern Bigloo installation this is no longer working since file _bigloo.jheap_ for example is missing after installation and which should be (usually) installed into directory: _/usr/local/lib/bigloo/4.6a_

This is plausible to me, because Scheme's **exception handling** may often rely on procedure _call-with-current-continuation_ (in short _call/cc_) on which Bigloo's user manual from 2019 states:

> Since _call/cc_ is difficult to compile efficiently, one might consider using _bind-exit_ instead. For this reason, we decided to enable _call/cc_ only with a compiler option.

(this compiler option is: _--call/cc_)

See also from this thesis from around 2015: https://andrebask.github.io/thesis/

> ...However, being the Java Virtual Machine devoid of stack manipulation primitives, **Kawa lacks of one of the most peculiar Scheme features**: First-class continuations. This dissertation describes an implementation of the _call/cc_ control operator in the Kawa compiler. In particular it shows how the exception handling feature, common to many programming languages, can be exploited to implement first-class continuations in an environment without stack manipulation primitives, and how this can be realised in a real compiler. ...
 
(my emphasis in bold)

I have no idea if those first-class continuations have ever been implemented in Kawa.

<br/>

Kawa is not the only Scheme dialect to target the JVM. Heap-based **SISC** (_Second Interpreter of Scheme Code_) after stack-based **LISC** (_Lightweight Interpreter of Scheme_): https://sisc-scheme.org/manual/html/index.html + http://sisc-scheme.org/sisc.pdf, was another one.

Then there was **JScheme**: https://norvig.com/jscheme.html. However, command: _$ java jscheme.Scheme r4rstest.scm_ from https://norvig.com/jscheme/ doesn't work with me.

But after some searching for the fate of **JScheme** I made an interesting discovery, though it's not a breakthrough solution for ambitious programs.

## JScheme

My efforts to get **JScheme** version 1.4 from April 16, 1998 at https://norvig.com/jscheme.html (1) running only succeeded in so far that I could start the JScheme REPL (Read-Eval-Print Loop). But I was not able to run something like this for example:

```
$ java jscheme.Scheme r4rstest.scm
```

So, I followed the link as indicated above (1):

> Since April 1998, development has been picked up by others, notably Tim Hickey at Brandeis...

..to a **JScheme** version 7.2 from 3/1/2005: https://sourceforge.net/projects/jscheme/files/
It even has a _make_ installation procedure (with _$ sh bin/make_), but again, the only thing that worked for me was a JScheme REPL.

After more searching I found this on SourceForge: https://sourceforge.net/projects/jschemeplus/files/

I downloaded file _**SchemePlus-1.4PF2.zip**_ and unpacked it in some working directory (Linux).

<br/>

### "Hello, World!" in JSchemePlus

Here I provide a "Hello, World!" example from a Scheme source code file running on the JVM (with me it's OpenJDK version 21.0.7). As usual, everything is being executed in the Bash shell:

```
$ cat hello_world.scm
(display "Hello World from JSchemePlus version 1.4PF2!")
(newline)
(display (random))
(newline)
(exit)
```

Scheme comments with semicolons (';' or ';;') work, but not these: #| ... |#

You can run this Scheme program like this on the JVM:

```
$ java -jar ./JSchemePlus-1.4PF2/runtime.jar ./hello_world.scm
Hello World from JSchemePlus version 1.4PF2!
0.9192306489663782
$
```

With expression _(display (random))_ I'm using one of the enhancement procedures provided by the author; see from here again: https://sourceforge.net/projects/jschemeplus/files/ or from file HELP.

Source code file _hello_world.scm_ can be wrapped inside a **Java Archive file** (~.jar) like this:

1. rename file _hello_world.scm_ into _main.scm_
2. make a copy of file _runtime.jar_ (don't mess up this file!) and place it in your local working directory, then rename it into _hello_world.jar_
3. add _main.scm_ into the _hello_world.jar_ file: _$ jar -uf hello_world.jar main.scm_
4. and finally execute this "uberjar" file on the JVM:

```
$ java -jar hello_world.jar
Hello World from JSchemePlus version 1.4PF2!
0.21798621286542608
$
```

<br/>

### Compliance test r4rstest.scm

Now I can also run Peter Norvig's original _r4rstest.scm_ compliance test successfully; see its source from here: https://norvig.com/jscheme/

However, I made one change to it and added expression _(exit)_ at the bottom to leave the JScheme REPL at the end of this test program. Run this test like this:

```
$ java -jar ./JSchemePlus-1.4PF2/runtime.jar ./r4rstest.scm
```

The tail of the console output of this program looks like this:

```
...
SECTION(6 5 6)
({string->number} "281474976710655") ==> 281474976710655
({number->string} 281474976710655) ==> "281474976710655"
({fact} 20) ==> 2432902008176640000
({fact} 40) ==> 8.159152832478977E47
Passed all tests
To fully test continuations, do (test-cont)
$
```

Beware that file name _r4rstest.scm_ is hard coded in this source code file (though it could be changed throughout I guess).

You can directly enter the JScheme REPL like this:

```
$ java -jar ./JSchemePlus-1.4PF2/runtime.jar
```

You will have more editing comfort in this REPL when you call rlwrap (a readline wrapper: https://linux.die.net/man/1/rlwrap) before:

```
$ rlwrap java -jar ./JSchemePlus-1.4PF2/runtime.jar
```

<br/>

By the way: I found out that not only in the Land of Scheme's, but generally with functional programming languages, there's no rule when _rlwrap_ helps and when not. Some REPL systems already provide editing comfort and others don't work with this Linux default readline wrapper.

<br/>

### Testing call-with-current-continuation (call/cc)

_r4rstest.scm_ also includes three optional tests, two of them are already included in its original version: _(test-sc4)_ and _(test-delay)_.
When you run _r4rstest.scm_ in its original version and thus finally stay in the JScheme REPL, the third test can be manually started by entering expression:

```
JS+> (test-cont)
;testing continuations;
SECTION(6 9)
({leaf-eq?} (a (b (c))) ((a) b c)) ==> JS+>
```

I'm not sure what to expect here. So, I tested some simple examples and one moderate one for _call/cc_ from "The Scheme Programming Language", 4th ed., 2009: https://www.scheme.com/tspl4/further.html#./further:h3

The first example works:

```
JS+> (call/cc
  (lambda (k)
    (* 5 4)))
20
JS+>
```

... and the next two, also simple ones, too. But not this one:

```
JS+> (define product
 (lambda (ls)
   (call/cc
     (lambda (break)
       (let f ([ls ls])
       (cond
         [(null? ls) 1]
         [(= (car ls) 0) (break 0)]
         [else (* (car ls) (f (cdr ls)))]))))))
product
JS+> (product '(1 2 3 4 5))
()
JS+>
```

_()_ is not the supposed result, 120 is. I tested this procedure in the Bigloo and CHICKEN REPL's and (naturally) both dialects evaluated to the correct result 120
(remember: you cannot do this in Gambit without first changing _[...]_ into _(...)_).

<br/>

### What about my Scheme program?

There are couple of restrictions in this environment for my program:

a/ JScheme - with some limitations - only supports Scheme in version R4RS: https://norvig.com/jscheme.html#limitations

b/ Pasquale Frega's enhancements don't introduce exception handlers (to be not misunderstood: I think it's great that somebody picked up the pieces from so long
ago!) which I'm using when writing strings to files:

- Racket: _with-handlers_
- Gambit and Bigloo: _with-exception-handler_
- CHICKEN: _handle-exceptions_

c/ original construct _(if (file-exists? filename) ..._ as a simple check is also not working because predicate _file-exists?_ was only introduced later in R6RS

<br/>

## The JSchemePlus hack

(TBD)

<br/>

##_end
