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

I have no idea if those first-class continuations have been implemented in Kawa.

<br/>

Kawa is not the only Scheme dialect to target the JVM. Heap-based **SISC** (_Second Interpreter of Scheme Code_) after stack-based **LISC** (_Lightweight Interpreter of Scheme_): https://sisc-scheme.org/manual/html/index.html + http://sisc-scheme.org/sisc.pdf, was another one.

Then there was **JScheme**: https://norvig.com/jscheme.html. However, this command: _$ java jscheme.Scheme r4rstest.scm_ (from https://norvig.com/jscheme/) doesn't work with me.

But after some searching for the fate of **JScheme** I made an interesting discovery, though it's not a breakthrough solution for ambitious programs.

## JScheme

(TBD)

<br/>

## The JSchemePlus hack

(TBD)

<br/>

##_end
