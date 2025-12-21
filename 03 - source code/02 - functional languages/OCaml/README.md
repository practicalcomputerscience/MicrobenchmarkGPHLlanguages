# OCaml

https://ocaml.org/

https://dune.build/

<br/>

In my opinion: 

> If you want to tinker in an imperative style in the source code of a functional (and fast) programming language, then take OCaml.
>
> Albeit, OCaml is probably not the easiest functional language to learn at first. It has a strict type system and you are forced to meticulously care about side effects.

---

Table of contents:

- [Installation tips](#installation-tips)
- [OCaml on the Java Virtual Machine (JVM)](#ocaml-on-the-java-virtual-machine-jvm)

---

### Installation tips

Rename:

- _random_streams_for_perf_stats_main.ml_ into _main.ml_ located in the _./bin_ project subdirectory
- _random_bitstring_and_flexible_password_generator_main.ml_ into _main.ml_ located in the _./bin_ project subdirectory

..here with project directories _random_streams_for_perf_stats_ and _random_bitstring_and_flexible_password_generator_ respectively, each built with the Dune build tool like this for example:

```
$ dune init proj random_streams_for_perf_stats
$ cd random_streams_for_perf_stats  # check main.ml file in project subdirectory ./bin
$ dune build
$ ./_build/default/bin/main.exe  # run program in Ubuntu 24 LTS
```
I'm not configuring special things in the _dune_ and _dune-project_ configuration files.

<br/>

### OCaml on the Java Virtual Machine (JVM)

When I had a look at what functional programming languages are available on the JVM: https://en.wikipedia.org/wiki/List_of_JVM_languages, I found OCaml: http://www.ocamljava.org/index.html

This version is pretty old from 2015 and abandoned, but I gave it a try.

I downloaded the last binary build to avoid compiling it by my own: http://www.ocamljava.org/downloads/ and set the path to the compiler binaries in my
_.bashrc_ config file (temporarily):

```
export PATH="$PATH:$HOME/scripts/OCaml-Java/ocamljava-2.0-alpha3-bin/bin/"
```

I checked my installation with:

```
$ ocamljava -v
... The OCaml Java-bytecode compiler, version 4.01.0 ... OCaml-Java version: 2.0-alpha3 ... Standard library directory: /ocamljava-2.0-alpha3/lib/ocaml
```

Then I compiled my original and working [OCaml source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/password_encryption_perf_stats_main.ml), which is normally meant to be compiled into a binary executable:

```
$ ocamljava -c password_encryption_perf_stats_main.ml
```

..and linked it into an "uberJAR" file (same like with Scala and Kotlin for example):

```
$ ocamljava -o password_encryption_perf_stats_main.jar password_encryption_perf_stats_main.cmj
```

..with Java version:

```
$ java -version
... OpenJDK Runtime Environment (build 21.0.7...) ...
$
```

..and ran it on the JVM:

```
$ time java -jar password_encryption_perf_stats_main.jar
```

Result: 724 [milliseconds] 

Not bad.

<br/>

##_end
