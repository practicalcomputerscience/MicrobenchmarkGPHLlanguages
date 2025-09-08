2025-09-08: work in progress

to-do: practical tips: how to make a uberJAR file and compile it with the GraalVM: see _(TBD)_ below

- Scala

- Kotlin

- Clojure

<br/>

### GraalVM

Using the GraalVM (https://www.graalvm.org/; VM = Virtual Machine) for a **Scala**, **Kotlin** and **Clojure** program is a real hit:

- very fast, standalone, native binary executables for Linux can be built by it according to my experience:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/mean_stddev_err_whiskers%20--%20only%20GraalVM%20a.png)

For example, with the Scala and Kotlin versions, these programs run faster than my OCaml, V, Ada or Mojo natively compiled versions!

Curious fact about Scala and Kotlin:

- the slower of both languages on the JVM, that is Scala, now runs faster with the help of the GraalVM:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/mean_stddev_err_whiskers%20--%20only%20GraalVM%20b.png)

- Scala with GraalVM: 14.5 milliseconds (mean of 20 runs), down from 143 milliseconds on the JVM
- Kotlin with GraalVM: 16.3 milliseconds (mean of 20 runs), down from 108 milliseconds on the JVM


### How to make a standalone executable for Linux with the GraalVM

Scala, Kotlin and Clojure are "native" JVM (Java Virtual Machine) languages and this means that it's easy to build so called "fat JAR" or "uberJAR"/"überJAR" (JAR = Java Archive) files from their source code files. This one uberJAR file can then be compiled - with the help of the GraalVM ecosystem - into one native binary executable for Linux.

#### 

To install the GraalVM in your Linux system (https://www.graalvm.org/latest/getting-started/linux/) I recommend to do it with installing the _The Software Development Kit Manager_ (_SDKMAN!_) first: https://sdkman.io/install/

```
$ curl -s "https://get.sdkman.io" | bash
```

Then ...





(TBD)

#### Scala

See notes in the header comment block from here: https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/password_encryption_perf_stats.scala
..and also here: https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala

(TBD)

#### Kotlin

(TBD)


#### Clojure

(TBD)

### GraalVM and Python

I wasn't successful in building a standalone executable from my Python program. The GraalVM is not allowing me to build one "proper" uberJAR file (with a _main manifest attribute_), a fact which also prevents the execution of a resulting uberJAR file based on Python source code on the JVM (with the _$ java -jar_ command). That's the end of this development path since Python is different:

> Python is a large language. “Batteries included” has long been a core tenet of CPython.

From: https://www.graalvm.org/python/docs/#reducing-binary-size (CPython is the normal version of Python.)

### Peak performance with the JVM, time to start performance with the GraalVM

However, be aware that peak performance (also considering GC = Garbage Collection) is probably better on the JVM, but time to start is faster with the GraalVM; see from: https://www.graalvm.org/python/docs/#comparison

##_end
