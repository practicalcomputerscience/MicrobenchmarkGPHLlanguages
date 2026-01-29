2026-01-29: work in progress

# Groovy

https://groovy-lang.org/index.html

https://github.com/apache/groovy/tree/master

JVM = Java Virtual Machine

JDK = Java Development Kit

---

Table of contents:

- [Idea of Groovy](#idea.of-groovy)
- [Groovy and the GraalVM](#groovy-and-the-graalvm)
- [](#)
- [](#)
- [](#)

<br/>

---

## Idea of Groovy

After [Scala](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala#scala),
[Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#kotlin)
and [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#clojure),
I decided to (quickly) implement the microbenchmark program also in Groovy as another language, which is being natively executed on the JVM.

While imperative languages Scala and Kotlin are statically typed, Groovy is dynamically typed by default, same like functional language Clojure.

However, the typing here is done differently, since Groovy also allows static compilation to JVM bytecode, a fact which usually improves the execution speed of a Groovy program
(though and in average, it probably won't beat Java in terms of execution speed).

But I think that the appeal of Groovy lies in its **much simpler syntax** compared to Java, while being fully compatible with the Java ecosystem.

Same like Scala and Kotlin, Groovy also allows functional programming.

<br/>

## Groovy and the GraalVM

Principally, it should be possible to use the [GraalVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm)
for Ahead Of Time (AOT) program compilation for speedy programs.

However, I gave up on it, even with the allegedly most suitable pairing currently of:

- Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01), and
- older Groovy version 4.0.30 for better interaction with the GraalVM than current Groovy version 5.0.4.

The successfully compiled program regularly ran into _java.lang.NullPointerException_'s, even after numerous experiments in the Groovy source code:

> [!CAUTION]
> Groovy's dynamic capabilities nature apparently clashes with GraalVM's static nature.

Here's a longer page from 2021 about successfully [Making a Groovy on GraalVM native image journey](https://github.com/croz-ltd/klokwrk-project/blob/master/support/documentation/article/groovy-graalvm-native-image/groovy-graalvm-native-image.md) after some struggles.

<br/>

## Execution speed with standard bytecode for the JVM

However, I learned something from my GraalVM experiments and came to these conclusion, also with having in mind to keep the Groovy source code idiomatic:

- use the OpenJDK over a JDK version with Java(TM) SE (Standard Edition) for the Oracle GraalVM, something which was already clear here at: [A significantly faster Scala based program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#a-significantly-faster-scala-based-program)
- directly executing a Groovy program with Groovy's command line processor _$ groovy <program name.groovy>_ is significantly slower than first making an uberjar file _~.jar_, which is then being executed with _$ java -jar <~.jar>_; this is apparently independent of the used JDK version

See also from [here](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#does-the-jdk-java-development-kit-version-matter-at-kotlin):

> ...in case of doubt and in my opinion, run the uberjar file with a JDK (Java Development Kit) as proposed by Ubuntu (or another Linux distribution in use), even when this uberjar file has been originally generated with a JDK version with a Java(TM) SE (Standard Edition) for the Oracle GraalVM for example.

A a zero-cost trick for speedy Groovy programs is to use type definitions and annotations, specifically annotation _@CompileStatic_ on classes and methods.
_@CompileStatic_ bypasses Groovy’s dynamic [Meta-Object Protocol (MOP)](https://docs.groovy-lang.org/latest/html/documentation/core-metaprogramming.html#_runtime_metaprogramming),
allowing the JVM to perform standard optimizations:

```
import groovy.transform.CompileStatic  // ...

@CompileStatic
class random_streams_for_perf_stats {
    ...
}
```

test # | command | exe time in milliseconds | commant
--- | --- | --- | --- 

TBD

## Installation tips

TBD

## Building uberjar files

TBD

<br/>

##_end

