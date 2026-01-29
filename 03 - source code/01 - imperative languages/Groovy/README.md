2026-01-29: work in progress

# Groovy

https://groovy-lang.org/index.html

https://github.com/apache/groovy/tree/master

PDF: [Groovy Language Documentation, Version 5.0.4](https://docs.groovy-lang.org/docs/latest/html/documentation/index.pdf)

JVM = Java Virtual Machine

JDK = Java Development Kit

---

Table of contents:

- [Idea of Groovy](#idea-of-groovy)
- [Groovy and the GraalVM](#groovy-and-the-graalvm)
- [Installation tips](#installation-tips)
- [Building uberjar files](#building-uberjar-files)

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

<br/>

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

You may also have a look at [Dynamic programming with Groovy](https://livebook.manning.com/book/groovy-in-action-second-edition/chapter-8) from 2015.

<br/>

Here's a table with indicative execution times from only one run to just get an overview:

test # | command | exe time in milliseconds | comment
--- | --- | --- | --- 
groovy without @CompileStatic | _time groovy ./random_streams_for_perf_stats_no_static_compilation.groovy_ | ~1017 |
groovy with @CompileStatic | _time groovy ./random_streams_for_perf_stats.groovy_ | ~925 |
uberjar without @CompileStatic | _time time java -jar ./build/libs/random_streams_for_perf_stats_no_static_compilation.jar_ | ~587 | 
uberjar with @CompileStatic | _time time java -jar ./build/libs/random_streams_for_perf_stats.jar_ | ~340 | 

<br/>

For comparision, Scala's, Kotlin's and Clojure's uberjar files run on the same JVM version with these execution times (from [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)):

- Scala: 142 milliseconds
- Kotlin: 73 milliseconds
- Clojure: 416 milliseconds

<br/>

## Installation tips

For the final version of my Groovy installation, I conveniently used the [SDKMAN!](https://sdkman.io/) software development kits manager, see also at the [Kotlin Installation tips](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#installation-tips):

```
$ sdk install groovy  # install latest Groovy version
...
$ sdk install gradle  # install latest Gradle build tool version
...
$
```

Then I individually expanded my _.bashrc_ configuration file like this for both tools, and re-commented the SDKMAN related entries again, see at [On SDKMAN and Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/20%20-%20language%20versions/README.md#on-sdkman-and-kotlin).

Don't forget to activate it after changes: _$ source ~/.bashrc_:

```
export PATH="$PATH:~/.sdkman/candidates/groovy/5.0.4/bin"
export PATH="$PATH:~/.sdkman/candidates/gradle/9.3.0/bin"
```

<br/>

## Building uberjar files

As indicated above, I used Gradle to make an uberjar file. But before that, a couple of things must be made.

First, I built the simple subdirectory manually. Then I stored Groovy program [random_streams_for_perf_stats.groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy/random_streams_for_perf_stats.groovy) in this subdirectory: _./src/main/groovy/_

Next, the build configuration file [build.gradle](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy/build.gradle) must be prepared. Again, I manually put its content together like this:

```
plugins {
    id 'groovy'
    id 'application'
    id 'com.gradleup.shadow' version '9.3.1'  // Replace with your Gradle version
}

repositories {
    mavenCentral()
}

dependencies {
    implementation 'org.apache.groovy:groovy-all:5.0.4'  // Replace with your required Groovy version
}

application {
    mainClass = 'random_streams_for_perf_stats'  // Replace with your Groovy script's main class name
}

tasks.named('shadowJar') {
    archiveBaseName.set('random_streams_for_perf_stats')
    archiveVersion.set('')
    archiveClassifier.set('')
}
```

<br/>

Now, the uberjar file can be hopefully generated, and then executed on the JVM like this:

```
$ gradle shadowJar
...
$ java -jar ./build/libs/random_streams_for_perf_stats.jar
...
$
```

<br/>

##_end
