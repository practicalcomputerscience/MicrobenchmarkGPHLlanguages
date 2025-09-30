2025-09-30: work in progress

# Running and building Scala programs: baby steps

Subpages:

- [(0) Glossary, symbols and names](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(0)%20Glossary,%20symbols%20and%20names#glossary-symbols-and-names)
- [(A) Hello world with the Scala code runner in Windows 11]()
- [(B) Compile Scala source code "directly" to run on the Java Virtual Machine (JVM)]()
- [(C) Working with simple build tool to run an app on the JVM in Windows with (3rd party) imports]()
- [(D) Working with simple build tool to create a standalone program ("Scala native") in Linux]()
- [(E) From a Scala program to JavaScript for the web browser]()
- [(F) From a Scala program to JavaScript for node.js]()
- [(G) My problems with Scala]()
- [(H) Tips for working with the sbt (simple build tool)]()

<br/>

# Introduction

I discovered that making my very first "shippable" Scala applications, with latest Scala version 3, with the official tool called **sbt** (**"simple build tool"**: [www.scala-sbt.org](https://www.scala-sbt.org/)) was not easy. It took me numerous trials and errors.

Understanding the sbt - at least to some extent - was my **key to understand practical coding with Scala** for more than just printing "Hello World!" to the console.

> And it's super-easy to trap into the Scala, sbt, Java version hell and to become lost in an increasingly cluttered directory of project files.

<br/>

In case you don't know: Scala heavily relies on the **Java ecosystem**.

However, whenever I was in doubt with one of the numerous third party libraries, I turned to an official and hopefully latest version of a Java or close to Java library. This was one of the better experiences, though the state of the Java ecosystem after all those year since 1996(!) appears strange to me: [The Complete History of Java Programming Language](https://www.geeksforgeeks.org/the-complete-history-of-java-programming-language/)

I fought a lot with successfully **importing and using libraries** into my first, little scala source code files (~.scala).

Just the week before, I've built successfully my first little Go (from Google) program: [Build simple, secure, scalable systems with Go](https://go.dev/) -- even with a version where I tested - also successfully - the famous and so called "goroutines".

This was a very positive experience, though I read good things about Go before. I could concentrate on coding instead of fighting with the ecosystem.

Well, this is apparently the difference between a programming language from a multi billion dollar company or something else.

<br/>

I knew for years that Scala has - by design and constraints - only a slim core ecosystem. However, I was surprised of the length of my struggles to get little things finally running.

However, after elaborate experimentations I "cracked the code"!! After this, I could also build:

- **default JVM** (Java Virtual Machine) based executables
- **"native" Scala** executables and even
- **JavaScript** based executables

..in Linux (here Ubuntu LTS 24). The only flavor missing at the moment is something that can run on node.js (https://nodejs.org/en), so something else than a web browser.

<br/>

Throughout my Scala journey so far, I discovered the first "dead" part of the Scala ecosystem, here a libary called "breeze" for mathematical functions (with Scala and Java these are not part of the standard libraries): [scalanlp / breeze](https://github.com/scalanlp/breeze)

I only needed something to calculate the (sample) mean and (sample) standard deviation of a set of numbers. Finally, I turned successfully to this library: [Commons Math: The Apache Commons Mathematics Library](https://commons.apache.org/proper/commons-math/)

But boy, can't this age-old library have an inbuilt function for standard deviation?!?

Instead, one has first to use the function for variance and then take the square root: [1 Statistics](https://commons.apache.org/proper/commons-math/userguide/stat.html)

```
import org.apache.commons.math3.stat.StatUtils  // StatUtils.mean, StatUtils.variance
import org.apache.commons.math3.util._          // FastMath.sqrt()
...
    // type casting over an array:
    val durations_Dbl: Array[Double] = durations.map(_.toDouble)

    val avg = StatUtils.mean(durations_Dbl)

    // variance and standard deviation of **sample**:
    val stddev = FastMath.sqrt(StatUtils.variance(durations_Dbl, avg))
```

<br/>

Here I will tell you already a "big secret": declaring the imports in a Scala source code file isn't enough. You have to have it listed in a configuration file of the sbt tool, called _build.sbt_ and hopefully with the right version numbers and syntax:

```
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
```

You necessarily don't need a build tool for very simple things, but for anything more demanding and stepping outside of the Scala core, using this tool - or another build tool - is **essential** from my point of view.

<br/>

Of course, it's never a good sign when a language specific library, which was under development and in use for more than a decade, becomes retired.

From my point of view it would be a real pitty if the Scala programming language, just after the boost of the major step forward with the version 3, starting around 2019/2020, becomes less and less used in the "real world":

[New in Scala 3](https://docs.scala-lang.org/scala3/new-in-scala3.html)

2021-02-17: [Scala 3.0.0-RC1 – first release candidate is here](https://dotty.epfl.ch/blog/2021/02/17/scala3-rc1.html)

<br/>

And last but not least: the probability that someone will write and publish a book like "100 Scala Mistakes and How to Avoid Them" (see https://www.thecoder.cafe/p/100-go-mistakes) is rather low in my opinion.

<br/>

##_end



