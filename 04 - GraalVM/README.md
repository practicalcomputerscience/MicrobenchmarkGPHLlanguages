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

<br/>

### How to make a standalone executable for Linux with the GraalVM

Scala, Kotlin and Clojure are "native" JVM (Java Virtual Machine) languages and this means that it's easy to build so called "fat JAR" or "uberJAR"/"überJAR" (JAR = Java Archive) files from their source code files. This one uberJAR file can then be compiled - with the help of the GraalVM ecosystem - into one native binary executable for Linux.

#### SDKMAN! and Java versions

To install the GraalVM in your Linux system (https://www.graalvm.org/latest/getting-started/linux/) I recommend to do it with installing _The Software Development Kit Manager_ (_SDKMAN!_) first: https://sdkman.io/install/

```
$ curl -s "https://get.sdkman.io" | bash
...
$ source "$HOME/.sdkman/bin/sdkman-init.sh"  # this command should add two lines at the end of the .bashrc file. See below.
...
$ sdk version  # check out installation success

SDKMAN!
script: 5.19.0
native: 0.7.4 (linux x86_64)

$
```

Then the GraalVM (for Java version 24) can be installed with this command:

```
$ sdk install java 24-graal
```

This command will install its own Java version at: _$HOME/.sdkman/candidates/java/current/bin_


Check it out like this:

```
$ java -version
java version "24" 2025-03-18
Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01)
Java HotSpot(TM) 64-Bit Server VM Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01, mixed mode, sharing)
$
```

When you want the "normal" **OpenJDK** installation being your default Java environment again, then edit and re-activate your _**.bashrc**_ file to get it back in front. Comment its last two lines like this:

```
# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
```

Only when working with the GraalVM I use the Java version installed with SDKMAN!, otherwise not.

#### Kotlin

I start with the Kotlin version because this is the easiest of all.



#### Scala

See notes in the header comment block from this [source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/password_encryption_perf_stats.scala)
..and also from this [source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala)

(TBD)


#### Clojure

(TBD)

<br/>

### GraalVM and Python

#### a/ Presumably the right way (it's not!)

(TBD)

I wasn't successful in building a standalone executable from my Python program. The GraalVM is not allowing me to build one "proper" uberJAR file (with a _main manifest attribute_), a fact which also prevents the execution of a resulting uberJAR file based on Python source code on the JVM (with the _$ java -jar_ command). That's the end of this development path since Python is different:

> Python is a large language. “Batteries included” has long been a core tenet of CPython.

From: https://www.graalvm.org/python/docs/#reducing-binary-size (CPython is the normal version of Python.)

#### b/ xxx

(TBD)

#### c/ xxx

(TBD)

<br/>

### Peak performance with the JVM, time to start performance with the GraalVM

However, be aware that peak performance (also considering GC = Garbage Collection) is probably better on the JVM, but time to start is faster with the GraalVM; see from: https://www.graalvm.org/python/docs/#comparison

##_end
