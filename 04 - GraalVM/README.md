2025-09-08: work in progress

to-do: practical tips: how to make a uberJAR file and compile it with the GraalVM:

- Clojure: TBD
- Python: TBD

Make a TOC: TBD

<br/>

## GraalVM

Using the GraalVM (https://www.graalvm.org/; VM = Virtual Machine) for a **Scala**, **Kotlin** and **Clojure** program is a real hit:

- very fast, standalone, native binary executables for Linux can be built with it according to my experience:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/mean_stddev_err_whiskers%20--%20only%20GraalVM.png)

For example, with the Scala and Kotlin versions, these programs run faster than the compiled Common Lisp or interpreted C# versions. And the Clojure program is now faster than the (normal) Python version.

<br/>

## How to make a standalone executable for Linux with the GraalVM

Scala, Kotlin and Clojure are "native" JVM (Java Virtual Machine) languages and this means that it's easy to build so called "fat JAR" or "uberJAR"/"überJAR" (JAR = Java Archive) files from their source code files. This one uberJAR file can then be compiled - with the help of the GraalVM ecosystem - into one native binary executable for Linux.

### SDKMAN! and Java versions

To install the GraalVM in your Linux system (https://www.graalvm.org/latest/getting-started/linux/) I recommend to do it with installing _The Software Development Kit Manager_ (_SDKMAN!_) first: https://sdkman.io/install/

```
$ curl -s "https://get.sdkman.io" | bash
...
$ source "$HOME/.sdkman/bin/sdkman-init.sh"  # this command adds two (active) lines at the end of the .bashrc file. See below.
...
$ sdk version  # check out installation success

SDKMAN!
script: 5.19.0
native: 0.7.4 (linux x86_64)

$
```

Then the GraalVM (for Java version 24) can be installed with like this:

```
$ sdk install java 24-graal
```

This command will install its own Java version at: _$HOME/.sdkman/candidates/java/current/bin_


Check this out like this:

```
$ java -version
java version "24" 2025-03-18
Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01)
Java HotSpot(TM) 64-Bit Server VM Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01, mixed mode, sharing)
$
```

When you want the "normal" **OpenJDK** (Java Development Kit: https://openjdk.org/index.html) installation being your default Java environment again, then edit and re-activate your _**.bashrc**_ file. Comment its last two lines like this:

```
# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
```

<br/>

### Kotlin

I start with the Kotlin version because it's the easiest to do of all languages I tested.

First I compile the uberJAR file from this [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin/random_streams_for_perf_stats.kt) like this:

```
$ kotlinc random_streams_for_perf_stats.kt -include-runtime -d random_streams_for_perf_stats.jar -opt-in=kotlin.ExperimentalStdlibApi
```

Then I test the resulting uberJAR file with the Java version of the Oracle GraalVM (see above):

```
$ java -jar random_streams_for_perf_stats.jar
...
$
```

Now I build the standalone binary executable with GraalVM's _native-image_ command:

```
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar random_streams_for_perf_stats.jar
...
$
```

> **Warning**
This build command may take its time!


Finally, I should be able to run the standalone executable:

```
$ ./random_streams_for_perf_stats
...
```

<br/>

### Scala

Also see the notes in the header comment block from this [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/password_encryption_perf_stats.scala) and also from this [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala)

#### A first way

A first, intuitive way is to use Scala's _simple build tool_ (_**sbt**_: https://www.scala-sbt.org/) because it allows you to make a standalone executable within one tool. See also from here: https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala#on-scala-installations (TBD)

This procedure requires to fix or check a few configuration files first:

_**./build.sbt**_

I post this file, which is located in the sbt project's root directory, in full to avoid any ambiguity:

```
val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "password_encryption_perf_stats_GraalVM",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

  enablePlugins(ScalaNativePlugin)
```

The only salient line here is the last one with directive _enablePlugins(ScalaNativePlugin)_.

<br/>

_**./project/plugin.sbt**_

This file needs to be created in the project's subdirectory _project_ and has only one directive:

```
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.7")
```

You don't need to install the **Maven** tool at this point.

This is a build tool for Java projects: https://mvnrepository.com/artifact/org.scala-native/sbt-scala-native_2.12_1.0 and can be installed like this for example: _$ sdk install maven_ to use the SDKMAN! another time) 

The sbt takes care about the given versions in the project's configuration files. This means for example that you could conveniently upgrade from older version 0.5.7 to latest version 0.5.8 (as of 2025-09-08) of Scala Native by only updating the _plugin.sbt_ file and running the sbt again.

However, also check your _.bashrc_ file to see that your active Java environment is indeed the Oracle GraalVM one (see from above).

Now start the sbt project, here named _password_encryption_perf_stats_GraalVM_, with Scala [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala) located in the _./src/main/scala_ subdirectory.

Then you start the sbt and enter sbt commands _compile_, _run_ (as a test) and _exit_:

```
$ sbt
copying runtime jar...
[info] welcome to sbt 1.10.11 (Ubuntu Java 21.0.8)
...
sbt:password_encryption_perf_stats_GraalVM> compile
...
sbt:password_encryption_perf_stats_GraalVM> run
...
generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
[success] Total time: 5 s, completed Sep 8, 2025, 11:54:06 AM
sbt:password_encryption_perf_stats_GraalVM> exit
[info] shutting down sbt server
$
```

Yes, the sbt is not for the faint of heart.

If all went well, you can run the resulting standalone executable like this:

```
$ ./target/scala-3.6.4/password_encryption_perf_stats_graalvm

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

#### A siginificantly faster Scala based program

However, using the Java(TM) SE Runtime Environment of the Oracle GraalVM isn't the best way to make a fast Scala based standalone executable in Linux as I found out. Using the "conventional" OpenJDK can create a faster one, at least in my case:

```
$ java -version
openjdk version "21.0.8" 2025-07-15
OpenJDK Runtime Environment (build 21.0.8+9-Ubuntu-0ubuntu124.04.1)
OpenJDK 64-Bit Server VM (build 21.0.8+9-Ubuntu-0ubuntu124.04.1, mixed mode, sharing)
$
```

> **Warning**
Now be careful: you need another sbt project to continue, here just named _password_encryption_perf_stats_. You cannot use the same sbt project as with the GraalVM!

So, after changing back to the OpenJDK Runtime Environment (see _.bashrc_ file from above) I run the _assembly_ command of the sbt in the _password_encryption_perf_stats_ project:

```
.../password_encryption_perf_stats$ sbt assembly
```

..which creates this uberJAR file: _./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar_

The next and final step is (almost) the same as shown above: compile the standalone executable with GraalVM's _native-image_ command:

```
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar
```

..which is then located in the project's root directory as file: _./password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT_

Execution times (mean of 20 runs with: _$ sudo perf stat -r 20 < program name >_):

- Java(TM) SE Runtime Environment of Oracle GraalVM based: 124.8 milliseconds +-9,13% standard deviation
- OpenJDK based: 46.3 milliseconds +-1,16% standard deviation

Though, the file size of the standalone executable based on the OpenJDK is significantly bigger:

- Java(TM) SE Runtime Environment of Oracle GraalVM based: 3,244,144 bytes
- OpenJDK based: 15,665,416 bytes

<br/>

### Clojure

First, have the build environment Leiningen (https://leiningen.org/) installed if not done yet: 

```
$ sudo apt install leiningen
```

You may also have a look at this page: [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/ec2f7950c255d171d3a2698952785feeb55926aa/03%20-%20source%20code/02%20-%20functional%20languages/Clojure)

Creating a project, here a "Hello, World!" example app, works like this:

```
$ lein new app my_project
$ cd my_project
$ lein repl  # start the REPL (Read-Eval-Print Loop) for a little test
...
my-project.core=> (require 'my-project.core)  # the underline character becomes the dash character! Load lib "my-project.core": https://clojuredocs.org/clojure.core/require
nil  # return value
my-project.core=> (my-project.core/-main)  # run the example app
Hello, World!
nil
my-project.core=> (source -main)  # show source code of example app
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
nil
my-project.core=> exit  # leave the REPL
Bye for now!
$
```

Specify a namespace as your _:main_ in project configuration file _project.clj_ in the project's root directory and ensure it’s also AOT (Ahead Of Time) compiled:

```
(defproject my_project "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main my-project.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
```

Now create the uberJAR file and run it on the JVM like this:

```
$ lein uberjar
Compiling my-project.core
Created .../my_project/target/uberjar/my_project-0.1.0-SNAPSHOT.jar
Created .../my_project/target/uberjar/my_project-0.1.0-SNAPSHOT-standalone.jar
$ java -jar ./target/uberjar/my_project-0.1.0-SNAPSHOT-standalone.jar
Hello, World!
$
```

Build the standalone binary executable with GraalVM's _native-image_ command in this expanded version and test it:

```
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image \
--initialize-at-build-time \
-jar ./target/uberjar/my_project-0.1.0-SNAPSHOT-standalone.jar \
-H:+UnlockExperimentalVMOptions \
-H:Name=my_project
...
$ ./my_project  # run the generated standalone executable
Hello, World!
$ 
```

..as seen from here: [Building A Fast Command Line App With Clojure](https://dev.to/kiraemclean/building-a-fast-command-line-app-with-clojure-1kc8)

By the way: creating the uberJAR file and building the standalone binary executable worked with both Java versions, the OpenJDK Java and the GraalVM's Java, at least with my Clojure programs. With both Java versions the same standalone executable file was created.

<br/>

### GraalVM and Python

TL;DR: to make a faster Python program with the help of the GraalVM isn't worth the effort according to my experience. I couldn't produce anything faster, though I found a way to make a working but super-slow program.

#### a/ Presumably the right way (it's not!)

Intuitively...

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

However, be aware that peak performance (also considering GC = Garbage Collection) is probably better on the JVM, but time to start is faster with the GraalVM; see from here: https://www.graalvm.org/python/docs/#comparison

##_end
