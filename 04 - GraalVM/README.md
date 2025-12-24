# Graal Virtual Machine (GraalVM)

Table of contents:

- [Ahead Of Time (AOT) program compilation with the GraalVM](#ahead-of-time-aot-program-compilation-with-the-graalvm)
- [How to make a standalone executable for Linux with the GraalVM](#how-to-make-a-standalone-executable-for-linux-with-the-graalvm)
- [SDKMAN! and Java versions](#sdkman-and-java-versions)
- [Kotlin](#kotlin)
- [Scala](#scala)
- [A significantly faster Scala based program](#a-significantly-faster-scala-based-program)
- [Clojure](#clojure)
- [GraalVM and Python](#graalvm-and-python)
- [GraalPy](#graalpy)
- [Peak performance with the JVM, time to start performance with the GraalVM](#peak-performance-with-the-jvm-time-to-start-performance-with-the-graalvm)

<br/>

---

## Ahead Of Time (AOT) program compilation with the GraalVM

Using the GraalVM (https://www.graalvm.org/; VM = Virtual Machine) for a **Scala**, **Kotlin** and **Clojure** program is a real hit:

- fast, standalone, native binary executables for Linux can be built with it according to my experience:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/mean_stddev_err_whiskers%20--%20only%20GraalVM.png)

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
...
Do you want java 24-graal to be set as default? (Y/n): Y  # I say "yes" here
...
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

Below, I had a problem with _libz.a_ missing and some unknown installations prevent me to officially install _$ sudo apt-get install libz-dev_, so I copied it from Chez Scheme:

```
$ sudo cp ~/scripts/Chez_Scheme/csv10.3.0/pb/zlib/libz.a /usr/lib/x86_64-linux-gnu/
$ ls /usr/lib/x86_64-linux-gnu/libz* -al  # make a check
…
$ 
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
$ native-image -jar random_streams_for_perf_stats.jar
...
$
```

> [!WARNING]
> This build command may take its time!


Finally, I should be able to run the standalone executable:

```
$ ./random_streams_for_perf_stats
...
```

<br/>

### Scala

Also see the notes in the header comment block from this [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala) and also from this [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/password_encryption_perf_stats_GraalVM.scala)

#### A first way

A first, intuitive way is to use Scala's _simple build tool_ (_**sbt**_: https://www.scala-sbt.org/) because it allows you to make a standalone executable within one tool. See also from here:

- [Working with simple build tool to create a standalone program ("Scala native") in Linux](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(E)%20Working%20with%20simple%20build%20tool%20to%20create%20a%20standalone%20program%20(Scala%20native)%20in%20Linux#working-with-simple-build-tool-to-create-a-standalone-program-scala-native-in-linux)

This procedure requires to fix or check a few configuration files first:

_**./build.sbt**_

I post this file, which is located in the sbt project's root directory, in full to avoid any ambiguity:

```
val scala3Version = "3.7.4"

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
$ ./target/scala-3.7.4/password_encryption_perf_stats_graalvm

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

### A significantly faster Scala based program

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

..which creates this uberJAR file: _./target/scala-3.7.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar_

The next and final step is (almost) the same as shown above: compile the standalone executable with GraalVM's _native-image_ command:

```
$ native-image -jar ./target/scala-3.7.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar
```

..which is then located in the project's root directory as file: _./password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT_

Get the mean execution time of 20 runs with shell command: _$ sudo perf stat -r 20 < program name >_:

- Java(TM) SE Runtime Environment of the Oracle GraalVM: 74.2 milliseconds +- 2.79% standard deviation (2025-10-20)
- **OpenJDK based: 23.6 milliseconds +- 3.21% standard deviation** (2025-10-20)

Though, the file size of the standalone executable based on the OpenJDK is significantly bigger:

- Java(TM) SE Runtime Environment of the Oracle GraalVM: 3,244,144 bytes
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
$ native-image \
--initialize-at-build-time \
-jar ./target/uberjar/my_project-0.1.0-SNAPSHOT-standalone.jar \
-H:+UnlockExperimentalVMOptions \
-H:Name=my_project
...
$ ./my_project  # run the generated standalone executable
Hello, World!
$ 
```

That are in case of the microbenchmark program these commands:

```
$ native-image \
--initialize-at-build-time \
-jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar \
-H:+UnlockExperimentalVMOptions \
-H:Name=random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone
...
$ ./random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone
...
$
```

..as seen from here: [Building A Fast Command Line App With Clojure](https://dev.to/kiraemclean/building-a-fast-command-line-app-with-clojure-1kc8)

By the way: creating the uberJAR file and building the standalone binary executable worked with both Java versions, OpenJDK Java and GraalVM's Java, at least with my Clojure programs. With both Java versions the same standalone executable file was created.

<br/>

### GraalVM and Python

I was not successful in building a standalone executable from my [Python program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py) in the way I described above for Scala etc.

Because the GraalVM is not allowing me to build one "proper" uberJAR file (with a _main manifest attribute_), a fact which prevents the execution of a resulting uberJAR file on the JVM (with the _$ java -jar_ command).

That's the end of this development path since Python is different than Java native languages like Scala, Kotlin or Clojure:

> Python is a large language. “Batteries included” has long been a core tenet of CPython.

From: https://www.graalvm.org/python/docs/#reducing-binary-size (CPython is the normal version of Python.)

### GraalPy

However, I managed to convert my [Python program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py) into a standalone native application with the help of [GraalPy](https://www.graalvm.org/python/docs/#python-standalone-applications).

Also see: https://www.graalvm.org/python/docs/#linux

Spoiler alert: this standalone program runs slow with my little benchmark program: _1,1359 +- 0,0475 seconds time elapsed  ( +-  4,18% )_

<br/>

Nevertheless, here's my rough description how to make it:

- download a suitable _... .gz_ file for your system und uncompress it in some project directory: https://github.com/oracle/graalpython/releases
- add this, depending on your system, to environment variable _PATH_ in the _.bashrc_ configuration file:

```
export PATH="$PATH:$HOME/.../GraalVM/graalpy-24.2.1-linux-amd64/bin/"
export PATH="$HOME/.sdkman/candidates/java/24-graal/bin/:$PATH"
export JAVA_HOME="$HOME/.sdkman/candidates/java/24-graal/"
```

- do not forget to re-activate this modified _.bashrc_ file: _$ source ~/.bashrc_
- it's important that the _JAVA_HOME_ environment variable points to a GraalVM installation and not something else: _$ $JAVA_HOME_
- additionally, you can also test the GraalPy version: _$ graalpy -V_

<br/>

At this point you can create the usual virtual Python environment, a step which takes a while: _$ graalpy -m venv < my_env >_

Now you can install _NumPy_ and other Python packages in this virtual environment:

- first, activate the new virtual environment: _$ source < my_env >/bin/activate_
- then, install _NumPy_ which may also take its time: _< my_env >$ pip install numpy_
- now, you can run the Python script in two ways:
- _< venv-dir >$ python3 < python program ~.py >_

or like this:

-	_< venv-dir >$ graalpy <python program ~.py >_

Both ways don't provide fast program execution according to my tests.

<br/>

So far, no standalone native application has been produced from the Python script. This can also be done with GraalPy.

First, leave the virtual Python environment if not done: _< my_env >$ deactivate_

Then, build a standalone native application from your Python script with this command, a process which also takes its time:

```
$ graalpy -m standalone native --module <my_script.py> --output <my_binary> --venv <my_env> 
```

..and run it: 

```
$ ./<my_binary>
...
Exception in thread "main" ModuleNotFoundError: No module named 'numpy'
...
$
```

So, this was a failure. However, in case of my little benchmark program I fixed this program with not using _NumPy_, which is anyway only used here:

```
import numpy as np
...
x[0] = np.random.randint(0, m, size=1, dtype=int)[0]
```

from [Python benchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py)

..and can be replaced with expressions:

```
# import numpy as np
import random
...
# x[0] = np.random.randint(0, m, size=1, dtype=int)[0]
x[0] = random.randint(1,m)
```

Now, running the standalone native application worked with me, but, as shown above, not in a fast fashion.

By the way: this app is a monster with a file size of 352 megabytes!

<br/>

### Peak performance with the JVM, time to start performance with the GraalVM

However, be aware that peak performance (also considering GC = Garbage Collection) is probably better on the JVM, but time to start is faster with the GraalVM; see from here: https://www.graalvm.org/python/docs/#comparison

<br/>

##_end
