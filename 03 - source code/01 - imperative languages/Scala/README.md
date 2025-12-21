# Scala

https://www.scala-lang.org/

https://www.scala-sbt.org/

---

## Installation tips

First, in my _~/.bashrc_ config file I masked this OpenJDK (Open **Java** Development Kit) installation for [Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#installation-tips), which is not usuable with Scala!!

Then I installed this OpenJDK version:

```
$ sudo apt install openjdk-25-jre-headless
...
$ java --version
openjdk 25.0.1 2025-10-21
OpenJDK Runtime Environment (build 25.0.1+8-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.1+8-Ubuntu-124.04, mixed mode, sharing)
$ 
```

..and set environment variable _JAVA_HOME_ in the _~/.bashrc_ config file with: _export JAVA_HOME="/usr/lib/jvm/java-25-openjdk-amd64"_

<br/>

I continued with installing the **Coursier** application and artifact manager to install Scala app's as decribed here: https://get-coursier.io/docs/cli-installation:

```
$ curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
...
$ ./cs setup
Checking if a JVM is installed
Found a JVM installed under ~/.sdkman/candidates/java/current.

Checking if ~/.local/share/coursier/bin is in PATH
  Should we add ~/.local/share/coursier/bin to your PATH via ~/.profile? [Y/n] Y

Checking if the standard Scala applications are installed
  Installed ammonite
  Installed cs
  Installed coursier
  Installed scala
  Installed scalac
  Installed scala-cli
  Installed sbt
  Installed sbtn
  Installed scalafmt

$ 
```

In Ubuntu, also expand the _PATH_ environment variable in your _~/.bashrc_ config file to have access to the Scala app's as listed above (under "Checking if the standard Scala applications are installed"):

```
export PATH="$PATH:~/.local/share/coursier/bin"
```

Do some tests now:

```
$ sbt --version
sbt runner version: 1.11.7
...
$ scala --version
Scala code runner version: 1.9.1
Scala version (default): 3.7.4
$
```

<br/>

## Building tips

I started with the sbt (simple build tool) in my Scala working directory:

```
$ sbt new
... # be patient here
Select a template: d  # select option d for a Scala 3 seed template
name [Scala 3 Project Template]: random_streams_for_perf_stats

Template applied in ~/scripts/Scala/./random_streams_for_perf_stats

$ cd ./random_streams_for_perf_stats  # change into the Scala app directory
$
```

In the Scala app directory, that is the project root directory named _random_streams_for_perf_stats_, I configured these files:

_./project/plugins.sbt_ like this:

```
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
```

Look up latest version for _SBT Assembly_, the sbt plugin to create a **single über jar**, from here: https://mvnrepository.com/artifact/com.eed3si9n/sbt-assembly_2.12_1.0

..and the _build.sbt_ config file in the project root directory:

```
val scala3Version = "3.7.4"
lazy val root = project
  .in(file("."))
  .settings(
    name := "random_streams_for_perf_stats",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
  Global / onChangedBuildSource := IgnoreSourceChanges
  scalacOptions ++= Seq("-deprecation", "-feature")
```

As last preperation, copy source code file _random_streams_for_perf_stats.scala_ as _Main.scala_ into subdirectoy: _./src/main/scala/Main.scala_

<br/>

Now comes the critical moment, that is compiling "1 Scala source" with the assembly plugin:

```
$ sbt assembly
...
[info] Built: ~/scripts/Scala/random_streams_for_perf_stats/target/scala-3.7.4/random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT.jar
[info] Jar hash: ae0649bc2db004ffe9898874f05933357fed162e
[success] Total time: 2 s, completed Dec 21, 2025, 1:13:29 PM
$ 
```

Beware that "assembly" is not a sbt command but the name of a sbt plugin.

Let's run the program:

```
$ java -jar ./target/scala-3.7.4/random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT.jar

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

<br/>

Otherwise, you may have a look at: [Running and building Scala programs: baby steps](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/README.md#running-and-building-scala-programs-baby-steps)

Originally, these pages have been created in April 2025 for another GitHub account and have now been moved to this account, albeit re-structured with even more subpages for even better orientation.

So, this documentation may be already outdated in the fast evolving environment of Scala.

<br/>

##_end
