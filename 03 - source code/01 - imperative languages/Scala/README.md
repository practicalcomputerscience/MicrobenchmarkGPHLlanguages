# Scala

https://www.scala-lang.org/

https://www.scala-sbt.org/

---

## Installation tips

First, I installed the OpenJDK (Open Java Development Kit), see from here at [Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#installation-tips)

I continued with installing the **Coursier** application and artifact manager to install Scala app's as decribed here: https://get-coursier.io/docs/cli-installation

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

In Ubuntu, also expand the _PATH_ environment variable in your _~/.bashrc_ config file to have access to the Scala app's as listed above:

```
export PATH="$PATH:~/.local/share/coursier/bin"
```

Have some tests now:

```
$ sbt --version
sbt runner version: 1.11.7
...
$ scala --version
Scala code runner version: 1.9.1
Scala version (default): 3.7.4
$
```

## Building tips

TBD

<br/>

Otherwise, you may have a look at: [Running and building Scala programs: baby steps](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/README.md#running-and-building-scala-programs-baby-steps)

Originally, these pages have been created in April 2025 for another GitHub account and have now been moved to this account, albeit re-structured with even more subpages for even better orientation.

So, this documentation may be already outdated in the fast evolving environment of Scala.

<br/>

##_end
