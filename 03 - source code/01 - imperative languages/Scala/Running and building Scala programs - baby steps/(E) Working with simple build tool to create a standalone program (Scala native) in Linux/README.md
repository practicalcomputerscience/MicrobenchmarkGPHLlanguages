# Working with simple build tool to create a standalone program ("Scala native") in Linux

If not happened yet, install the _clang_ compiler first. LLVM ("Low Level Virtual Machine") is not needed according to my tests. I used this description: https://www.cyberithub.com/how-to-install-clang-tool-on-ubuntu-or-debian-linux/ ("How to Install clang tool on Ubuntu or Debian Linux") and installed it like this:

_$ sudo apt install clang_

Then check the version and its availability:

_$ clang --version_

_Ubuntu clang version 18.1.3 (1ubuntu1)_

_Target: x86_64-pc-linux-gnu_

_Thread model: posix_

_InstalledDir: /usr/bin_

<br/>

## Install Scala on Ubuntu and test it a little bit

You can install Scala on Ubuntu like this:

```
$ curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs && ./cs setup
...
Checking if a JVM is installed
...
No JVM found, should we try to install one? [Y/n]
```

Here press the [Y] key!

In case of a problem, here's the official installation guide for **Coursier**: https://get-coursier.io/docs/cli-installation

_These instructions will install the coursier CLI cs itself, as well as a typical Scala development environment._

Now a reboot of the Linux machine is needed, not only a re-opening of the Linux shell.

Check the installation after the reboot:

_$ scala -version_

_Scala code runner version: 1.5.4_

_Scala version (default): 3.6.4_

<br/>

## Start a Scala project

Now open the Bash shell (terminal) and set your working directory so that it will be **the parent directory of the later project root directory**:

_$ sbt new_  # create a new Scala project; this command also creates the project root directory

_Welcome to sbt new!_

_Here are some templates to get started:_
…

**Select option d)** with only pressing key [d] (and not pressing a following [ENTER] key): _d) scala/scala3.g8 - Scala 3 seed template_

Now enter your project name which is also the name of the project root directory in the filesystem of the OS:

_name [Scala 3 Project Template]: test_sbt_Linux_  # this is only an example project name

_Template applied in ./\<project name\>_

See also from this use case: [Select option d)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(D)%20Working%20with%20simple%20build%20tool%20to%20run%20an%20app%20on%20the%20JVM%20in%20Windows%20with%20(3rd%20party)%20imports/README.md#select-option-d)

<br/>

Optionally, edit the _**build.sbt**_ file in _./\<project root dir\>/build.sbt_: often one or more library dependencies have to be added there, here for example:

_libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"_

You may look up the exact version of a library from searching here: https://mvnrepository.com/
	
<br/>

Now change into the project root directory:

_$ cd \<project root dir\>_

..and start the sbt (simple build tool) again:

_$ sbt_

_[info] welcome to sbt 1.10.11 (Eclipse Adoptium Java 11.0.26)_

_[info] loading project definition from /home/ ... /\<project name\>/project_

_[info] loading settings for project root from build.sbt..._

_[info] set current project to \<project name\> (in build file:/home/\<user\>/ ... /\<project name\>/)_

_[info] sbt server started at local:///home/\<user\>/.sbt/1.0/server/35e75b11c375067c3e8d/sock_

_[info] started sbt server_

<br/>

Now the sbt prompt should show up:

_sbt:\<project name\>>_

Now build and run this demo project like this:

_sbt:\<project name\>> run_

There should be success now:

_[info] compiling 1 Scala source to /home/\<user\>/ ... /\<project name\>/target/scala-3.6.4/classes ..._

_[info] running hello_

_**Hello world!**_

_I was compiled by Scala 3. :)_

_[success] Total time: 3 s, completed Apr 12, 2025, 10:41:22 AM_

<br/>

## How to make this application "Scala native"?

I followed this advice at chapter "Compiling and running your Scala Native application" from:

_Getting Started with Scala Native: A Comprehensive Guide for Beginners_

https://medium.com/@diehardankush/getting-started-with-scala-native-a-comprehensive-guide-for-beginners-dedafeed7f25 by Ankush Singh, Apr 23, 2023

…and found this command:

_sbt:<project name>> **nativeLink**_

_[error] Not a valid command: nativeLink_

_[error] Not a valid project ID: nativeLink_

_[error] Expected ':'_

_[error] Not a valid key: nativeLink_

_[error] nativeLink_

_[error]           ^_

_sbt:\<project name\>>_

<br/>

Apparently, the _build.sbt_ file must be fixed first.

**Without leaving the sbt**, open a second terminal (and keep it running!) and **append** line _enablePlugins(ScalaNativePlugin)_ like this for example (you could also use a text editor for doing this parallel job):

_\<project root dir\>/project$ echo "enablePlugins(ScalaNativePlugin)" > build.sbt_

You may also check success of this operation like this for example:

_\<project root dir\>/project$ cat build.sbt_

```
val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "test_sbt_Linux",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

enablePlugins(ScalaNativePlugin)
```

<br/>

Then the _plugin.sbt_ file must be added in the already existing _./project_ sub directory and have this plugin being added:

_\<project root dir\>$ cd project_

_\<project root dir\>/project$ echo "addSbtPlugin(\"org.scala-native\" % \"sbt-scala-native\" % \"0.5.8\")" > plugin.sbt_

I got this latest Scala native version (as of September 2025) number from here: https://www.scala-native.org/en/stable/

Again, you can check success like this:

_\<project root dir\>/project$ cat plugin.sbt_

```
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.8")
```

<br/>

You may edit the _Main.scala_ source code file in the _\<project root dir\>/src/main/scala_ sub directory like this as a test:

```
@main def hello(): Unit =
  println("Hello world from Scala Native with version 0.5.8!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
```

<br/>

Now return to the terminal with the sbt running and apply these commands:

_sbt:\<project name\>> reload_  # reload the changed project configuration

\<hopefully some downloading is happening here\>
	
Do again:

_sbt:\<project name\>> nativeLink_

\<hopefully some more downloading is happening here and then some building\>

_[success] Total time: 5 s, completed Apr 12, 2025, 11:27:35 AM_

_sbt:\<project name\>>_

<br/>

Make a run from the sbt as a test:

_sbt:\<project name\>> run_

_[info] Build skipped: No changes detected in build configuration and class path contents since last build._

**_Hello world from Scala Native with version 0.5.8!_**

_I was compiled by Scala 3. :)_

_[success] Total time: 0 s, completed Apr 12, 2025, 11:29:53 AM_

_sbt:\<project name\>>_

<br/>

**The executable** can now be found in sub directory _\<project root dir\>/target/scala-3.6.4/_ with file name **\<project name\>**

Run it in the second terminal (so still keep the sbt running in the first terminal):

_\<project root dir\>/target/scala-3.6.4$_ ./\<project name\>

_Hello world from Scala Native with version 0.5.8!_

_I was compiled by Scala 3. :)_

_\<project root dir\>/target/scala-3.6.4$_

Bingo!

<br/>

As a second test, I copied file \<project name\> to another Ubuntu 24 LTS system and run it there (with no Scala, Java etc. resources installed).

Don't forget to do first: _$ chmod 774 ./_\<project name\>

_$_ ./\<project name\>

_Hello world from Scala Native with version 0.5.8!_

_I was compiled by Scala 3. :)_

Double Bingo!

<br/>

Finally leave this sbt session with the exit command and return to the Bash shell prompt:

_sbt:\<project name\>> exit_

_[info] shutting down sbt server_

_$_

<br/>

I also tried option c) at the _sbt new_ command:

_c) sbt/cross-platform.local - A cross-JVM/JS/Native project_

..but was not really sure so far what to make out of the _./\<project root dir\>/core_ sub directory and all the other created artefacts.

<br/>

Fun fact: a cool 6.711 items, totalling 34,5 MB, have been created for this project so far! 

<br/>

##_end
