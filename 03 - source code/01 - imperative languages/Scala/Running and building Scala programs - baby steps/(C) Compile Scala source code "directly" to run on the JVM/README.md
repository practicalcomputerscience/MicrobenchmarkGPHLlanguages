# Compile Scala source code "directly" to run on the JVM

Again, have a dedicated project (root) directory for this Scala source code file, here named _hello_world_for_java.scala_:

```
object hello_world_for_java:
  def main(args: Array[String]): Unit =
    println("Hello, world for Java!")
```

Compile it on Windows 11 like this:

_> scalac hello_world_for_java.scala_

The Scala compiler generated these 3 files in the same (Windows) directory:

_hello_world_for_java$.class_

_hello_world_for_java.class_

_hello_world_for_java.tasty_

<br/>

Two more files are needed to execute the _java_ command:

_scala-library.jar_

_scala-reflect.jar_

I copied these Java Archive files from my Windows installation of Scala: _<my Windows Home dir>\.sbt\boot\scala-2.12.20\lib_ to the project root directory, that is here the same directory as the Scala source code file and run the compiled source code file on the JVM (Java Virtual Machine) like this:

_> java -cp .\scala-library.jar;.\scala-reflect.jar;.\ hello_world_for_java_

**_Hello, world for Java!_**

The _-cp_ option is for the "class search path of directories and zip/jar files"; see from output of command: _> java_

The real "trick" here is to also consider the relative path .\ at the class search path of directories!

<br/>

By the way: copy these two _.jar_ files also to your **Linux machine**, when you want to run there _java -cp .._ commands but cannot find these two files from a Linux installed Java Runtime Environment so easily. These _.jar_ files are portable from OS to OS.

<br/>

##_end
