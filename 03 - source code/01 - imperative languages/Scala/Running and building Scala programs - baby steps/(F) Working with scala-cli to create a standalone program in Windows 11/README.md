# Working with scala-cli to create a standalone program in Windows 11

"Standalone program" == "Scala native" program

In Windows the Scala shell, that is the command-line tool _scala-cli_, can be used to compile Scala source code into an executable Windows program.

If not done yet, download and install something like this: **Visual Studio Community 2022** with the "C++ for Desktop workload".

Manually add path to _cl.exe_ to the Windows _PATH_ environment variable; for example:

_C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.43.34808\bin\Hostx64\x64;_

Check availability of _cl.exe_ in a Windows Terminal ("cmd.exe"):

_> cl.exe_

_Microsoft (R) C/C++-Optimierungscompiler Version 19.43.34809 für x64_

...

Change to the directory of the source code file which contains _main_, here _hello_world.scala_:

```
@main def helloWorld() = println("Hello, world!")
```

This directory may contain only this source code file, nothing more is needed.

Run the build command like this. It may take some time the first time:

```
> scala-cli package hello_world.scala -o hello_world --native-image --power
Starting compilation server
Compiling project (Scala 3.6.4, JVM (17))
Compiled project (Scala 3.6.4, JVM (17))
<project dir>>chcp 437
Active code page: 437
...
----------------------------------------------------------------------
Produced artifacts:
<project dir>\hello_world.build_artifacts.txt (txt)
<project dir>\hello_world.exe (executable)
======================================================================
Finished generating 'hello_world' in 12,7s.
Wrote <project dir>\hello_world.exe, run it with
.\hello_world.exe
```

Run this program as just told to hopefully see this output:

_**Hello, world!**_

<br/>

## Is "Scala native" worth the effort?

I don't think so.

I think the natural runtime environment for Scala programs is (still) the Java Runtime Environment (JRE).

When I wrote a more elaborate Scala program, I noticed that running this program in the JRE is (substantially) **faster** than the "Scala native" version! :flushed:

This seems to be specifically true if a program is doing big and/or a lot of iterations.

See from here for example: https://www.oreilly.com/library/view/learning-java-5th/9781492056263/

> Historically, interpreters have been considered slow, but Java is not a traditional interpreted language. In addition to compiling source code down to portable bytecode, Java has also been carefully designed so that software implementations of the runtime system can further optimize their performance by compiling bytecode to native machine code on the fly. This is called just-in-time (JIT) or dynamic compilation. With JIT compilation, Java code can execute as fast as native code and maintain its transportability and security.

So, compiling Scala source code to OS specific binary code is not necessarily a better thing than compiling it to standard and portable bytecode for the JVM.

<br/>

##_end
