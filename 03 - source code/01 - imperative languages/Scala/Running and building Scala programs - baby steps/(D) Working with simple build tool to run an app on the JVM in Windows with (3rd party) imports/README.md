# Working with simple build tool to run an app on the JVM in Windows with (3rd party) imports

Open the Windows command prompt ("cmd.exe"): ...\> 

> Set your working directory so that it will be the parent directory of the later project root directory.

<br/>

Now create a new Scala project; this command also creates the project root directory:

_\> sbt new_

_Welcome to  new!_

_Here are some templates to get started:_

_..._

### Select option d)

..with only pressing key [d] (and not pressing a following [ENTER] key): _d) scala/scala3.g8 - Scala 3 seed template_

Now enter your project name which is also the name of the project root directory in the filesystem of the OS:

_name [Scala 3 Project Template]:_ _test_sbt_Windows_  # this is only an example project name

_Template applied in .\<project name>_
	
![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/sbt_new_Windows.png)

<br/>

Optionally, edit the _**build.sbt**_ file in _.\<project root dir>\build.sbt_: often one or more library dependencies have to be added there, here for example:

_libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"_

You may look up the exact version of a library from searching here: https://mvnrepository.com/
	
<br/>

Now change into the project root directory:

_\> cd \<project root dir\>_

..and start the sbt (simple build tool) again:

_\> sbt_

_[info] welcome to sbt 1.10.11 (Oracle Corporation Java 1.8.0_441)_

_[info] loading project definition from ... .\<project name>\project_

_[info] loading settings for project root from build.sbt..._

_[info] set current project to <project name> (in build file: ... ./\<project name\>/)_

_[info] sbt server started at local:sbt-server-84a7d2d60ce9c59ce491_

_[info] started sbt server_

<br/>

Now the sbt prompt should show up:

_sbt:\<project name\>>_


Now build and run this demo project like this:

_sbt:\<project name\>> run_

There should be success now:

 _[info] compiling 1 Scala source to … .\<project name>\target\scala-3.6.4\classes ..._

_[info] running hello_

_**Hello world!**_

_I was compiled by Scala 3. :)_

_[success] Total time: 3 s, completed 05.04.2025, 21:44:50_

_sbt:\<project name\>>_

<br/>
 
Now you can edit the Scala source code file with the main function (_@main def hello(): Unit =_ or something like this) at this relative path:

```
.\<project name>\src\main\scala\Main.scala
```

In the meantime you can put the sbt into listening mode with the "~" character before a command to start building and running again when the _Main.scala_ file has been changed by you:

 _sbt:\<project name\>> ~run_

_[info] 1. Monitoring source files for root/run..._

_[info]    Press <enter> to interrupt or '?' for more options._

_[info] Build triggered by … .\<project name>\src\main\scala\Main.scala. Running 'run'._
	
_[info] compiling 1 Scala source to … .\<project name>\target\scala-3.6.4\classes ..._

_[info] running hello_

_**Hello world!++**_

_I was compiled by Scala 3. :)_

_[success] Total time: 0 s, completed 05.04.2025, 21:49:33_

_[info] 2. Monitoring source files for root/run..._

_[info]    Press <enter> to interrupt or '?' for more options._

<br/>

We end this monitoring session now, so press key [ENTER]:

_[info] Received input event: CancelWatch._

...and finally leave this sbt session with the _exit_ command:

_sbt:\<project name\>> exit_

_[info] shutting down sbt server_

Now we are back at the Windows Terminal:

\>

<br/>

When you want to continues with this project, do this: go to the project root directory and just enter (in the Windows Terminal): _\> sbt_

_[info] welcome to sbt 1.10.11 (Oracle Corporation Java 23.0.2)_
_..._

<br/>

## (D) as a basic workflow for my Scala programming

This workflow (D), be it in Windows or Linux, is a basic workflow for my (little) Scala programming. It involves four key concepts:

1. the starting point is opening an OS shell and changing to a directory which will be the parent directory of the later project root directory
2. remember that with the _> sbt new_ command from the OS shell and setting a project name at: _name [Scala 3 Project Template]:_ inside the simple build tool you will automatically create the OS project root directory!
3. do not forget to work on the _build.sbt_ configuration file located in the project root directory
4. get familiar with operating the sbt, often having a leading "~" character at a sbt prompt in the background, while working in the foreground on Scala source code files and project configuration files

It took me a while to become familiar with this workflow.

<br/>

##_end
