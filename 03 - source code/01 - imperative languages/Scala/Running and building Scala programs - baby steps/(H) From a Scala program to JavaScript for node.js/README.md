> **Spoler alert**: this use case has practically no value for me because so far I couldn't find a solution for the open point described below at: [How to read from the console when executing JavaScript on node.js?](#how-to-read-from-the-console-when-executing-javascript-on-nodejs)

<br/>

# From a Scala program to JavaScript for node.js

## The Hello world! example

Start a new sbt project in a parent directory of the later project root directory:

_$ sbt new_

Again, choose **option m)**; see also from here: [Build the default demo project with the sbt](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(F)%20From%20a%20Scala%20program%20to%20JavaScript%20for%20the%20web%20browser/README.md#build-the-default-demo-project-with-the-sbt)

..and enter _scala_to_js_on_nodejs_ as the project name for example.

Rename sub directory _./src/main/scala/example_ to _./src/main/scala/main_ and change _example_ to _main_ in the _build.sbt_ configuration file too (just to get away from term "example").

Change application source code file _Main.scala_ to this, which is the **real change** compared to prior use case [From a Scala program to JavaScript for the web browser](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(F)%20From%20a%20Scala%20program%20to%20JavaScript%20for%20the%20web%20browser/README.md#from-a-scala-program-to-javascript-for-the-web-browser):

```
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }
}
```

See also from here: https://www.scala-js.org/doc/project/building.html

In the project root directory change configuration file _build.sbt_ like this:

```
…
    scalaVersion := "3.6.4",
…
         scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("main")))
    },
…
```

You can delete line _libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",_ because it's not needed here, only in a web app.

In config file _plugins.sbt_ in the _./project_ directory add this line:

```
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")
```

Again, you can look up the exact version numbers from here: https://mvnrepository.com/artifact/org.scala-js/sbt-scalajs_2.12_1.0/1.18.2

<br/>

Now in the project root directory do:

``` 
$ sbt
sbt:scala_to_js_on_nodejs>
sbt:scala_to_js_on_nodejs> compile
…
[success] …
sbt:scala_to_js_on_nodejs> fastLinkJS
…
[success] …
sbt:scala_to_js_on_nodejs>
```

See also from here: https://www.scala-js.org/doc/project/building.html

<br/>

Then run this app:

```
sbt:scala_to_js_on_nodejs> run
[info] Running Main.
Hello world!
[success] Total time: 0 s, completed Apr 13, 2025, 1:09:15 PM
sbt:scala_to_js_on_nodejs>
```

You can find the transpiled JavaScript file in this directory:

_./scala_to_js_on_nodejs/target/scala-3.6.4/scala_to_js_on_nodejs-fastopt/main.js_

Execute this JavaScript code on node.js like this:

```
$ node main.js
Hello world!
$
```

<br/>

Of course this can be done also in other directories, when _main.js_ has been copied there and maybe, as in this case, being renamed to a more descriptive name like _scala_to_js_on_nodejs.js_ for example:

```
$ mv main.js scala_to_js_on_nodejs.js
$ node scala_to_js_on_nodejs.js
Hello world!
$
```

See also more official advice from here: https://scala-cli.virtuslab.org/docs/guides/advanced/scala-js/

<br/>

## "Stopwatch" app in JavaScript for node.js

This little demo app doesn't implement a real stopwatch since no genuine computations are going on, but a timeout of 1,000 milliseconds:

```
//> using scala 3.6.4
import scala.scalajs.js.timers
import scala.scalajs.js.Date

object stopwatch_in_JavaScript {
  def main(args: Array[String]): Unit = {
    println("Welcome to a test of some imported scala.scalajs.js methods:")
    val sleep_ms = 1000
    val t0 = Date.now()
    val sleep = timers.setTimeout(sleep_ms) {
                  val t1 = Date.now()
                  val time_diff = t1 - t0
                  val out_str = "this was a waiting time of " + time_diff + " milliseconds"
                  println(out_str)
                }
  }
}
```

Again, check config files _plugins.sbt_ and _build.sbt_.

In _build.sbt_ line:

_scalaJSUseMainModuleInitializer := true,_

..is needed, otherwise the app cannot be linked with the _fastLinkJS_ command in the sbt.

Check for **%%%** in _build.sbt_:

_libraryDependencies += "org.typelevel" **%%%** "cats-effect" % "3.6.1"_

<br/>

> **Use %%% instead of %% when depending on other Scala.js library's**

See from here: https://www.scala-js.org/doc/project/linking-errors.html

<br/>

## The scala.scalajs.js package

The _scala.scalajs.js_ package is for "Types, methods and values for interoperability with JavaScript libraries."; see officially from here: https://javadoc.io/doc/org.scala-js/scalajs-library_2.12/latest/scala/scalajs/js/index.html

<br/>

## How to read from the console when executing JavaScript on node.js?

Often I like some **user interaction** with a computer program on the console and so I tried hard - but in vain so far - to write an interactive Scala-to-JavaScript program, which is then being executed by node.js.

I did't encounter these kind of problems when programming this in Scala programs for the JVM (Java Virtual Machine) or for Scala native apps.

Like this for example:

```
    var N_CHAR = 12
    var answer = false
    var answer_str = ""
    while !answer do {
      N_CHAR = 12
      var N_CHAR_STR = N_CHAR.toString
      print(f"\n\nAre $N_CHAR%s printable chars OK? 'y' or another integer number >= 8: ")
      answer_str = scala.io.StdIn.readLine()
      if answer_str == "y" then
        answer = true
      else
        try
          N_CHAR = answer_str.toInt
          if N_CHAR < 8 then
            print("enter an integer number >= 8 or 'y'")
          else
            answer = true
        catch
          // Exception in thread "main" java.lang.NumberFormatException: For an arbitrary input string like "sdaf"
          case ex: NumberFormatException =>
            print("enter an integer number >= 8 or 'y'")
    }
```

This part of a Scala program runs without any problems in a Windows or Linux terminal.

The very best I got so far is this:

```
import cats.effect.{IO, IOApp, ExitCode}
import fs2.io._  // for stdinUtf8 
import fs2.Stream
import fs2.text

object cats_effect3_io_example extends IOApp.Simple:
  var answer = ""

  def readNext: Stream[IO, String] = stdinUtf8[IO](1)
      .through(text.lines)
      .take(1)  // emits the first element of this stream
        
  def prnInput(line: String): IO[Unit] =
      IO.println(s"your input was: $line")
      answer = line
  
  val run: IO[Unit] = for {
    _ <- IO.println("enter a integer number from 1 to 99 or 'q' for quitting:")
    _ <- IO[answer]
    i <- readNext.parEvalMap(1)(prnInput).compile.drain
  } yield ExitCode.Success
```

This program runs like this for example:

```
./cats_effect3_io_example/target/scala-3.6.4/cats_effect3_io_example-fastopt$ node main
enter a integer number from 1 to 99 or 'q' for quitting:
56
your input was: 56
./cats_effect3_io_example/target/scala-3.6.4/cats_effect3_io_example-fastopt$
```

What I'm specifically **missing is a working example in Scala 3** which is handling more than one "IO stream" since what I want to accomplish finally is a dialog between the computer and its user.

It looks like that there's not much support in the Scala ecosystem for this.

For example look at this: [Cats Effect 3.x -- Console](https://typelevel.org/cats-effect/docs/std/console)

_Console provides common methods to write to and read from the standard console. Suited only for extremely simple console input and output, ..._

At the end of this page the authors write:

_readLine is not implemented for Scala.js. On Node.js consider using **fs2.io.stdin**._

fs2 is a different library for "Functional, effectful, concurrent streams for Scala" (https://fs2.io/#/) and using method _fs2.io.stdinUtf8\[IO\]()_ inside an _object cats_effect3_io_example extends IOApp.Simple:_ or something similar (see code from above) was only a very tiny step towards a potentially working solution, if any.

However, it's not my intentation to make something bigger, like a **client-server application**, for a simple computer-user dialog.

Anyhow, here's a list of links with more background information:

- https://typelevel.org/cats-effect/docs/getting-started
- https://github.com/slouc/concurrency-in-scala-with-ce
- https://gist.github.com/BalmungSan/d4a5d524cab529e18fbf05f100ec3296
- https://fs2.io/#/getstarted/example
- https://medium.com/@geethana.kattar/building-reactive-applications-with-cats-effect-and-fs2-in-scala-e427703887ab

<br/>

##_end
