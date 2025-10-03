# From a Scala program to JavaScript for the web browser

In Windows 11 the official Scala to JavaScript demo didn't work for me, that is in the sbt taking option _m) scala-js/vite.g8 - A Scala.JS + Vite project_, but in Ubuntu 24 LTS!

I did this:

## Install Vite and Yarn

At first, I installed Vite (a frontend build tool) and Yarn (a JavaScript package manager).

From this source: https://tecadmin.net/install-yarn-on-ubuntu-22-04/ I took method 1:

```
$ sudo apt update
$ sudo apt install nodejs npm
$ sudo npm install --global yarn
$ yarn --version
1.22.22
$ nodejs --version
v18.19.1
$
```

## Build the default demo project with the sbt

Run in the parent directory of the new project root directory:

_$ sbt new_

Here take option _m) scala-js/vite.g8 - A Scala.JS + Vite project_

As usual, just press the [m] key, do not press [ENTER] here, or any other option!

_name_: press [ENTER] to accept demo project name "scalajs-vite-example"

_use_yarn_: press [ENTER] to use yarn

...

_.\Template applied in Template applied in ... ./scalajs-vite-example_

Then change into the project root directory:

_$ cd ./scalajs-vite-example_

Now do the background build of this demo project after every source code change ("~"):

```
_$ sbt ~fastLinkJS_
...
[success] Total time: 15 s, completed Apr 2, 2025, 9:12:40 PM
[info] 1. Monitoring source files for root/fastLinkJS...
[info]    Press <enter> to interrupt or '?' for more options.  # here I press the [ENTER] key because I don't open a second terminal for the next steps
[info] Received input event: CancelWatch.
[info] shutting down sbt server
```

Do this for every new project:

```
$ yarn
...
yarn install v1.22.22
info No lockfile found.
[1/4] Resolving packages...
[2/4] Fetching packages...
[3/4] Linking dependencies...
[4/4] Building fresh packages...
success Saved lockfile.
Done in 15.69s.
$
```

<br/>

Now start the web server:

```
$ npm run dev
> scalajs-vite-example \@ 0.1.0-SNAPSHOT dev
> vite
[info] welcome to sbt 1.8.2 (Eclipse Adoptium Java 11.0.26)
[info] loading settings for project scalajs-vite-example-build from plugins.sbt ...
[info] loading project definition from ... ./Scala/scalajs-vite-example/project
[info] loading settings for project root from build.sbt ...
[info] set current project to scalajs-vite-example (in build file: ... ./scalajs-vite-example/)
... ./scalajs-vite-example/target/scala-3.2.2/scalajs-vite-example-fastopt
VITE v4.5.11  ready in 3764 ms
➜  Local:   http://localhost:5173/
➜  Network: use --host to expose
➜  press h to show help
```

<br/>

Now open a local web browser to the given address http://localhost:5173/ -- but be careful not to stop the web server with wrong mouse clicks or so in this reactive Terminal (otherwise restart it with running _npm run dev_ again):

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/hello_world_from_vite.png)

<br/>

## Building my own little demo project with the sbt

After this initial success (though Vite complained about _Sourcemap for "... .js" points to missing source files_ four times), I started another demo project with only a few changes in a different project root directory, here named _hello_world2_with_sbt_:

_$ sbt new_ --> after selecting option m) again, enter _hello_world2_with_sbt_ as the project name for example

<sbt is doing its stuff>

Now I worked a bit on the main source code file, the project configuration file and the project directory structure:

### Change #1

Starting with the project root directory, I moved file _./src/main/scala/example/Main.scala_ to _./src/main/scala/main/Main.scala_

Having a _main_ subdirectory under directory _scala_ isn't very creative indeed, it's just a replacement for _example_ from the sbt.

I modified _Main.scala_ like this:

```
package main
@main def something(): Unit =  // @main is obviously important, def xxxx() is not
  // some more fancy HTML code:
  dom.document.querySelector("#app").innerHTML = s"""
  <head>
    <style>
      .my-element {
        width: 600px; /* Set the width to 600 pixels */
      }
    </style>
  </head>
  <body>
    <div class="my-element">
      __** this is an example element with a constant width of 600 pixels **__
    </div>
  </body>
  """
```

### Change #2

In the project configuration file _.\hello_world2_with_sbt\build.sbt_ change the project name to:

_name := "**hello_world2_with_sbt**",_

Change this line with the change in bold format: _.withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("**main**")))_

Principially, keep the sbt directory structure _...\<project root dir\>/src/main/scala/main_, even with a duplicate _main_ name.

Link the project updates:

_$ sbt ~fastLinkJS_

Open another terminal and run in the background:

_$ yarn_

_$ npm run dev  # start the web server_

Open the web browser again at: http://localhost:5173/ =>

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/hello_world2_with_sbt_from_vite.png)

## Sources for more elaborate examples

See from here: https://www.scala-js.org/doc/tutorial/basic/

See other advanced examples from here: https://scribble.ninja/

<br/>

##_end
