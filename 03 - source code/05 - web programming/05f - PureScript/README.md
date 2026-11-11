2026-11-11: work in progress

<br/>

# PureScript

https://www.purescript.org/

> PureScript is a small, strongly, statically typed programming language with expressive types, written in and inspired by Haskell, and compiling to Javascript."

..from "the PureScript book": [PureScript By Example](https://book.purescript.org/#purescript-by-example)

Another quote from there in chapter [Functions and Records](https://book.purescript.org/chapter3.html#functions-and-records):

> ..we prefer pure functions and immutable data where possible.

Playground: https://try.purescript.org/

<br/>

## Installation tips

I started as officially advised at [Getting Started with PureScript](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md#getting-started-with-purescript):

```
$ npm install -g purescript

added 108 packages in 7s

20 packages are looking for funding
  run `npm fund` for details
npm notice
...
$ purs --version  # test the PureScript compiler
0.15.16
$ npm install -g spago  # installing the Spago package manager and build tool
npm warn deprecated glob@11.1.0: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me

added 73 packages in 9s
npm warn install-scripts 2 packages have install scripts not yet covered by allowScripts:
npm warn install-scripts   ssh2@1.17.0 (install: node install.js)
npm warn install-scripts   cpu-features@0.0.10 (install: node buildcheck.js > buildcheck.gypi && node-gyp rebuild)
npm warn install-scripts
npm warn install-scripts Run `npm install -g --allow-scripts=ssh2,cpu-features` to allow these scripts once, or `npm config set allow-scripts=ssh2,cpu-features --location=user` to allow them for all global installs.
$ spago --version  # I ignored above suggestion to make these allowances (for now)
1.0.5
$ mkdir my-project  # make a test project
$ cd my-project
$ spago init
Refreshing the Registry Index...
...
Set up a new Spago project.
Try running `spago run`
$ spago build
Reading Spago workspace configuration...
...
Building...
[ 1 of 59] Compiling Type.Proxy
...
[59 of 59] Compiling Test.Main
           Src   Lib   All
Warnings     0     0     0
Errors       0     0     0

✓ Build succeeded.

$ spago test
Reading Spago workspace configuration...
...
✓ Test succeeded for package "my-project".

$
```

<br/>

However, one part (at least) of this installation guide is no longer working, because just doing _$ spago bundle-app_ no longer works and the apparently new command _$ spago bundle_ also fails without any prerequisites!

As PureScript claims that it "compiles to JavaScript", my goal is to transpile a PureScript script into - at best - one JavaScript script and run that on **node.js**.

I should note that a simple spago run works out of the box (and also produces some extra output in quiet mode):

```
$ spago run --quiet
           Src   Lib   All
Warnings     0     0     0
Errors       0     0     0
🍝
$
```

After some research I found out that the difficulty lies in the fact that an _esbuild_ executable must be found for _$ spago bundle_ to work.

This can be done at least locally for every project. I didn't try to find a more global solution:

```
$ npm install --save-dev esbuild
$ npm approve-scripts esbuild
$ PATH="$PWD/node_modules/.bin:$PATH" spago bundle  # expand PATH to the esbuild executable
$ node ./index.js  # run the JavaScript script on node.js
🍝
$ 
```

So, when you change the PureScript source code in _./<project directory>//src/Main.purs_, just run _$ PATH="$PWD/node_modules/.bin:$PATH" spago bundle_ again.




tbd


<br/>

##_end
