2026-01-31: work in progress

- extra exe speed diagram: JavaScript, TypeScript + ReScript??

<br/>

# From "back-end" to "front-end" programming languages

These are quick implementations of the speed part of the microbenchmark program to be executed on [node.js](https://nodejs.org/en).

Though web programming was not even on my long list, I got the idea to implement the microbenchmark program in web programming languages for two reasons:

- transpiling from Standard ML to JavaScript with [LunarML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml): [random_streams_for_perf_stats.mjs](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats.mjs), and
- my [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy) implementation, with Groovy often being described as a "scripting language for the Java Virtual Machine", and which "can largely be viewed as a superset of Java": [Introducing Groovy](https://www.oracle.com/technical-resources/articles/java/groovy.html)

<br/>

From that point on, it was only a small step to transpile program [random_streams_for_perf_stats.groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy/random_streams_for_perf_stats.groovy), which is here type annotated for speedy, static compilation,
with the help of Duck.ai (because the [tsc compiler](https://manpages.debian.org/testing/node-typescript/tsc.1.en.html), version 5.9.3, tumbled over warnings):

- first, into [TypeScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20node.js%20for%20%22web%20languages%22/random_streams_for_perf_stats.ts), and
- then from there into [JavaScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20node.js%20for%20%22web%20languages%22/random_streams_for_perf_stats.js), again with Duck.ai

<br/>

The TypeScript script should work out of the box for node.js version v22.21.0 or higher (_$ node -v_). Version 18.19.1, coming as standard with Ubuntu 24 LTS for example, is too old for it for example.

In Linux you can upgrade the version with the nvm (the Node Version Manager for a node.js installation per Linux user) like this (see from here: https://linux.how2shout.com/how-to-install-nvm-on-ubuntu-24-04-or-22-04-linux/):

```
$ sudo apt install curl build-essential libssl-dev -y
...
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
...
$ source ~/.bashrc  # re-activate your Bash shell configuration
$ nvm --version  # check nvm version
0.40.4
$ nvm install --lts  # install latest long term support version
...
$ nvm ls  # check all locally available versions, including the default version
...
$ node -v
v24.13.0  # this version is pretty modern (though, I'm still using v.22.21.0 for benchmarking)
$ node ./random_streams_for_perf_stats.ts

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

Voilà!

I think this is the easiest way before getting into another version hell to make this little TypeScript script running in older node.js environments.

<br/>

This leaves this question to me: 

## Why is the TypeScript variant slower than the equivalent JavaScript variant?

<TBD: exe speed diagram>

Transpiling from TypeScript code into JavaScript code, even though with the transpiler being re-written in [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/60%20-%20the%20future%20of%20transpiling#microsofts-efforts-with-transpilation) some day, comes with an overhead, even with this little example, which takes more time to process.

Since JavaScript engines, like node.js, are optimized for running JavaScript, TypeScript’s potentially more complex data structures might also not benefit from these optimizations.

<br/>

##_end
