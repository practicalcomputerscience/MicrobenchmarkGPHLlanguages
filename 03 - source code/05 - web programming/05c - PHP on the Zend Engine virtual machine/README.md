# PHP

https://www.php.net/

<br/>

Using modern PHP (originally for "Personal Home Page" tools) on the [Zend Engine](https://www.zend.com/), a stack-based virtual machine
(same like [WebAssembly](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05a%20-%20web%20languages%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#the-webassembly-wasm-virtual-machine)) has become a real hit:

- the "speed part" of the microbenchmark program is executed in around just 15 milliseconds (as interpreted bytecode), which is just a little bit slower than ahead-of-time compiled Dart code: [Dart execution speeds diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05b%20-%20Dart%20on%20the%20Dart%20virtual%20machine#dart-execution-speeds-diagram)

Remember from facebook's [Hack](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#hack):

> ...also this language didn't survive, because PHP had its comeback...

<br/>

## Installation tips

In Ubuntu 24 LTS, I installed PHP and the Zend Engine like this:

```
$ sudo apt install php8.3-cli
$ cat ./hello_world.php  # a small PHP script for testing
<?php
echo "Hello, World from PHP!\n";
?>
$ php ./hello_world.php  # run script on the Zend Engine as compiled opcodes (bytecode) 
Hello, World from PHP!
$ 
```

<br/>

## JIT experiments

Since 2020, PHP can also employ just-in-time (JIT) compilation; see here at: [PHP 8.0: JIT](https://php.watch/versions/8.0/JIT), where PHP is then using [DynASM](https://luajit.org/dynasm.html) from [LuaJIT](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Lua#luajit). Also see [Configuring JIT ](https://php.watch/versions/8.0/JIT#jit-configure).

However, with this microbenchmark program, JIT compilation doesn't move the needle in terms of execution speed:

```
$ time php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./random_streams_for_perf_stats.php

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.015s
...
$ 
```

Same like with [Ruby version 4](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby/README.md#jit-experiments-with-ruby-version-4), I also tried here using JIT compilation for recursively calculating the 35th Fibonacci number:

```
<?php
function fib($n) {
    return $n <= 1 ? $n : fib($n - 1) + fib($n - 2);
}
$start_time = microtime(true);
echo fib(35) . "\n";
echo "Time: " . (microtime(true) - $start_time) . "s\n";
?>
```

First, normal interpretation of Zend bytecode:

```
$ php ./fib.php
9227465
Time: 0.3171238899231s
$
```

Now, with JIT compiled Zend bytecode:

```
$ php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./fib.php
9227465
Time: 0.098063945770264s
$
```

So, running arithmetic calculations with JIT compilation in modern PHP can have a substantial effect on the program execution time, here by -68% (measured with: _$ multitime -n 20 php \<switches\> ./fib.php_).

<br/>

#### Wrapping and executing a PHP script inside a node.js command

Due to these critical PHP vulnerabilities as of October 2026:

- https://www.php.net/archive/2026.php#2026-10-21-5
- https://teramont.net/blog/php2shell-cve-2026-109618-109619-php-update (*)

In Ubuntu 24 LTS, I automatically fixed these issues with first checking the current versions, and then doing normal updates and upgrades via my (default) distribution channels.

But first some version checkings:

```
$ php -v
PHP 8.3.6 (cli) (built: Oct  7 2026 18:06:38) (NTS)
Copyright (c) The PHP Group
Zend Engine v4.3.6, Copyright (c) Zend Technologies
    with Zend OPcache v8.3.6, Copyright (c), by Zend Technologies
$ dpkg-query -W 'php*' 'libapache2-mod-php*'
...
php8.3-cli	8.3.6-0ubuntu0.24.04.12
php8.3-common	8.3.6-0ubuntu0.24.04.12
...
$ sudo apt update
...
$ sudo apt upgrade
...
$ dpkg-query -W 'php*' 'libapache2-mod-php*'  # check again
...
php8.3-cli	8.3.6-0ubuntu0.24.04.13  # 24.04.13 means fixed according to (*)
php8.3-common	8.3.6-0ubuntu0.24.04.13  # 24.04.13 means fixed according to (*)
...
$ php -v
PHP 8.3.6 (cli) (built: Oct 19 2026 15:16:58) (NTS)
Copyright (c) The PHP Group
Zend Engine v4.3.6, Copyright (c) Zend Technologies
    with Zend OPcache v8.3.6, Copyright (c), by Zend Technologies
$
```

These security issued raised the question with me, if node.js can process PHP scripts. Not directly, but with some wrapping like this for example:

```
$ node -e "require('child_process').execFile('php', ['random_streams_for_perf_stats.php'], (e, o, s) => { if (e) throw e; process.stdout.write(o); process.stderr.write(s); })"

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

<br/>

##_end
