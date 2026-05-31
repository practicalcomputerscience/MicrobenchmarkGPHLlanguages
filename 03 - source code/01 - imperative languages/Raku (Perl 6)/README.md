# Raku

A compiler named _Rakudo_ implements the Raku programming language (https://raku.org/):

- https://www.rakudo.org/
- https://github.com/rakudo/rakudo

.., here on the Moar ("Metamodel On A Runtime") Virtual Machine (**MoarVM**): https://www.moarvm.org/

NQP = [Not Quite Perl](https://github.com/Raku/nqp): "a lightweight Raku-like environment for virtual machines" and "is focused on being a high-level way to create compilers and libraries for virtual machines like MoarVM, the JVM, and others."

<br/>

Although Raku is (still) a very slow programming language ([Languages that were too slow](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#languages-that-were-too-slow)), its modern and more "computer science oriented" syntax is definitely an improvement over the syntax of Perl 5 from my point of view.

---

<br/>

## Installation tips

See from: https://rakubrew.org/ or https://raku.org/install

Start installation with running for example:

```
$ curl https://rakubrew.org/install-on-perl.sh | sh
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1383  100  1383    0     0   6417      0 --:--:-- --:--:-- --:--:--  6432
Downloading rakubrew...
Installing rakubrew to $HOME/.rakubrew ...
...
$
```

Then I added these paths to my _.bashrc_ file (run _$ source ~/.bashrc_ to activate any changes):

```
export PATH="$PATH:$HOME/.rakubrew/bin"
export PATH="$PATH:$HOME/.rakubrew/shims"
eval "$($HOME/.rakubrew/bin/rakubrew init Bash)"
```

The last command line (_eval_) makes sure that my Bash shell is always running the latest Rakudo version that I downloaded and installed.

Make sure that the _git_ program is installed in your Linux environment:

```
$ git --version
git version 2.48.1
```

Then I ran this command as a test:

```
$ rakubrew available
Available Rakudo versions:
   2009-02
   2009-03
   2009-04
   2009-05
   2009-06
...
D 2025.08
   main

Available backends:
  jvm
  moar
  moar-blead
$
```

Then I downloaded the latest Rakudo on MoarVM compiler version. Actually, this is the central command to install Raku in a Linux system:

```
$ rakubrew download
Downloading https://rakudo.org/dl/rakudo/rakudo-moar-2025.08-01-linux-x86_64-gcc.tar.gz
Extracting
Updating shims
Switching to moar-2025.08
Done, moar-2025.08 installed
$
```

At last, I did a small test:

```
$ raku -e 'say "Now running {$*RAKU.compiler.version}!"'
Now running 2025.08!
$
```

2026-05-31: switch to a more modern version of Rakudo for example like this: 

```
$ rakubrew switch moar-2026.04
Switching to moar-2026.04
$ raku -e 'say "Now running {$*RAKU.compiler.version}!"'
Now running 2026.04!
$ rakudo-m --version  # for checking the compiler for target MoarVM
Welcome to Rakudo™ v2026.04.
Implementing the Raku® Programming Language v6.d.
Built on MoarVM version 2026.04.
$
```

<br/>

## Compiling a Raku script for the Java Virtual Machine (JVM)

See also from [Configuring Rakudo to run on the JVM](https://github.com/rakudo/rakudo/#configuring-rakudo-to-run-on-the-jvm), though these procedures didn't work for me!

tbd: _rakudo-2026.05.tar.gz_

After some experimentation, I found a way how to build and install Rakudo with target JVM! (tbd: really?)

I could even use a modern Java version:

```
$ java --version
openjdk 25.0.3 2026-04-21
OpenJDK Runtime Environment (build 25.0.3+9-2-24.04.2-Ubuntu)
OpenJDK 64-Bit Server VM (build 25.0.3+9-2-24.04.2-Ubuntu, mixed mode, sharing)
$
```

If several JDK installations are available, you can select a specific one like this:

```
$ sudo update-alternatives --config java
...
  0            /usr/lib/jvm/java-25-openjdk-amd64/bin/java      2511      auto mode
...
$ java --version
...
$
```

### Compiling for standard target MoarVM

First, start with the easier and usual target MoarVM before trying to move to the JVM. In extracted subdirectrory _./rakudo-2026.05_ run this command:

```
$ make clean  # clean up the mess of prior attempts
...
$ perl Configure.pl --backends=moar --gen-moar --relocatable
...
$ make -j1  # this may take some time! -j1 = force single-threaded compilation to stop memory leaks
...
$ make test
...
All tests successful.

Test Summary Report
-------------------
t/09-moar/Line_Break__LineBreak.t                             (Wstat: 0 Tests: 2 Failed: 0)
  TODO passed:   1-2
t/12-rakuast/xx-fixed-in-rakuast.rakutest                     (Wstat: 0 Tests: 114 Failed: 0)
  TODO passed:   32, 106
Files=173, Tests=2822, 27 wallclock secs ( 0.68 usr  0.19 sys + 170.06 cusr 13.97 csys = 184.90 CPU)
Result: PASS
$ sudo make install
+++ Rakudo installed successfully!
$ ./rakudo-m ../random_bitstring_and_flexible_password_generator.raku  # make a real test!

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$ 
```

### Compiling for target JVM

```
$ perl Configure.pl --backends=jvm --gen-moar --relocatable
...
$ make -j1
...
$ make test
...

```



raku --target=jar --output=app.jar app.raku



<br/>

##_end
