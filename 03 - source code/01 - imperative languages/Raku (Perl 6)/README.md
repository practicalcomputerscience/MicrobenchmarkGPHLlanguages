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

See from: https://raku.org/nav/1/install

Start installation with running:

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

Then I downloaded the latest Rakudo on MoarVM version. Actually, this is the central command to install Raku in a Linux system:

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

<br/>

##_end
