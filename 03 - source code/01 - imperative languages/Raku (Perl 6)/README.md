# Raku

https://raku.org/

The Raku compiler named _Rakudo_:
- https://www.rakudo.org/
- https://github.com/rakudo/rakudo

Moar Virtual Machine (**MoarVM**) for NQP (a lightweight Raku-like environment for virtual machines) And Rakudo: https://www.moarvm.org/

See also from here: [Raku](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#raku)

<br/>

### Installation tips

I built and installed my Rakudo environment from this source: https://github.com/rakudo/rakudo/releases/tag/2025.08

Unzip archive file _rakudo-2025.08.tar.gz_ and then run in the Bash shell:

```
$ cd rakudo-2025.08
$ perl Configure.pl --gen-moar --gen-nqp --backends=moar
...
$ make
...
$ make test
...
$ make install
...
$ 
```

I also added these paths to my _.bashrc_ file (run _$ source ~/.bashrc_ to activate any changes):

```
export PATH="$PATH:$HOME/.rakubrew/bin"
export PATH="$PATH:$HOME/.rakubrew/shims"
eval "$($HOME/.rakubrew/bin/rakubrew init Bash)"
```

The last command line (_eval_) makes sure that my Bash shell is always running the latest Rakudo version that I downloaded and installed.

I ran this command as a test:

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

Then I downloaded the latest Rakudo on MoarVM version:

```
$ rakubrew download
Downloading https://rakudo.org/dl/rakudo/rakudo-moar-2025.08-01-linux-x86_64-gcc.tar.gz
Extracting
Updating shims
Switching to moar-2025.08
Done, moar-2025.08 installed
$
```

..and tested it:

```
$ raku -e 'say "Now running {$*RAKU.compiler.version}!"'
Now running 2025.08!
$
```

<br/>

It looks to me that the latest Rakudo on MoarVM version, here 2025.08, can run my [microbenchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Raku%20(Perl%206)/random_bitstring_and_flexible_password_generator.raku) a little bit faster, albeit still over 5 seconds execution time, than my prior installation with version 2022 that I (probably) installed with Ubuntu 24 LTS. 

<br/>

##_end
