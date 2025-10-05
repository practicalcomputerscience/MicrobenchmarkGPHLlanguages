# Raku

https://raku.org/

The Raku compiler named _Rakudo_:
- https://www.rakudo.org/
- https://github.com/rakudo/rakudo

Moar Virtual Machine (**MoarVM**) for NQP (a lightweight Raku-like environment for virtual machines) and Rakudo: https://www.moarvm.org/

See also from here: [Raku](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#raku)

---

### Installation tips

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

It looks to me that the latest Rakudo on MoarVM version, here 2025.08, can run my [microbenchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Raku%20(Perl%206)/random_bitstring_and_flexible_password_generator.raku) a little bit faster, albeit with still over 5 seconds execution time, than my prior installation with version 2022, which I installed with means of Ubuntu 24 LTS: _$ sudo apt install rakudo_

<br/>

##_end
