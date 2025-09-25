# Inko

The implementation in **Inko** (https://inko-lang.org/) is the first I upload after the original Python one as shown at the top README: https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages

I deem this version now my final one for Inko after I struggled to correctly build and install the Inko compiler (before I had to use compiler switch _--opt=none_, which indeed makes a slower program), which is, like some other programming languages, based on the ubiquitous **LLVM** compiler infrastructure: https://llvm.org/

Now I can compile without any problems like this for example:

```
$ inko build random_streams_for_perf_stats.inko --opt aggressive
```

I cannot say if other LLVM versions than 17 are working for Inko version 0.18.1 too (see from: _$ inko --version_), however I guess so (LLVM version 18, 19, 20, ...).

<br/>

The most important issue here from my point of view is that the _**Polly - Polyhedral optimizations for LLVM**_ (https://polly.llvm.org/index.html) are correctly installed too for the LLVM version in use for Inko. And the easiest way to assure this from my point of view is to install them in your Ubuntu Linux like this (see _libpolly-17-dev_):

```
$ sudo apt-get install --yes rustc cargo git build-essential llvm-17 llvm-17-dev libstdc++-11-dev libclang-common-17-dev zlib1g-dev libpolly-17-dev libzstd-dev
```

<br/>

By the way: the LLVM in use by Rust (the Inko compiler is written in Rust: https://github.com/inko-lang/inko/tree/main/compiler) is not relevant here. Rust ships with its own LLVM version:

```
$ rustc -v -V
rustc 1.88.0 (6b00bc388 2025-06-23)
binary: rustc
commit-hash: 6b00bc3880198600130e1cf62b8f8a93494488cc
commit-date: 2025-06-23
host: x86_64-unknown-linux-gnu
release: 1.88.0
LLVM version: 20.1.5
$
```

See also the comment block of the source code files for some more information for installations like **first** installing Inko version manager _ivm_:

```
$ cargo install ivm --force
```

Also have a view at your _.bashrc_ file for a path to this program:

```
export PATH="$HOME/.cargo/bin/ivm:$PATH"
```


<br/>

### Installation tips

for Inko on Ubuntu 24 LTS. See also from here: https://github.com/orgs/inko-lang/discussions/819#discussioncomment-13823413

This is just a copy here:

#### Re Ubuntu 24

The Inko installation for Ubuntu version 24 guide has an omission: https://docs.inko-lang.org/manual/latest/setup/installation/#ubuntu

Also for Ubuntu 24.04 - and probably newer - the **Polly LLVM framework** (https://polly.llvm.org/index.html) must be included during installation of the LLVM framework, at least for version 17 (which works for me): see _libpolly-17-dev_

You can do this with this command on the Bash shell:  

`$ sudo apt-get install --yes rustc cargo git build-essential llvm-17 llvm-17-dev libstdc++-11-dev libclang-common-17-dev zlib1g-dev libpolly-17-dev libzstd-dev
`

This is the same related command as given for Ubuntu 23.10.

However, it's not a problem when you have only applied given command for Ubuntu 24. Because you can add the Polly optimizer afterwards like this:

`$ sudo apt-get install --yes libpolly-17-dev
`

LLVM version 17, with the command as given above, will be installed into directory: _/usr/lib/llvm-17_

Because without Polly you cannot install the Inko compiler with installation manager _ivm_ like this for example (that is that the Inko compiler cannot be compiled without Polly being available):

`$ ivm install latest
`

In Ubuntu you may or should also add some configurations in your _**.bashrc**_ file, for example like this (even when it may look a little bit redundant, but maybe also helpful as an idea when the LLVM framework has been built and/or installed differently):

```
export LLVM_HOME="/usr/lib/llvm-17"
export PATH=$LLVM_HOME/bin:$PATH
export PATH="$HOME/.cargo/bin/ivm:$PATH"
export LLVM_SYS_170_PREFIX="/usr/lib/llvm-17"
export LLVM_SYS_181_PREFIX="/usr/lib/llvm-17"
export LD_LIBRARY_PATH=$LLVM_HOME/lib:$LD_LIBRARY_PATH
export PATH="$PATH:$HOME/.local/share/ivm/installed/0.18.1/bin"
```

After changes, don't forget to re-active this file by applying command: 

`$ source ~/.bashrc
`

##_end
