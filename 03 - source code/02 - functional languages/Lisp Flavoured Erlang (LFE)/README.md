2025-12-13: work in progress

# Lisp Flavoured Erlang (LFE)

After [Gleam](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Erlang%3A%20Gleam%20on%20vm%20BEAM#gleam), LFE is another functional language on Erlang's virtual machine (vm) [BEAM](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/) with "a lisp syntax front-end to the Erlang compiler":

- https://lfe.io/
- GitHub: https://github.com/lfe/lfe
- User Guide: https://github.com/lfe/lfe/blob/master/doc/src/lfe_guide.7.md
- rebar3 plugin for LFE projects: https://github.com/lfe/rebar3#upgrading-rebar3_lfe-globally

<br/>

TBD

## Installation tips

First, compile and install latest LFE version, here from: https://github.com/lfe/lfe/releases/tag/v2.2.0, in an unzipped working directory like: _./lfe-2.2.0_

```
$ make compile
$ make tests
...
$ sudo make install
$ lfe --version
Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 16.1.2
$
```

<br/>

Now, download and run the installation script for the **rebar3** build tool for Erlang (and other BEAM languages): https://www.rebar3.org/docs/getting-started/#installing-binary, in your local working directory:

```
$ rebar3 local install
..
===> Add to $PATH for use: export PATH=~/.cache/rebar3/bin:$PATH
$
$ mkdir ~/.config/rebar3
$ touch ~/.config/rebar3/rebar.config
$
```

Get the latest rebar3 plugin for LFE projects: https://github.com/lfe/rebar3/releases/tag/0.5.5, and add to _~/.config/rebar3/rebar.config_ file:

```
{plugins, [
    {rebar3_lfe, "0.5.5"}
]}.

{deps, [
    {lfe, "2.2.0"}
]}.
```

List rebar3 plugins as a first test:

```
$ rebar3 plugins list
===> Fetching rebar3_lfe v0.5.5
===> Fetching lfe v2.2.0
===> Analyzing applications...
===> Compiling lfe
===> Compiling rebar3_lfe
src/cl.lfe:472: Warning: redefining core function car/1
src/cl.lfe:479: Warning: redefining core function cdr/1
--- Global plugins ---
rebar3_lfe (0.5.5)

$
```

A little calculation as a second test:

```
$ rebar3 lfe eval '(+ 1 2 3)'
===> Verifying dependencies...
===> Compiling 0 LFE application(s)
6
$
```



TBD






<br/>

TBD

<br/>

TBD

<br/>

##_end
