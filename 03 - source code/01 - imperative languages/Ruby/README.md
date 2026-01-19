# Ruby

https://www.ruby-lang.org/en/

https://github.com/planetruby/awesome-rubies

IR often stands for "Intermediate Representation", which is then a compiler's internal language.

---

Table of contents:

- [Execution speed](#execution-speed)
- [MJIT in 2018](#mjit-in-2018)
- [YJIT in 2021](#yjit-in-2021)
- [ZJIT in 2025](#zjit-in-2025)
- [Installation tips](#installation-tips)
- [mruby to make a standalone Ruby based app](#mruby-to-make-a-standalone-ruby-based-app)

<br/>

---

## Execution speed

While Ruby had a reputation to be rather on the slow side, this has apparently changed significantly in the last couple of years: [Ruby Performance Evolution: From 1.0 to Today](https://dev.to/daviducolo/ruby-performance-evolution-from-10-to-today-4hc0) from 17. Dez. 2024.

These claims of good execution speed motivated me to give Ruby a try (though I didn't even have it on my long list), only to be very positively surprised:

- [Python 3.12.3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_streams_for_perf_stats.py): 139 milliseconds 
- [Ruby 3.2.3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby/random_streams_for_perf_stats.rb): 73 milliseconds

..for the [speed part](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments) of this microbenchmark program.

Same like Python (called _StringIO_), Ruby has a string builder of the same name:

```
...
require 'stringio'
...
bits_x   = StringIO.new
...
  bits_x_str = format('%016b', x[i])
  bits_x.write(bits_x_str)
...
```

Playing with Ruby's (default) possibilities for just-in-time (JIT) compilation didn't improve the execution speed:

command | exe time (real of time command in a single run)
--- | ---
_$ ruby ./random_streams_for_perf_stats.rb_ | 76 milliseconds
_$ ruby --mjit ./random_streams_for_perf_stats.rb_ | 83 milliseconds
_$ ruby --jit ./random_streams_for_perf_stats.rb_  | 85 milliseconds
_$ ruby --yjit ./random_streams_for_perf_stats.rb_ | _ruby: warning: Ruby was built without YJIT support. You may need to install rustc to build Ruby with YJIT._

<br/>

### MJIT in 2018

The first major JIT success has been MJIT ("MRI JIT" for "Matz's Ruby Interpreter JIT" named after Ruby creator Yukihiro Matsumoto ("Matz"), or in short "MJIT",
and also named "Matz JIT" or "Method-based JIT"), which was introduced with [Ruby 2.6.0 Released](https://www.ruby-lang.org/en/news/2018/12/25/ruby-2-6-0-released/) in 2018:

> The JIT compiler aims to improve the performance of Ruby programs. Unlike traditional JIT compilers which operate in-process, 
> Ruby’s JIT compiler writes out C code to disk and spawns a common C compiler to generate native code.

The MJIT compiler is doing this with existing compilers, see at (*) below.

See also these articles, both from 2018:

- [The method JIT compiler for Ruby 2.6](https://k0kubun.medium.com/the-method-jit-compiler-for-ruby-2-6-388ee0989c13)
- [Ruby’s New JIT](https://developer.squareup.com/blog/rubys-new-jit/), which lists prior attempts of JIT compilation in Ruby

Here's the original branch repository for the MJIT compiler in GitHub (*): [rtl_mjit_branch](https://github.com/vnmakarov/ruby/tree/rtl_mjit_branch#whats-the-branch-about)

The MJIT compiler of the official Ruby distributions is still using Ruby’s **YARV** bytecode: YARV = Yet Another Ruby VM (Virtual Machine), which has been the official Ruby interpreter since Ruby 1.9, introduced in 2007/2008.

YARV has been "designed for an interpreter" (**), which makes designing a JIT compiler for Ruby difficult.

### YJIT in 2021

MJIT has "been less successful at delivering real-world speedups on widely used Ruby applications such as Ruby on Rails.", see at: [YJIT: Building a New JIT Compiler for CRuby](https://shopify.engineering/yjit-just-in-time-compiler-cruby#) from 2021.

However, and as it can be seen at the table above, YJIT is not part of the commonly distributed Ruby version 3.2.3.

**CRuby** is the common implementation of Ruby (same like CPython for Python).

### ZJIT in 2025

In 2025, and from the same team, an early successor of YJIT has been introduced: [ZJIT: Building a Next Generation Ruby JIT](https://rubykaigi.org/2025/presentations/maximecb.html),
to overcome YJIT's deficits in "large-scale production environments".

Same like MJIT and YJIT, also ZJIT is using Ruby’s YARV bytecode as input data, see from here: [ZJIT has been merged into Ruby](https://railsatscale.com/2025-05-14-merge-zjit/),
and same like MJIT, ZJIT is again a conventional "method-based" JIT, while YJIT is based on a new concept called "Lazy Basic Block Versioning" (LBBV), see from here: https://de.slideshare.net/slideshow/zjit-building-a-next-generation-ruby-jit/278807093 (**)

A method-based JIT is "a JIT that optimizes hot code paths using a method as the smallest optimization target", see from here: https://www.heroku.com/blog/ruby-mjit/

By the way: building Ruby from source for the usage with YJIT or ZJIT needs a working Rust compiler: _$ curl https://sh.rustup.rs -sSf | sh; rustc --version_

<br/>

This leaves me this question: what kind of JIT compilation do you get, in version 3.2.3 at least, when you use Ruby's _--jit_ switch?

Ruby's help command says this: _enable JIT for the platform, same as --mjit (experimental)_, with switch _--mjit_ to: _enable C compiler-based JIT compiler (experimental)_

<br/>

So, all in all, it looks like that the JIT compilation landscape of Ruby is still under active development, and that JIT compilation is still not automatically activated when running Ruby source code.

<br/>

## Installation tips

Building the latest version of Ruby from sources ([Quick start guide](https://github.com/ruby/ruby/blob/master/doc/contributing/building_ruby.md#quick-start-guide)) needs an older Ruby installation. Thus, I just installed Ruby with Ubuntu 24 LTS's package management: 

```
$ sudo apt install ruby
...
$ ruby -v
ruby 3.2.3 (2024-01-18 revision 52bb2ac0a6) [x86_64-linux-gnu]
$
```

Since Ruby's execution time is already beating Python's hands down, I refrain from installing latest [Ruby version 4.0.1](https://github.com/ruby/ruby/releases/tag/v4.0.1) on my testing system as of 2026-01-18.

For the same reason, I also refrain from trying Ruby's (official) implementation on the [GraalVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm), called [TruffleRuby](https://github.com/truffleruby/truffleruby) on my testing system.

<br/>

However, on a different Ubuntu 24 LTS system I experimented with installing latest Ruby version 4.0.1 (https://www.ruby-lang.org/en/news/2026/01/13/ruby-4-0-1-released/) as of 2026-01-18, and found out that building it "naked" from sources isn't so easy.

But with the help of the [Ruby Version Manager](https://rvm.io/) (RVM) I succeeded! Think of a working Rust compiler (see above), when configuring Ruby for YJIT or ZJIT: _$ rvm install ruby-4.0.1 --with-configure-flag --enable-zjit_

Running the microbenchmark program with Ruby v.4.0.1 with switches _--yjit_ or _--zjit_ or no JIT didn't improve the execution speed of the microbenchmark program! The opposite is true: running it with Ruby v.4.0.1 tallied a slightly slower execution time in all three variants, which was about +5.5% longer!

<br/>

### mruby to make a standalone Ruby based app

Later I discovered [mruby](https://mruby.org/), a Ruby project officially sponsored by the government of Japan.

TL;DR: it's not a panacea, but it works with the "speed part" of this microbenchmark program (and I'm sure it would work with the whole microbenchmark program), cutting off around 50% from the execution speed.

Actually, mruby is generating a C-based wrapper around mruby's bytecode for its virtual machine inside from my point of view: "mruby can be linked and embedded within your application."

Since this is a longer story, I made a subpage with more details: [mruby for embedding a Ruby application](./TBD)

<br/>

##_end
