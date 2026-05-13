2026-05-12: work in progress

- check: TBD's
- runtime targets: VM's HashLink (JIT): _hl output.hl_ and NekoVM + bytecode interpretation in HashLink: _haxe --main HelloWorld --interp_

- neko <file>               generate Neko bytecode into target file
- hl <file>                 generate HashLink .hl bytecode or .c code into target file: _haxe -hl output.hl -main MyApp _
- jvm <file>                generate JVM bytecode into target file: _haxe --main HelloWorld --jvm HelloWorld.jar_ See from: https://haxe.org/documentation/introduction/language-introduction.html
- js <file>                 generate JavaScript code into target file: _haxe --main HelloWorld --js HelloWorld.js_
- java <directory>          generate Java code into target directory: _???_

<br/>

# Haxe

https://haxe.org/ (*)

https://en.wikipedia.org/wiki/Haxe

https://en.wikipedia.org/wiki/OpenFL, https://www.openfl.org/, https://lib.haxe.org/p/OpenFL/9.5.1/, version 9.5.1 as of 2026-05-12 (***)

https://hashlink.haxe.org/

https://nekovm.org/

<br/>


- FL = Flash Library
- HL = HashLink, a JIT (Just-In-Time) VM
- VM = Virtual Machine

<br/>

---

## Idea of Haxe: from development of Adobe Flash games to cross-platform development for front-end and back-end

Haxe originated in the French MTASC (Motion-Twin ActionScript 2 Compiler), an ActionScript 2.0 compiler, written in the
[OCaml programming language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) (from France!),
and was meant to faster produce applications for the Flash Player than the original Adobe Flash ActionScript compiler:

- [History](https://haxe.org/manual/introduction-haxe-history.html)
- [Haxe Interview](https://web.archive.org/web/20151208134720/http://ncannasse.fr/blog/haxe_interview)

However, Haxe then evolved to support the **OpenFL** (Open Flash Library for 2D development) (***). So, ActionScript 3.0 code needs transpilation into Haxe code to make it useful for OpenFL: https://github.com/openfl/AS3ConversionGuide/tree/master

Later, Adobe Flash (Professional) evolved into Adobe Animate: https://www.adobe.com/products/animate.html

Today, Haxe transpiles to a decent range of target languages, and "allows access to each platform's native capabilities" (*).

#### Lists of compiler targets

Here's a list of currently supported target languages: [What is Haxe?](https://haxe.org/manual/introduction-what-is-haxe.html) (+)

Just running command _$ haxe_ will also provide a list of compiler targets, or having a view at this documentation: [Compiler Targets](https://haxe.org/documentation/introduction/compiler-targets.html)

#### Haxe VM's

Haxe also has "its own VMs ([HashLink](https://hashlink.haxe.org/) and [NekoVM](https://nekovm.org/))", and the capability to "also run in interpreted mode" (*).

"Neko" is also the name of a high-level programming language ([Frequently Asked Questions about Neko](https://nekovm.org/faq/)), see also below at [Neko and the Neko virtual machine (NekoVM) - deprecated!](neko-and-the-neko-virtual-machine-nekovm-deprecated).

#### Type system

While Haxe basically is a statically typed programming language, Neko is dynamically typed, though Haxe also allows for dynamic typing by the developer, see at [Types: Dynamic](https://haxe.org/manual/types-dynamic.html).

Neko, same as Haxe (+), has been published as version 1.0 in 2005 (https://nekovm.org/news/), and therefore is roughly a decade older than HashLink, which has been published as version 1.0 in 2016: https://github.com/HaxeFoundation/hashlink

Web page (+) tells this about main application targets of these two VM's:

- Neko VM: _Desktop, Server, CLI_
- HashLink VM: _Desktop, Mobile, Game consoles_

<br/>

Apparently, the Haxe ecosystem is not the smallest one nowadays.

<br/>

## Installation and compilation tips

I followed to some extent the instructions (for Ubuntu 24) of web page [Linux Software Packages](https://haxe.org/download/linux/):

```
$ sudo apt-get update
...
$ sudo apt-get install haxe
...
$ mkdir ~/haxelib && haxelib setup ~/haxelib
haxelib repository is now ~/haxelib
$ haxe --version
4.3.3
$ neko
NekoVM 2.3.0 (c)2005-2017 Haxe Foundation
  Usage : neko <file>
$ 
```

#### Error messaging

Then I started to develop Haxe source code file [RandomStreamsForPerfStats.hx](./RandomStreamsForPerfStats.hx) piece by piece with the help of the interpreter:

```
$ haxe --main RandomStreamsForPerfStats --interp
RandomStreamsForPerfStats.hx:71: characters 11-12 : Missing ;
$
```

..only to notice that the error messaging of Haxe is not helpful (at all). The error was in this line of source code:

```
io:println('\nbits_x_str_total = $bits_x_str_total');  // for testing
```

..instead of:

```
Sys.println('\nbits_x_str_total = $bits_x_str_total');  // for testing
```

Well, Haxe's compiler is written in OCaml: [Consequence of the Hindley-Milner type inference: uninformative error reporting at compilation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#consequence-of-the-hindley-milner-type-inference-uninformative-error-reporting-at-compilation)

<br/>

## Interpretation of Haxe source code

I would say that the easiest starting point to test Haxe source code is to interpret it.

_$ haxe --help_ says this about interpretation with the _--interp_ compiler switch: _interpret the program using internal macro system_

So, something like this:

```
$ haxe --main RandomStreamsForPerfStats --interp

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

Simmilar to this is to use the _--run_ switch to "interpret a Haxe module with command line arguments":

```
$ haxe --run RandomStreamsForPerfStats
...
$
```

Both commands practically tally the same execution time of about 220 milliseconds.

<br/>

## Neko and the Neko virtual machine (NekoVM) - deprecated!

Bytecode, which is stored into file _RandomStreamsForPerfStats.n_, for the (old) NekoVM can be produced and executed like this:

```
$ haxe --neko RandomStreamsForPerfStats.n --main RandomStreamsForPerfStats
$ neko RandomStreamsForPerfStats.n
...
$
```

Executing the Neko bytecode takes about 900 milliseconds of program execution time!

Then I tested latest NekoVM version 2.4.1 (as of 2026-05-13) from here: https://nekovm.org/download/, only to notice that this NekoVM version needs even more execution time with about 1.6 seconds!

So, I will stick with older version 2.3.0, which also comes with Ubuntu command _$ sudo apt-get install neko_.

<br/>

However, the NekoVM is [Deprecated as of 2021-09-09](https://github.com/HaxeFoundation/neko#deprecated-as-of-2021-09-09)! 

Which leaves me these open questions: what is going to happen to the [Neko Programming Language](https://nekovm.org/), and also the [NekoML high-order functional language with type inference](https://nekovm.org/doc/nekoml/)?

<br/>

## The new HashLink virtual machine (HashLinkVM)

Since my Ubuntu system (already) has "unmet dependencies", I turned to the Homebrew package manager (again) to install the newer HashLink JIT VM:

```
$ brew install hashlink
...
$ hl
HL/JIT 1.15.0 (c)2015-2025 Haxe Foundation
  Usage : hl [--debug <port>] [--debug-wait] <file>
$
```

Bytecode, which is stored into file _RandomStreamsForPerfStats.hl_, for the HashLinkVM can be produced and executed like this:

```
$ haxe --hl RandomStreamsForPerfStats.hl -main RandomStreamsForPerfStats
$ time hl RandomStreamsForPerfStats.hl

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.153s
user	0m0.143s
sys	0m0.011s
$
```

Around 150 milliseconds marks a significantly faster program execution time than using the (deprecated) NekoVM.

TBD

<br/>

##_end
