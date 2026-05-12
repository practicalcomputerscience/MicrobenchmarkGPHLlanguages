2026-05-12: work in progress

- runtime targets: VM's HashLink (JIT) and NekoVM + bytecode interpretation in HashLink

<br/>

# Haxe

https://haxe.org/ (*)

https://en.wikipedia.org/wiki/Haxe, https://web.archive.org/web/20151208134720/http://ncannasse.fr/blog/haxe_interview (**)

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

Haxe originated (**) in the French MTASC (Motion-Twin ActionScript 2 Compiler), an ActionScript 2.0 compiler, written in the
[OCaml programming language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) (from France!),
and was meant to faster produce applications for the Flash Player than the original Adobe Flash ActionScript compiler.

However, Haxe then evolved to support the **OpenFL** (Open Flash Library for 2D development) (***). So, ActionScript 3.0 code needs transpilation into Haxe code to make it useful for OpenFL: https://github.com/openfl/AS3ConversionGuide/tree/master

Later, Adobe Flash (Professional) evolved into Adobe Animate: https://www.adobe.com/products/animate.html

Today, Haxe claims to transpile to a decent selection of target languages, and that it "allows access to each platform's native capabilities" (*). I won't compile a target list here, which easily may be wrong or incomplete.

However, Haxe notes to have "its own VMs ([HashLink](https://hashlink.haxe.org/) and [NekoVM](https://nekovm.org/))", and the capability to "also run in interpreted mode" (*).

Though, Neko is also a high-level  programming language ([Frequently Asked Questions about Neko](https://nekovm.org/faq/)), see below at [Neko and the Neko virtual machine (NekoVM)](neko-and-the-neko-virtual-machine-nekovm).

While Haxe basically is a statically typed programming language, Neko is dynamically typed, though Haxe also allows for dynamic typing by the coder, see at [Types: Dynamic](https://haxe.org/manual/types-dynamic.html).

Neko, being published as version 1.0 in 2005 (https://nekovm.org/news/) is older than HashLink, which was published as version 1.0 in 2016: https://github.com/HaxeFoundation/hashlink

<br/>

Apparently, the Haxe ecosystem is not the smallest one.

<br/>

## Neko and the Neko virtual machine (NekoVM)

[Neko](https://nekovm.org/) "is a high-level dynamically typed programming language. It can be used as an embedded scripting language. It has been designed to provide a common runtime for several different languages.",
and this by the same author as Haxe and HashLink, Nicolas Cannasse (and team).



















TBD





<br/>

##_end
