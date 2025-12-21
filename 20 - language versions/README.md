# Language versions

> [!WARNING]
> When I've tested the last language, the version of the first language probably has already become outdated.

Usually, I've no intention to re-test a language when an update has been published. But I may do so, when I feel the need for further tests or potential improvements.

This has already happened massively when I corrected a **logical error** in most language implementations in December 2025, that is constraining the initial and random seed value of the pseudo-random number generator to a range of 1 to m - 1, both inclusively, with m being 65521. If not, that is also allowing values 0 and 65521, the ouput of this number generator could be only **zero**.

<br/>

programming language | versions | Bash command to retrieve versions
--- | --- | ---
Ada | alr 2.1.0; gprbuild 25.0.1; gnat_native 15.2.1 | $ alr --version; $ alr toolchain
C | Homebrew clang version 21.1.7 | $ clang -v
C# | Microsoft.AspNetCore.App 8.0.22 ... ; Microsoft.NETCore.App 8.0.22 ...; 8.0.122 ... | $ dotnet --list-runtimes; $ dotnet --list-sdks
C3 | C3 Compiler Version:       0.7.8; ... ; Backends: LLVM; LLVM version: 19.1.7; LLVM default target: x86_64-pc-linux-gnu | $ c3c -V
Chapel | chpl version 2.6.0; built with LLVM version 20.1.8; ... ; Copyright 2020-2025 Hewlett Packard Enterprise Development LP; Copyright 2004-2019 Cray Inc. ... | $ chpl --version
Clojure | Leiningen 2.10.0 on Java 21.0.7 OpenJDK 64-Bit Server VM | $ lein version
Common Lisp | SBCL 2.2.9.debian | $ sbcl --version
Crystal | Crystal 1.18.2 [635ca37a6] (2025-10-21); LLVM: 18.1.8; Default target: x86_64-unknown-linux-gnu | $ crystal --version
FreeBASIC | FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for linux-x86_64 (64bit); Copyright (C) 2004-2023 The FreeBASIC development team. | $ fbc --version
Gleam | gleam 1.13.0 | $ gleam -V
Go | go version go1.25.5 linux/amd64 | $ go version
Inko | inko 0.19.1 | $ inko --version
Koka | Koka 3.2.2, 20:30:51 Jul 22 2025 (ghc release version) | $ koka --version
Kotlin | info: kotlinc-jvm 2.3.0 (JRE 27-ea+2-79) | $ $HOME/.sdkman/candidates/kotlin/current/bin/kotlinc -version
Lua | Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio | $ lua -v
LuaJIT | LuaJIT 2.1.1753364724 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/ | $ luajit -v
LunarML | LunarML version 0.2.1 | $ lunarml -v
Mercury | Mercury Compiler, version rotd-2025-11-01; Copyright (C) 1993-2012 The University of Melbourne; Copyright (C) 2013-2025 The Mercury team | $ mmc --version
Mojo | Mojo 0.26.1.0.dev2025121217 (3e295ef6) | $ pixi run mojo --version
OCaml | 3.19.0; The OCaml toplevel, version 4.14.1 | $ dune --version; $ ocaml --version
Oz | Mozart Compiler 1.4.0 (20080703) playing Oz 3 | >ozc -v  # Windows 11; misusing verbose switch
Perl 5 | This is perl 5, version 38, subversion 2 (v5.38.2) built for x86_64-linux-gnu-thread-multi | $ perl --version
Picat | Picat version 3.9 | $ picat --version
PowerShell | PowerShell 7.5.4 | $ pwsh -v
Prolog, SWI | SWI-Prolog version 9.3.34 for x86_64-linux | $ swipl --version
Python | Python 3.12.3 | $ python3 --version
Raku | Welcome to Rakudo™ v2025.11.; Implementing the Raku® Programming Language v6.d.; Built on MoarVM version 2025.11. | $ raku -version
Roc | roc nightly pre-release, built from commit c47a8e9 on Sa 22 Mär 2025 09:02:05 UTC | $ roc --version
Rust | rustc 1.92.0 (ded5c06cf 2025-12-08); binary: rustc; commit-hash: ded5c06cf21d2b93bffd5d884aa6e96934ee4234; commit-date: 2025-12-08; host: x86_64-unknown-linux-gnu; release: 1.92.0; LLVM version: 21.1.3 | $ rustc -V -v
Scala | sbt runner version: 1.11.7; Scala code runner version: 1.9.1; Scala version (default): 3.7.4 | $ sbt --version; $ scala --version
Scheme, Bigloo | Bigloo (4.6a) | $ bigloo -version
Scheme, CHICKEN | CHICKEN; (c) 2008-2022, The CHICKEN Team; (c) 2000-2007, Felix L. Winkelmann; Version 5.4.0 (rev 1a1d1495); linux-unix-gnu-x86-64 [ 64bit dload ptables ] | $ csc -version
Scheme, Gambit | v4.9.6 20250310181758 x86_64-pc-linux-gnu "./configure '--enable-single-host'" | $ gsc -v
Scheme, Racket | Welcome to Racket v8.17 [cs]. | $ racket --version
Standard ML | MLton 20241230 | $ mlton
Swift | Swift version 6.2.3 (swift-6.2.3-RELEASE); Target: x86_64-unknown-linux-gnu | $ swift --version
V | V 0.4.12 26305ec | $ v version
wren | wren 0.4.0 | $ wren_cli --version
Zig | 0.15.2 | $ zig version

<br/>

other environments | versions | Bash command to retrieve versions
--- | --- | ---
GraalVM | native-image 24 2025-03-18; GraalVM Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01); Substrate VM Oracle GraalVM 24+36.1 (build 24+36, serial gc, compressed references) | $ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image --version
Java | openjdk 27-ea 2026-09-15; OpenJDK Runtime Environment (build 27-ea+2-79); OpenJDK 64-Bit Server VM (build 27-ea+2-79, mixed mode, sharing) | $ java --version
SDKMAN | SDKMAN!; script: 5.20.0; native: 0.7.14 (linux x86_64) | $ sdk version

<br/>

#### On SDKMAN and Kotlin

_$HOME/.sdkman_ refers to a directory created when installing _The Software Development Kit Manager_, in short _SDKMAN_: https://sdkman.io/

Usually, my SDKMAN configuration in my _.bashrc_ file is commented out to not interfere with my usual Java environment (that is the OpenJDK):

```
...
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
```

Actually, I need this environment only when working with Kotlin.

When working with the [GraalVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm), that is building a standalone, binary executable from a so called uberJAR file, I call the related command directly like this:

```
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar <name of your uberJAR file>.jar
```

<br/>

##_end
