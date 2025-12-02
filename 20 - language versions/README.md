# Language versions

A word of warning: when I've tested the last language, the version of the first language probably has already become outdated.

Usually, I've no intention to re-test a language when an update has been published. But I may do so, when I feel the need for further tests or potential improvements. This has already happened at least once.

<br/>

programming language | versions | Bash command to retrieve versions
--- | --- | ---
Ada | alr 2.1.0; gprbuild 22.0.1; gnat_native 13.2.1; gnat_external 13.3.0 | $ alr --version; $ alr toolchain
C | gcc version 13.3.0 (Ubuntu 13.3.0-6ubuntu2~24.04); Homebrew clang version 21.1.4 | $ gcc --version; clang -v  # clang for the "speed part"
C# | Microsoft.AspNetCore.App 8.0.18 ... ; Microsoft.NETCore.App 8.0.18 ...; 8.0.104 ... | $ dotnet --list-runtimes; $ dotnet --list-sdks
C3 | C3 Compiler Version:       0.7.1; ... ; Backends: LLVM; LLVM version: 17.0.6; LLVM default target: x86_64-pc-linux-gnu | $ c3c -V
Chapel | chpl version 2.4.0; built with LLVM version 18.1.8; ... ; Copyright 2020-2025 Hewlett Packard Enterprise Development LP; Copyright 2004-2019 Cray Inc. (See LICENSE file for more details) | $ chpl --version
Clojure | Leiningen 2.10.0 on Java 21.0.7 OpenJDK 64-Bit Server VM | $ lein version
Common Lisp | SBCL 2.2.9.debian | $ sbcl --version
Crystal | Crystal 1.16.3 [3f369d2c7] (2025-05-12); LLVM: 18.1.8; Default target: x86_64-unknown-linux-gnu | $ crystal --version
FreeBASIC | FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for linux-x86_64 (64bit); Copyright (C) 2004-2023 The FreeBASIC development team. | $ fbc --version
Gleam | gleam 1.13.0 | $ gleam -V
Go | go version go1.24.1 linux/amd64 | $ go version
Inko | inko 0.18.1 | $ inko --version
Koka | Koka 3.1.2, 17:56:43 May 30 2024 (ghc release version) | $ koka --version
Kotlin | info: kotlinc-jvm 2.1.21 (JRE 11.0.26+4) | $ $HOME/.sdkman/candidates/kotlin/current/bin/kotlinc -version
Lua | Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio | $ lua -v
LuaJIT | LuaJIT 2.1.1753364724 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/ | $ luajit -v
LunarML | LunarML version 0.2.1 | $ lunarml -v
Mercury | Mercury Compiler, version rotd-2025-11-01; Copyright (C) 1993-2012 The University of Melbourne; Copyright (C) 2013-2025 The Mercury team | $ mmc --version
Mojo | mojo 25.4.0.dev2025050405 (0e8f7772) | $ mojo --version
OCaml | 3.19.0; The OCaml toplevel, version 4.14.1 | $ dune --version; $ ocaml --version
Oz | Mozart Compiler 2.0.1 (Wed, 5 Sep 2018 03:16:51 +0200) playing Oz 3 | $ ozc -v
Perl 5 | This is perl 5, version 38, subversion 2 (v5.38.2) built for x86_64-linux-gnu-thread-multi | $ perl --version
Picat | Picat version 3.9 | picat --version
PowerShell | PowerShell 7.4.5 | $ pwsh -v
Prolog, SWI | SWI-Prolog version 9.3.34 for x86_64-linux | $ swipl --version
Python | Python 3.12.3 | $ python3 --version
Raku | Welcome to Rakudo™ v2025.08.; Implementing the Raku® Programming Language v6.d.; Built on MoarVM version 2025.08. | $ raku -version
Roc | roc nightly pre-release, built from commit c47a8e9 on Sa 22 Mär 2025 09:02:05 UTC | $ roc --version
Rust | rustc 1.88.0 (6b00bc388 2025-06-23); binary: rustc; commit-hash: 6b00bc3880198600130e1cf62b8f8a93494488cc; commit-date: 2025-06-23; host: x86_64-unknown-linux-gnu; release: 1.88.0; LLVM version: 20.1.5 | $ rustc -V -v
Scala | sbt 1.11.0 (Ubuntu Java 21.0.7); Scala code runner version: 1.5.4; Scala version (default): 3.6.4 | $ sbt; $ sbt --version; $ scala --version
Scheme, Bigloo | Bigloo (4.6a) | $ bigloo -version
Scheme, CHICKEN | CHICKEN; (c) 2008-2022, The CHICKEN Team; (c) 2000-2007, Felix L. Winkelmann; Version 5.4.0 (rev 1a1d1495); linux-unix-gnu-x86-64 [ 64bit dload ptables ] | $ csc -version
Scheme, Gambit | v4.9.6 20250310181758 x86_64-pc-linux-gnu "./configure '--enable-single-host'" | $ gsc -v
Scheme, Racket | Welcome to Racket v8.17 [cs]. | $ racket --version
Standard ML | MLton 20241230 | $ mlton
Swift | Swift version 6.1 (swift-6.1-RELEASE); Target: x86_64-unknown-linux-gnu | $ swift --version
V | V 0.4.10 ddfedc7 | $ v version
wren | wren 0.4.0 | $ wren_cli --version
Zig | 0.14.1 | $ zig version

<br/>

other environments | versions | how to retrieve versions
--- | --- | ---
GraalVM | native-image 24 2025-03-18; GraalVM Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01); Substrate VM Oracle GraalVM 24+36.1 (build 24+36, serial gc, compressed references) | $ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image --version
Java | openjdk 21.0.8 2025-07-15; OpenJDK Runtime Environment (build 21.0.8+9-Ubuntu-0ubuntu124.04.1); OpenJDK 64-Bit Server VM (build 21.0.8+9-Ubuntu-0ubuntu124.04.1, mixed mode, sharing) | $ java --version
SDKMAN | SDKMAN!; script: 5.19.0; native: 0.7.4 (linux x86_64) | $ sdk version

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
