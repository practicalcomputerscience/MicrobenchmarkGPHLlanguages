A work in progress, but not in this sense: when I've tested the last language, the version of the first language probably has already become outdated.

<br/>

programming language | versions | Bash command to retrieve versions
--- | --- | ---
Ada | alr 2.1.0 | $ alr --version
C# | Microsoft.AspNetCore.App 8.0.18 ... ; Microsoft.NETCore.App 8.0.18 ...; 8.0.104 ... | $ dotnet --list-runtimes; $ dotnet --list-sdks
C3 | C3 Compiler Version:       0.7.1; ... ; Backends: LLVM; LLVM version: 17.0.6; LLVM default target: x86_64-pc-linux-gnu | $ c3c -V
Chapel | chpl version 2.4.0; built with LLVM version 18.1.8; ... ; Copyright 2020-2025 Hewlett Packard Enterprise Development LP; Copyright 2004-2019 Cray Inc. (See LICENSE file for more details) | $ chpl --version
Clojure |  | 
Common Lisp | SBCL 2.2.9.debian | $ sbcl --version
Crystal |  | 
FreeBASIC | FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for linux-x86_64 (64bit); Copyright (C) 2004-2023 The FreeBASIC development team. | $ fbc --version
Go | go version go1.24.1 linux/amd64 | $ go version
Inko | inko 0.18.1 | $ inko --version
Kotlin | info: kotlinc-jvm 2.1.21 (JRE 11.0.26+4) | $ $HOME/.sdkman/candidates/kotlin/current/bin/kotlinc -version
Lua | Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio | $ lua -v
Mojo | mojo 25.4.0.dev2025050405 (0e8f7772) | $ mojo --version
OCaml | 3.19.0; The OCaml toplevel, version 4.14.1 | $ dune --version; $ ocaml --version
Perl 5 | This is perl 5, version 38, subversion 2 (v5.38.2) built for x86_64-linux-gnu-thread-multi | $ perl --version
PowerShell |  | 
Python |  | 
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
Zig | 0.14.1 | $ zig version

<br/>

other environments | versions | how to retrieve versions
--- | --- | ---
GraalVM | native-image 24 2025-03-18; GraalVM Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01); Substrate VM Oracle GraalVM 24+36.1 (build 24+36, serial gc, compressed references) | $ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image --version
Java | openjdk 21.0.7 2025-04-15; OpenJDK Runtime Environment (build 21.0.7+6-Ubuntu-0ubuntu124.04); OpenJDK 64-Bit Server VM (build 21.0.7+6-Ubuntu-0ubuntu124.04, mixed mode, sharing) | $ java --version
SDKMAN | SDKMAN!; script: 5.19.0; native: 0.7.4 (linux x86_64) | $ sdk version

<br/>

_$HOME/.sdkman_ refers to a directory created when installing _The Software Development Kit Manager_, in short _SDKMAN_: https://sdkman.io/

Usually, my SDKMAN configuration in my _.bashrc_ file is commented out to not interfere with my usual Java environment (that is the OpenJDK):

```
...
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
```

Actually, I need this environment only when working with Kotlin.

When working with the GraalVM (Link: TBD), that is building a standalone, binary executable with it, I call the related command directly like this:

```
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar <name of your uberJAR file>.jar
```

<br/>

##_end
