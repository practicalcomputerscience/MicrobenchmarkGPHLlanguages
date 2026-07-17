2026-07-17: work in progress

<br/>

# Free Pascal

https://www.freepascal.org/

including **Objective-Pascal** to access the Mac OS X system framework, which usually happens with Objective-C, and **Object Pascal** support.

Free Pascal is shipped with **a lot of documentation** inside the _doc-pdf.tar.gz_ file (with 8 PDF files)
inside tarball file _fpc-3.2.2.x86_64-linux.tar_ (as of 2026-07-17) from here: https://www.freepascal.org/down/x86_64/linux-hungary.html

Free Pascal supports a lot of platforms, including for example iOS.

The Free Pascal compiler is supporting these modes:

compiler switch | description
--- | ---
-Mfpc            | Free Pascal dialect (default)
-Mobjfpc         | FPC mode with Object Pascal support
-Mdelphi         | Delphi 7 compatibility mode
-Mtp             | TP/BP 7.0 compatibility mode
-Mmacpas         | Macintosh Pascal dialects compatibility mode
-Miso            | ISO 7185 mode
-Mextendedpascal | ISO 10206 mode
-Mdelphiunicode  | Delphi 2009 and later compatibility mode

<br/>

## Installation tips

I just ran the _./install.sh_ script of the (unzipped) tarball file _fpc-3.2.2.x86_64-linux.tar_ as my normal Linux user.
It will ask the user a couple of questions for the desired configuration. I installed Free Pascal into my home directory.

Then I added line _export PATH="$HOME/fpc-3.2.2/bin:$PATH"_ in my _~/.bashrc_ configuration file, which I then activated with: _$ source ~/.bashrc_

A first version test after installation:

```
$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$
```

Free Pascal can also be installed like this in (Ubuntu) Linux: _$ sudo apt install fp-compiler-3.2.2_,
or in its IDE (Integrated development environment) version: _$ sudo apt install fp-ide-3.2.2_

<br/>





<br/>

fpc -O3 

<br/>


<br/>




<br/>


##_end
