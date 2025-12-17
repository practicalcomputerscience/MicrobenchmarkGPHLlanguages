# Inko

https://inko-lang.org/

https://github.com/inko-lang/inko

---

## Installation tips

Things got much easier with Inko version 0.19.1 compared to the version that I used before, that was version 0.18.1.

I installed Inko version 0.19.1 like this, leaning to chapter _**From source**_ and starting with the latest release tarball: https://docs.inko-lang.org/manual/latest/setup/installation/ (*)

```
$ VER='0.19.1'
$ mkdir $VER
$ curl https://releases.inko-lang.org/$VER.tar.gz -o $VER.tar.gz
$ tar -C $VER -xf $VER.tar.gz
$ cd $VER
$ cargo build --release
$ make
$ sudo make install
$ inko --version
inko 0.19.1
$
```

This is the Rust version I've been using here ("Inko's native code compiler is written in Rust and uses LLVM as its backend." (*)):

```
$ rustc -V
rustc 1.92.0 (ded5c06cf 2025-12-08)
$
```

..and this the LLVM version:

```
$ llvm-config --version
21.1.7
$
```

<br/>

##_end
