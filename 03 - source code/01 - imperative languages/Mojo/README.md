# Mojo

https://mojolang.org/

<br/>

> [!WARNING]
> Beware that Mojo is a language still under heavy development!

I had a little and working Mojo program from October 2024 (which tapped into the _buffer_ package: https://docs.modular.com/mojo/stdlib/buffer/), which was no longer working with an upgraded Mojo version of March 2025!

2026-05-27: now refactoring for Mojo version >= 1.0.0: let's see how stable things have become...

<br/>

---

#### Error handling when writing to files

With latest version 1.0.0b2, this behavior can no longer be observed by me, or put differently: Mojo v.1.0.0 is now behaving like other programming languages. A code change here was not needed:

The _write_ function of Mojo, when writing to files, works differently as far as I can tell from testing other programming languages so far:

```
    try:
      with open(file_bits_x, "w") as f:
          f.write(bits_x)
      print("Bit stream has been written to disk under name:", file_bits_x)
    except:
      print("could not write to file:", file_bits_x)
```

Even when the access permissions of target file named _file_bits_x_ have been set to "Read-Only", _f.write(bits_x)_ will create a new file with this name. In other words, an exception won't be triggered by this modification. Instead, set the access permissions of the whole directory to "Access Files". Normally, they are set to "Create and Delete Files". Now, testing Mojo's error handling should work as to be expected.

<br/>

## Mojo string builder

The unofficial _mojo-stringbuilder_ solution (https://github.com/maniartech/mojo-stringbuilder) doesn't work anymore in this fast changing language, which is a pitty because such a concept may push the program execution time down towards the execution time of the Rust program for example.

So, this Mojo program is just using plain string concatenation, which isn't so slow in this languages anyway.

<br/>

## Installation tips

Same like [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#installation-tips), the compiled program depends on one shared library (_libKGENCompilerRTShared_) at least, which is provided if Mojo is installed on the target machine. 

<br/>

#### Upgrading the Mojo version

In this still very young programming language, a language upgrade may be advisable from time to time. Do this in the Pixi shell:

```
$ pixi
$ pixi upgrade
...
  ~ C mojo            0.26.3.0.dev2026042305 release  ->  1.0.0b2.dev2026052706 release
  ~ C mojo-compiler   0.26.3.0.dev2026042305 release  ->  1.0.0b2.dev2026052706 release
  ~ C mojo-python     0.26.3.0.dev2026042305 release  ->  1.0.0b2.dev2026052706 release
$ pixi run mojo --version
Mojo 1.0.0b2.dev2026052706 (83444c6d)
$
```

<br/>

#### Regular expressions in Mojo

As of 2026-05-28, it looks like that Mojo (still) hasn't implemented its own regular‑expression engine. So for now, only 3rd party libraries, including [Python's re library](https://docs.python.org/3/library/re.html), can be used, like for example:

- [mojo‑regex](https://github.com/msaelices/mojo-regex), or
- experimental [EmberRegex](https://github.com/bgreni/EmberRegex)

However, and also after the [mojo-stringbuilder](#mojo-string-builder) experience, I've decided against using them, and still stick to this simple string based and proven solution:

```
    var char_set: String = ""
    if WITH_SPECIAL_CHARS == True:
        # add chars dec 33 .. dec 126:
        for i in range(33,127):
          char_set += chr(i)
    else:
        char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
```

<br/>

##_end
