# Mojo

https://www.modular.com/mojo

<br/>

> [!WARNING]
> Beware that Mojo is a language still under heavy development!

I had a little and working Mojo program from October 2024 (which tapped into the _buffer_ package: https://docs.modular.com/mojo/stdlib/buffer/), which was no longer working with an updated Mojo version of March 2025!

---

### Error handling when writing to files

The _write_ function of Mojo, when writing to files, works differently as far as I can tell from testing other programming languages so far:

```
    try:
      with open(file_bits_x, "w") as f:
          f.write(bits_x)
      print("Bit stream has been written to disk under name:", file_bits_x)
    except:
      print("could not write to file:", file_bits_x)
```

Even when the access permissions of target file named _file_bits_x_ have been set to "Read-Only", _f.write(bits_x)_ will create a new file with this name. In other words, an exception won't be triggered by this modification.

Instead, set the access permissions of the whole directory to "Access Files". Normally, they are set to "Create and Delete Files". Now, testing Mojo's error handling should work as to be expected.

### Mojo string builder

The unofficial _mojo-stringbuilder_ solution (https://github.com/maniartech/mojo-stringbuilder) doesn't work anymore in this fast changing language, which is a pitty because such a concept may push the program execution time down towards the execution time of the Rust program for example.

So, this Mojo program is just using plain string concatenation, which isn't so slow in this languages anyway.

### Installation tips

Same like [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#installation-tips), the compiled program depends on one shared library (_libKGENCompilerRTShared_) at least, which is provided if Mojo is installed on the target machine. 

##_end
