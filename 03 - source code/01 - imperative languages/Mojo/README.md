https://www.modular.com/mojo

---

#### Error handling when writing to files

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

#### Mojo string builder

The unofficial _mojo-stringbuilder_ solution (https://github.com/maniartech/mojo-stringbuilder) doesn't work anymore in this fast changing language, which is a pitty because such a concept may push the program execution time down towards the execution time of the Rust program for example.

So, this Mojo program is just using plain string concatenation, which isn't so slow in this languages anyway.

##_end
