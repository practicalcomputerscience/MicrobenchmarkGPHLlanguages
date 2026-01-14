# Crystal

https://crystal-lang.org/

https://github.com/crystal-lang/crystal

<br/>

I call Crystal "Compiled Ruby": this language can be **very fast**: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

Key for success here is using Crystal's _**IO::Memory**_ class (https://crystal-lang.org/api/1.18.2/IO/Memory.html) for fast string building:

```
...
bits_x   = IO::Memory.new  # https://crystal-lang.org/reference/1.16/guides/performance.html
...
  bits_x_str = x[i].to_s(2, precision: 16)
  ...
  bits_x << bits_x_str
...
```

<br/>

##_end
