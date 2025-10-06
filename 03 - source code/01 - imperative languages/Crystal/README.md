# Crystal

https://crystal-lang.org/

<br/>

I call Crystal "Compiled Ruby": this language can be **very fast**, even beating Go at my microbenchmark and for now: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

Key for success here is using its _**IO::Memory**_ class (https://crystal-lang.org/api/1.17.1/IO/Memory.html) for fast string building:

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

With execution speed and other features Crystal is clearly - but unspoken of - aiming for low-level systems programming from my point of view, which may seem counter-intuitive, when you are thinking of Ruby with it's aim to be the better Perl.

<br/>

##_end
