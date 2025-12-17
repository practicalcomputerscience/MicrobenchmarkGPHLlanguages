# Crystal

https://crystal-lang.org/

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

With execution speed and other features Crystal is clearly - but unspoken of - aiming for low-level systems programming from my point of view. This may seem to be counter-intuitive, when you think of Ruby with it's aim to be the better Perl: 

> I was talking with my colleague about the possibility of an object-oriented scripting language. I knew Perl (Perl4, not Perl5), but I didn’t like it really, because it had the smell of a toy language (it still has).

from: https://www.ruby-lang.org/en/documentation/faq/1/

<br/>

##_end
