# V programming language

https://vlang.io/

<br/>

Another "C for the 21 century" - or another Go not from a big corporation - which compiles to fast code: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

Further benefit: V belongs to a group of languages that don't show [memory leaks](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/15%20-%20memory%20leak%20detection%20with%20Valgrind#memory-leak-detection-with-valgrind) after program exit.

When I started to code my microbenchmark program in V, I was already aware of the controversies surrounding it. Those can easily be searched in the Internet.

---

Individually filling an array needs the << operator since x[i] is not working here:

```
...
mut x := []int{cap: upper_limit}
...
    x << x_now
...
```

from: [source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/V/random_streams_for_perf_stats.v)

<br/>

The standard library (https://modules.vlang.io/) is decent but still needs fixing. So, I had to write my own user defined functions based on code in the V libraries. However, that made the programm a little bit faster because of no need to support generality here.

<br/>

##_end
