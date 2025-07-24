https://www.roc-lang.org/

---

Roc is the only so called _pure_ functional programming language for implementing this microbenchmark program so far.

While Haskell (https://www.haskell.org/) being the "mother and father" of all modern pure functional programming languages, Roc as a "small" language may have the advantage to really learn _functional programming_ with one "baby step" after the other. A pure functional programming language is forcing one to do so, because there are no imperative constructs like a for-loop for example!

So, even coding a simple (recursive) loop, from scratch and without help of AI, to count from 1 to 10 for example may be the first challenge when coding in a pure functional programming language for the first time.

First I struggled heavily, but then I found a solution:

```
upper_limit = 10
s = List.range({start: At 0, end: Before upper_limit})
    |> List.walk([], \nums, i -> List.append(nums, i))
```

..and a day later another solution:

```
upper_limit = 10u32
counter_start = 0u32

# user defined record for state of the number generation:
State : {
  y : U32,       # prior number
  limit: U32,    # how many numbers still to generate
  x : List (U32) # list of generated integer numbers
}

# user defined, pure function:
int_generate : State -> List (U32)
int_generate = |{y,limit,x}| # |...| = function arguments
    if limit > 1 then
      list0 = List.append(x,y)
      z = y + 1 # calculate next number
      int_generate({y : z,limit : limit-1,x : list0}) # recursion
    else
      List.append(x,y)

# make a list of integer numbers:
s = int_generate({y : counter_start, limit : upper_limit, x : []})
```

##_end
