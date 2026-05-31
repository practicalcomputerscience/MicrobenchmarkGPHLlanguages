# Roc

https://www.roc-lang.org/

https://github.com/roc-lang/roc

---

Roc is a so called _pure_ functional programming language.

Wikipedia (https://en.wikipedia.org/wiki/Purely_functional_programming) has a short description about what "pure" means here: _...(treating) all computation as the evaluation of mathematical functions._ 

While **Haskell** (https://www.haskell.org/), being the "mother and father" of all modern **pure** functional programming languages, Roc as a "small" language may have the advantage to really learn _functional programming_ with one "baby step" after the other. A pure functional programming language is forcing you to do so, because there are no imperative constructs like a for-loop for example!

So, even coding a simple (recursive) loop, from scratch and without help of AI, to count from 1 to 10 for example, may be the first challenge, when coding in a pure functional programming language for the first time.

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

<br/>

## Installation tips

I followed these instructions: https://www.roc-lang.org/install/unix

Have a short check that things are working:

```
$ roc version
roc nightly pre-release, built from commit d73ea109 on Tue 09 Sep 2025 09:02:08 AM UTC
$
```

<br/>

#### Regular expressions

2026-05-31: regular expressions are not supported currently in Roc natively. There has been this 3rd party [roc_regex](https://github.com/KilianVounckx/roc_regex) package.

However, I can't compile it anymore due to this error in package file [Regex.roc](https://github.com/KilianVounckx/roc_regex/blob/2f58e376bcfe49b5a15c2363044d486502e564ed/package/Regex.roc#L37):

```
$ roc check random_bitstring_and_flexible_password_generator_slim2.roc
── NOT END OF FILE in roc_regex/Regex.roc ──────────────────────────────────────

I expected to reach the end of the file, but got stuck here:

37│  deriveWord : Regex a, List a -> Regex a | a has Eq
                                             ^...:~/scripts/Roc$ 
$
```

That's the end of my efforts to also use regular expressions in Roc instead of working with (pure) user defined functions _printable_chars_ and _alphanum_chars_, which are based on using numeric codepoints:

```
printable_chars : List (U8) -> List (U8)
printable_chars = |list_u8|
                  List.walk(list_u8, [], |nums, nbr_u8|
                              # https://www.ascii-code.com/ --> dec [33...126] is printable => no space char (like in Scala)
                              if nbr_u8 >= 33 && nbr_u8 <= 126 then
                                List.append(nums, nbr_u8)
                              else
                                nums
                           )

# only alphanumerical chars 0..9, A-Z, a-z = dec [48..57], [65..90], [97..122]
alphanum_chars : List (U8) -> List (U8)  # 2026-05-31
alphanum_chars = |list_u8|
                  List.walk(list_u8, [], |nums, nbr_u8|
                             if nbr_u8 >= 48 && nbr_u8 <= 57 ||
                                nbr_u8 >= 65 && nbr_u8 <= 90 ||
                                nbr_u8 >= 97 && nbr_u8 <= 122 then
                                List.append(nums, nbr_u8)
                             else
                                nums
                           )
```

<br/>

##_end
