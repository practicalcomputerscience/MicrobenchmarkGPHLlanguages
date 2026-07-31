2026-07-31: work in progress tbd

<br/>

# Miranda

https://www.cs.kent.ac.uk/people/staff/dat/miranda/

<br/>

Miranda was practically the precursor of [Haskell](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell#haskell),
which in return more or less started as an answer to the commercialization of Miranda (see at [A history of Haskell: being lazy with class](https://dl.acm.org/doi/10.1145/1238844.1238856) from 2007):

```
$ mira


                                                     T h e   M i r a n d a   S y s t e m

                                                  version 2.066 last revised 31 January 2020

                                                  Copyright Research Software Ltd 1985-2020

                                                    World Wide Web: http://miranda.org.uk


new file script.m
for help type /h
Miranda /q
miranda logout
$
```

Finally in 2020, Miranda has been open sourced by its inventor [David Turner](https://en.wikipedia.org/wiki/David_Turner_(computer_scientist)):

> Miranda, created by David Turner 🇬🇧 at the University of Kent at Canterbury and released in 1985, was the first language to combine lazy evaluation,
> strong polymorphic type inference, algebraic data types, and pattern matching into a single coherent whole.
> It was the direct intellectual ancestor of Haskell. In 2020, Turner released Miranda 2.0 as open-source software.

from: https://sota.io/blog/deploy-miranda-europe-eu-hosting 

<br/>

However, Miranda programs, or better _scripts_, are strictly meant for interpretation compared to compiled Haskell programs, and thus significantly slower to run.
The Miranda implementation of the "speed part" of the microbenchmark, script [random_streams_for_perf_stats.m](./random_streams_for_perf_stats.m),
takes about 2000 milliseconds to run compared to about 43 milliseconds for the Haskell counterpart!

<br/>

On the other side, Miranda scripts have to stay really _pure_ (until today) compared to the possibilities of "imperative tinkering" for a speedy Haskell program.
Thus, the Miranda script strictly adheres to (pure) functional list building at its implementation of the "masterloop":

```
masterloop :: num -> num -> ([num], [[char]], [[char]])  || return a tuple
masterloop n seed =
  masterloop' [] [] [] n seed  || masterloop' is just the name of this helper function
  where
    masterloop' sofar1 sofar2 sofar3 n current_seed
      || recursion with "guard" (if n > 0)
      || use cons : operator for efficiency:
      = masterloop' (current_seed : sofar1) (bits_x_str : sofar2) (bits_hex_str : sofar3) (n-1) new_seed, if n > 0

      || Reverse both accumulated lists when recursion finishes:
      = (reverse sofar1, reverse sofar2, reverse sofar3), otherwise
        where
          new_seed     = (a * current_seed + c) mod m
          bits_x_str   = integer_to_bin_string new_seed
          bits_hex_str = integer_to_hex_string new_seed
```

<br/>

My impressions after building Miranda script _random_streams_for_perf_stats.m_ (built manually from scratch with big help from Google AI) have been these:

- counterintuitively, I think it's easier in Miranda to make "dirty" (functional) code, or at least "clumsier" functional code, than in Haskell, specifically when using "Big AI" as a coding companion
- this comment is also quite illustrative: "||Any comments welcome -- my Miranda is not as hot as it could be ..." in example script _box.m_. It may be another indicator that finding the "best" or "most efficient" or fastest solution in Miranda isn't so easy
- I learnt the most from functions in the example scripts (_~.m_) located in directory _./miranda/miralib/ex_ from tarball file _mira-2066-src.tgz_, specifically in bigger scripts like _box.m_ (there are not too many of them unfortuantely)

<br/>

## Installation tips

Get latest sources _mira-2066-src.tgz_ for a modern 64-bit Linux system from here: https://www.cs.kent.ac.uk/people/staff/dat/miranda/downloads/

Then unzip this tarball file.

Deviating from the official and old installation instructions given in the __./mira-2066-src/miranda/README_ file, do this now:
change this line in make file _./mira-2066-src/miranda/Makefile_ from:

```
CFLAGS = #-O #-DCYGWIN #-DUWIN #-DIBMRISC #-Dsparc7 #-Dsparc8 
```

..to:

```
CFLAGS = -w -fcommon #-O #-DCYGWIN #-DUWIN #-DIBMRISC #-Dsparc7 #-Dsparc8
```

Then compile the Miranda sources with this command (with providing compilation switches _CFLAGS="-w -fcommon"_ as an extra guard):

```
$ make CFLAGS="-w -fcommon"
...
$
```

Finally, install Miranda (to the usual places) and make a little version test:

```
$ sudo make install
...
$ mira -version
2.066 last revised 31 January 2020
$
```

If the build process goes wrong (because of missing prerequisites for example), just clean up the working directory with command: _$ make cleanup_

<br/>

### Evolution of Miranda

Like every other (serious) general purpose, high-level programming language, also Miranda went through some evolution after its first release.

For example, I noticed that this example from official paper [An Overview of Miranda](https://www.cs.kent.ac.uk/people/staff/dat/miranda/overview.pdf) (PDF)
from late 1986 by its inventor David Turner:

```
primes = sieve [ 2.. ]
         where
         sieve (p:x) = p : sieve [n | n <- x; n mod p > 0]
```

..doesn't work (anymore). But this one (which is also doing some cosmetics for better printing), and it's not the introduction of _main_:

```
main =
  show sieve [2..100]
  ++ "\n"
  where
    sieve [] = []  || the empty list case is needed to avoid the program to stop on an error
    sieve (p:x) = p : sieve [n | n <- x; n mod p ~= 0]
```

Run this script like this:

```
$ mira -exec ./primes.m
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
$
```

<br/>

I also noticed that (specifically) the Miranda interpreter, at least in the version I have used, is very picky with **indentations in the source code**.
Regularly, it has been complaining about the positions of the (important) _where_ clauses. I then accustomed myself to this pattern for a suitable _where_ location:

```
<function name> <arguments> =
  <helper function or other stuff>
  where
    <pattern matching or other stuff>
```

See also from this very original paper: [Miranda: A non-strict functional language with polymorphic types](https://www.cs.kent.ac.uk/people/staff/dat/miranda/nancypaper.pdf) (PDF) by David Turner from 1985:

> There is a nested block structure using **where**, and indentation of inner blocks is compulsory — as in SASL the compiler uses the offside rule to determine the scopes of local definitions.

<br/>

tbd




<br/>

##_end
