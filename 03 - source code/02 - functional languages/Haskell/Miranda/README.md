2026-07-31: work in progress tbd

# Miranda

https://www.cs.kent.ac.uk/people/staff/dat/miranda/

Miranda was practically the precursor of [Haskell](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell#haskell),
which in return more or less started as an answer to the commercialization of Miranda (confer [A history of Haskell: being lazy with class](https://dl.acm.org/doi/10.1145/1238844.1238856) from 2007):

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
The Miranda implementation of the "speed part" of the microbenchmark, script [random_streams_for_perf_stats.m](tbd),
takes about 2000 milliseconds to run compared to about 43 milliseconds for the Haskell counterpart.

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

My impressions after building this Miranda script (built manually from scratch with big help from Google AI):

- counterintuitively, I think it's easier in Miranda to make "dirty" (functional) code, or at least "clumsier" functional code, than in Haskell, specifically when using "Big AI" as a coding companion
- this comment is also quite illustrative: "||Any comments welcome -- my Miranda is not as hot as it could be ..." in example script _box.m_
- I learnt the most from functions in the example scripts (_~.m_) in sources directory ./miranda/miralib/ex in tarball file _mira-2066-src.tgz_, specifically from bigger scripts like _box.m_ (there are not too many of them unfortuantely)

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

Then compile the Miranda sources with this command (with _CFLAGS="-w -fcommon"_ provided as an extra guard):

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

If the build process goes wrong (because of missing prerequisites), just clean up the working directory with command: _make cleanup_

<br/>

##_end
