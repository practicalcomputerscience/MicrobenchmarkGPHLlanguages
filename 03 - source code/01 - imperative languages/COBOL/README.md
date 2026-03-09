2026-03-09: work in progress

Can this very elegant solution be used in other languages, which need an extra user defined function for the int number to hex string conversion?

```
        MOVE HEX-DIGITS(REM-HEX-VAL + 1:1)  *> source: "0123456789abcdef". This is a very elegant solution!!
          TO BITS-HEX-STR(STR-INDEX:1)
```

to-do:

- make record of int random numbers: see from bubble_sort2.cob: WS-NUMBERS

<br/>

# COBOL

COBOL for: "Common Business Oriented Language": 

https://savannah.gnu.org/projects/gnucobol

https://www.iso.org/standard/74527.html

<br/>

At first, I had no intention to implement the microbenchmark program in a language which is supposed to be on the way out. However, with the rise of AI coding:

2026-02-24: [IBM Sinks Most Since 2000 as Anthropic Touts Cobol Tool](https://finance.yahoo.com/news/ibm-sinks-most-since-2000-210436663.html)

..allegedly still widely used COBOL made it into the news again recently, and so I just got curious how this archaic, general purpose programming language (*1960: https://en.wikipedia.org/wiki/COBOL#History_and_specification) would do in this microbenchmark program.

<br/>

## Installation tips

Install and test gnuCOBOL in Ubuntu (24 LTS) like this:

```
$ sudo apt install gnucobol
...
$ cobc -V
cobc (GnuCOBOL) 3.1.2.0
Copyright (C) 2020 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart
Built     Apr 14 2024 07:59:15
Packaged  Dec 23 2020 12:04:58 UTC
C version "13.2.0"
$
```

TBD



<br/>

##_end
