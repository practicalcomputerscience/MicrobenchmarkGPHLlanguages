# Array-oriented languages

Table of contents:

- [APL's nearness to functional programming](#apls-nearness-to-functional-programming)
- [What came after APL](#what-came-after-apl)
- [Speakeasy](#speakeasy)

---

## APL's nearness to functional programming

I would ascribe some so called _array-oriented_ programming languages to a nearness to [functional programming](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#functional-languages).

However, there are array languages which explicitly support the _combination of both functional and imperative styles_: https://www.nial-array-language.org/

Array languages, like the mother and father of all array languages **APL** (https://aplwiki.com/), are used to simulate technical systems ([So, who is mostly using functional programming?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#so-who-is-mostly-using-functional-programming)), here a master piece example from Finland: https://www.dyalog.com/case-studies/simulation.htm, which can read real time input data from external sources (side effects!), here weather data from the Finnish Meteorological Institute.

Though I guess it's fair to say that what later became **Speakeasy** established array-oriented languages latest in 1965:

- https://digital.library.unt.edu/ark:/67531/metadc1030830/
- https://en.wikipedia.org/wiki/Speakeasy_(computational_environment)

## What came after APL

Over the years a myriad of descendants of APL have been created; here's a good open-source overview: https://aplwiki.com/wiki/List_of_open-source_array_languages

I only had a short look into APL's successor language **J** from same creator Kenneth E. Iverson (https://www.jsoftware.com/#/), which only uses basic ASCII characters instead of APL's "glyphs" (APL Font - NARS2000), since I'm not intending to dig into APL glyphs.

However, in the tradition of Lisp these are interpreted languages and not compiled ones. The reason is probably this:

> Some “very high-level languages”, like APL, are normally interpreted because there are many things about the data, such as size and shape of arrays, that cannot be deduced at compile time.

from: https://www.zhb.uni-luebeck.de/epubs/ediss1099.pdf and originally from: Alfred V. Aho, Ravi Sethi, and Jeffrey D. Ullman: "Compilers: Principles, Techniques, and Tools", Addison Wesley, Reading, Massachusetts, USA, 1986 -- that's the first book edition.

Though **APL** never has been a pure functional programming language, yet can be turned into one (https://www.jsoftware.com/papers/eem/functional.htm), I would say that APL and its direct descendants, like J and **K** (https://kx.com/ (*)) are functional programming languages by nature:

> APL is mainly a functional language, although it has also some imperative features.

from: https://encyclopediaofmath.org/wiki/APL

I would not say this about every array-oriented language, like **MATLAB** for example, which clearly has its roots in imperative **Fortran**:

> In the late 1970s, following Wirth’s methodology, I used Fortran and portions of LINPACK and EISPACK to develop the first version of MATLAB. The only data type was “matrix.”

from: https://www.mathworks.com/company/technical-articles/the-origins-of-matlab.html

But APL had an influence on MATLAB:

> So APL, Speakeasy, LINPACK, EISPACK, and PL0 were the predecessors to MATLAB.

from: http://archive.computerhistory.org/resources/access/text/2013/12/102746804-05-01-acc.pdf

APL was still a Top 100 language in July 2025: https://www.tiobe.com/tiobe-index/; now in October 2025 I see **J** and **Q** there.

## Speakeasy

However, I think that Speakeasy's influence on MATLAB was greater than APL's, and if its only for practical reasons of language implementation. And Speakeasy, built on top of Fortran, clearly is an imperative programming language. Or was, since what happened to Speakeasy?

It looks like that this language, just like many others, just fizzled out: https://retrocomputing.stackexchange.com/questions/20444/what-happened-to-the-speakeasy-computational-environment


<br/>

(*) the landscape of the descendants of J, starting with K, looks confusing to me. Apparently, Q was the first descendant of K, and now Q has become q: https://code.kx.com/q4m3/ It seems that terminology work at kx.com is an ongoing thing: _Monadic and dyadic are now respectively unary and binary; verbs are now operators and keywords; and adverbs are iterators._ from the Preface as of 2025-10-22)

<br/>

##_end
