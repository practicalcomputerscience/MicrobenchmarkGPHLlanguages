# Standard ML (SML)

> ‘ML’ stands for meta language; this is the term logicians use for a language in which other (formal or informal) languages are discussed and analysed.

from: https://direct.mit.edu/books/monograph/2094/The-Definition-of-Standard-ML,  see Book Chapter: Preface 

<br/>

SMLNJ = Standard ML of New Jersey: https://www.smlnj.org/

<br/>

> MLton is a whole-program optimizing compiler for the Standard ML programming language.
- from: http://mlton.org/
- https://github.com/MLton/mlton

---

(TBD)

<br/>

### String building with Standard ML

The speed bottleneck of my initial and slow SML program was not my string handling, but having **a local list of integers** in the masterloop **to which I append one integer number in each iteration**!

I changed this to an (imperative) global array of integers, initially declared in its final size, and the execution time dropped from 12 seconds to 1.6 seconds! Obviously there is generally a speed issue with (very "functional") _lists_ in functional programming languages, see also best practice #2 from here: [My 5 best practices with Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#my-5-best-practices-with-scheme-dialects)

However, in a new functional programming I would always start with ("easy") _lists_ to get the functional aspects like recursions right first and only then try using other (imperative) data types like arrays or vectors, if available, to make a competitively performing program.

<br/>

##_end
