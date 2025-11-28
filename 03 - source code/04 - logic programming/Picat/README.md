2025-11-27: work in progress

# Picat: further development of the old B-Prolog

https://www.picat-lang.org/bprolog/

https://picat-lang.org/

> Picat is a .. logic-based multi-paradigm programming language aimed for general-purpose applications.
> Picat is a rule-based language, in which predicates, functions, and actors are defined with pattern-matching rules.

Picat = **P**attern-matching - **I**ntuitive - **C**onstraints - **A**ctors - **T**abling

---

Table of contents:

- [Some concepts of Picat](some-concepts-of-picat)
- [Map coloring problem of Germany](#map-coloring-problem-of-germany)
- [Program execution speed](#program-execution-speed)
- []()
- []()
- []()
- []()

<br/>

---

## Some concepts of Picat

First question first: with its B-Prolog legacy, is Picat still a Prolog system?

Let's make this [test](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#concepts-of-mercury) in the Picat REPL:

```
$ picat
...
Picat> append([], L, L).
yes
Picat> append([X | [1,2]], [3,4], [X | N]).
N = [1,2,3,4]
yes
Picat> append([1,2], [3,4], N).
N = [1,2,3,4]
yes
Picat> halt.
$
```

(empty lines removed)

So, I would say that Picat still is a Prolog system, even when there are syntactical, grammatical and conceptual differences to a "normal" Prolog system. However, KÖRNER P, LEUSCHEL M, BARBOSA J, et al. do not consider Picat to be a Prolog system (*).

<br/>

B-Prolog and [XSB Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#xsb-prolog) have common roots in SB-Prolog (1987), see again at: [Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD) (*), on page 798.

<br/>

Re _Actors_: though I'm not using them here, [Action Rules (AR)](https://en.wikipedia.org/wiki/B-Prolog#Action_rules) have been a new major feature of B-Prolog, "which allow delayable subgoals to be activated later." (*):

> Actors are event-driven calls. Picat provides action rules for describing event-driven behaviors of actors.

from: "A User’s Guide to Picat", Version 3.9, Last updated Auguest 23, 2025, Neng-Fa Zhou and Jonathan Fruhman: https://picat-lang.org/

<br/>

## Map coloring problem of Germany

As a next practical test, I asked ChatGPT to translate the [GNU Prolog version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2a.pl) of this program into Picat, with this [result](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Picat/graph_4coloring_Germany.pi):

```
color(red).
color(green).
color(blue).
color(yellow).

neighbor(A,B) :-
    color(A),
    color(B),
    A != B.

germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY) :-
    neighbor(SH, NI),
    neighbor(SH, HH),
    neighbor(SH, MV),
    ...
    neighbor(HE, BY),
    neighbor(TH, BY),
    neighbor(BW, BY).

main =>
    L = findall((SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY),
            germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY)
        ),
    X = L.len,
    write_solutions(L, X),
    writef("%n"),
    halt.
...
```

So, 

- _\\=_ became _!=_
- _:-_ became _=>_ at _main_
- and other rather small differences.

At least this little Picat source code feels like a dialect of Prolog.

Important here with this already bigger map coloring problem and similar to [GNU Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#gnu-prolog), is to increase memory sizes for program area, global and local stack, and trail stack, which I did like this:

```
$ picat -p 512000000 -s 512000000 -b 512000000 ./graph_4coloring_Germany.pi
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = (red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow)
...
Last solution = (yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red)
$
```

This result is the same as with [GNU Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#gnu-prolog).

<br/>

### Program execution speed

Execution speed is this:

```
...
===> multitime results
1: picat -p 512000000 -s 512000000 -b 512000000 ./graph_4coloring_Germany.pi
            Mean        Std.Dev.    Min         Median      Max
real        0.445       0.006       0.439       0.443       0.464       
user        0.411       0.006       0.402       0.412       0.421       
sys         0.034       0.005       0.022       0.034       0.040       
$ 

```

...with a mean value of only 445 milliseconds from 20 runs, which makes Picat the fastest Prolog system in this microbenchmark: [The TL;DR execution speed diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#the-tldr-execution-speed-diagram).

<br/>

I experimented with potentially smaller memory sizes and run this program with only one eighth of the first sizes:

```
$ multitime -n 20 picat -p 64000000 -s 64000000 -b 64000000 ./graph_4coloring_Germany.pi
...
           Mean        Std.Dev.    Min         Median      Max
real        0.496       0.010       0.484       0.494       0.524       
...
$
```

Mean execution time, again after 20 runs, increased to almost 500 milliseconds.

<br/>


TBD

<br/>

TBD

<br/>

##_end
