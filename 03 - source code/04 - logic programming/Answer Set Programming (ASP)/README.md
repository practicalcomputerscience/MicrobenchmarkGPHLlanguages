2026-06-24: work in progress: TBD

# Answer Set Programming (ASP)

[Prolog](tbd) is nowadays not the only "natural" choice to solve Constraint Satisfaction Problems (CSP's).

<br/>

## Idea of Answer Set Programming

A new approach in Logic Programming was introduced in 1988 with "The Stable Model Semantics for Logic Programming" by Gelfond and Lifschitz: https://www.cs.utexas.edu/~vl/papers/stable.pdf

Since then numerous **answer set solvers** have been developed.

However, these solvers cannot be used for general purpose programming, and thus need interfacing with the outside world. One such combination is [clingo](https://potassco.org/clingo/) and Python: https://pypi.org/project/clingo/

> [!NOTE]
> The "programming" in "Answer Set Programming" means to model a problem as a logic problem. 

<br/>

In this paper [Answer Set Programming: A Primer](https://web.umons.ac.be/app/uploads/sites/84/2026/02/Answer_Set_Programming_A_Primer.pdf)
by Eiter, Ianni, and Krennwallner from 2009, I found good arguments why using ASP over Prolog can make sense (with my emphasises):

> There are however aspects which make the suitability of Prolog (with respect to AnsProlog) less apparent. Among such aspects, there is the fact that many common
> problems require **preference handling** (that is, the possibility to describe which solutions are preferred to others with respect to some “quality” criterion),
> and to properly deal with **incomplete information** (that is, the ability to properly complete missing information with default assumptions,
> or with assumptions of falsity, or with using some notion of undefinedness).

AnsProlog or A-Prolog just means: "Programming in Logic with Answer Sets", so there are no direct implementations of AnsProlog. I think it's an unfortunate term.

<br/>

Körner et al. have written in 2022 ([Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD)) that:

> Answer Set Programming (ASP) is arguably one of the largest successes of logic programming. It is a logic programming paradigm that focuses on solving (hard) search problems,
by reducing them to computing stable models. Note that ASP is not a Turing-complete programming language, but rather a language to represent aforementioned problems. It is based on the stable models semantics and uses answer set solvers to provide truth assignments as models for programs.

<br/>

### clingo playground and ASP efficiency

See from here: https://potassco.org/clingo/run/

Here's the n-queens puzzle in its inefficient form ("encoding") as presented on page 33 at [AN INTRODUCTION TO ANSWER SET PROGRAMMING, Paul Vicol, October 14, 2015](https://www.paulvicol.com/pdfs/ASP-Lecture.pdf) (*). Enter this source code as file _encoding.lp_ and press the "Run" button:

```
row(1..4).
col(1..4).

% Generate
4 { queen(I,J) : row(I), col(J) } 4.

% Test: old encoding on page 33:
:- queen(I,J1), queen(I,J2), J1 != J2.
:- queen(I1,J), queen(I2,J), I1 != I2.
:- queen(I,J), queen(II,JJ), (I,J) != (II,JJ), I+J == II+JJ.
:- queen(I,J), queen(II,JJ), (I,J) != (II,JJ), I-J == II-JJ.

% Test: new encoding on page 51, which is much faster:
%   1 { queen(I,1..n) } 1 :- I = 1..n.
%   1 { queen(1..n,J) } 1 :- J = 1..n.
%   :- 2 { queen(D-J,J) }, D = 2..2*n.
%   :- 2 { queen(D+J,J) }, D = 1-n..n-1.

#show queen/2.
```

n is the size of the board and the number of queens. Replace n with 4, ..., 15, ..., 250, ...

An answer should look like this (as of 2026-06-25):

```
clingo version 6.0.0
Reading from encoding.lp
Solving...
Answer: 1 (Time: 0.002s)
queen(1,3) queen(2,1) queen(3,4) queen(4,2)
SATISFIABLE

Models       : 1+
Calls        : 1
Time         : 0.002s   (Solving: 0.000s 1st Model: 0.000s Unsat: 0.000s)
CPU Time     : N/A
```

Beware that this program will run in your web browser, not on the https://potassco.org/ servers!

From (*):

> [!IMPORTANT]
> • Different problem encodings can yield different solving times
> 
> • Efficiency still depends on how you specify your problem

<br/>

### The clingo-Python pair for solving the map coloring problem of Australia

tbd 

<br/>

### The clingo-Python pair for solving the map coloring problem of Germany

tbd

<br/>

##_end
