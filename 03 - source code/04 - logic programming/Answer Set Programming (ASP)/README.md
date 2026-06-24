2026-06-24: work in progress: TBD

# Answer Set Programming (ASP)

[Prolog](tbd) is nowadays not the only "natural" choice to solve Constraint Satisfaction Problems (CSP's).

A new approach in Logic Programming was introduced in 1988 with "The Stable Model Semantics for Logic Programming" by Gelfond and Lifschitz: https://www.cs.utexas.edu/~vl/papers/stable.pdf

Since then numerous **answer set solvers** have been developed.

However, these solvers cannot be used for general purpose programming, they take in information which **models a problem**, and thus need interfacing with the outside world. One such combination is [clingo](https://potassco.org/clingo/) and Python: https://pypi.org/project/clingo/.

> [!NOTE]
> The "programming" in "Answer Set Programming" means to model a problem as a logic problem. 

<br/>

In this paper [Answer Set Programming: A Primer](https://web.umons.ac.be/app/uploads/sites/84/2026/02/Answer_Set_Programming_A_Primer.pdf)
by Eiter, Ianni, and Krennwallner from 2009, I found good arguments why using ASP over Prolog can make sense (with my emphasises):

> There are however aspects which make the suitability of Prolog (with respect to AnsProlog) less apparent. Among such aspects, there is the fact that many common
> problems require **preference handling** (that is, the possibility to describe which solutions are preferred to others with respect to some “quality” criterion),
> and to properly deal with **incomplete information** (that is, the ability to properly complete missing information with default assumptions,
> or with assumptions of falsity, or with using some notion of undefinedness).

AnsProlog or A-Prolog just means: "Programming in Logic with Answer Sets", so there are no direct implementations of AnsProlog.

<br/>

Körner et al. have written in 2022 ([Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD)) that:

> Answer Set Programming (ASP) is arguably one of the largest successes of logic programming. It is a logic programming paradigm that focuses on solving (hard) search problems,
by reducing them to computing stable models. Note that ASP is not a Turing-complete programming language, but rather a language to represent aforementioned problems. It is based on the stable models semantics and uses answer set solvers to provide truth assignments as models for programs.





tbd

<br/>

##_end
