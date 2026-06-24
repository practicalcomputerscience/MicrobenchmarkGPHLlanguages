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



tbd












##_end
