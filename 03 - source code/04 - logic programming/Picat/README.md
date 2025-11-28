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
- []()
- []()
- []()
- []()

<br/>

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

So, I would say that Picat still is a Prolog system, even when there are syntactical, grammatical and conceptual differences to a "normal" Prolog system. However, KÖRNER P, LEUSCHEL M, BARBOSA J, et al. do not consider Picat as a Prolog system (*).

<br/>

B-Prolog and [XSB Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#xsb-prolog) have common roots in SB-Prolog (1987), see again at: [Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD) (*), on page 798.

<br/>

Re _Actors_: though I'm not using them here, [Action Rules (AR)](https://en.wikipedia.org/wiki/B-Prolog#Action_rules) have been a new major feature of B-Prolog, "which allow delayable subgoals to be activated later." (*):

> Actors are event-driven calls. Picat provides action rules for describing event-driven behaviors of actors.

from: "A User’s Guide to Picat", Version 3.9, Last updated Auguest 23, 2025, Neng-Fa Zhou and Jonathan Fruhman: https://picat-lang.org/

<br/>

(TBD)

This result is the same as with [GNU Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#gnu-prolog).

<br/>

##_end
