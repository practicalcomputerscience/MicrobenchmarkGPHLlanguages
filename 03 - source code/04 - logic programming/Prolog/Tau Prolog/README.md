2025-11-10: work in progress

# Tau Prolog: Prolog embedded in JavaScript

According to my experience, the best way to start with Tau Prolog is to use its sandbox: http://tau-prolog.org/sandbox/

I noticed that the _:- use_module(library(lists))._ rule isn't needed there, since this library seems to be visible anyway.

Since the complete map coloring problem of Germany, with 16 states and 4 colors, also in this sandbox doesn't come to a final result (within a reasonable amount of time), I developed a shorter version with only the first 8 states (but still with four colors):

![plot](./tau_prolog_graph_4coloring_Germany%20-%20Tau%20sandbox%20OK%2C%20smaller%20number%20of%20states.png)

So, enter the Prolog code into the left box under "Program" and press the "Consult program" button, or after code changes the "Reconsult program" button, that is for code:

```
color(red).
color(green).
color(blue).
color(yellow).

neighbor(StateAColor, StateBColor) :- color(StateAColor), color(StateBColor), StateAColor \= StateBColor.

germany(SH,MV,HH,HB,NI,ST,BE,BB) :-
neighbor(SH, NI),
neighbor(SH, HH),
neighbor(SH, MV),
neighbor(HH, NI),
neighbor(MV, NI),
neighbor(MV, BB),
neighbor(NI, HB),
neighbor(NI, BB),
neighbor(NI, ST),
neighbor(ST, BB),
neighbor(BB, BE).
```

Now check the default query limit in the upper right box. For this problem, I increased it to **100000**. If not, "limit exceeded" can easily be reached!

Then enter the Prolog query, that is also Prolog's goal, into its upper right box - including the full stop character! - like this: _findall((SH,MV,HH,HB,NI,ST,BE,BB),germany(SH,MV,HH,HB,NI,ST,BE,BB),L)._

Then press the [ENTER] key at the query's end to start Prolog's computations. List _L_ hopefully is then being shown, in this example with all valid results.

We can let Prolog count all resulting color combinations with an expanded query, like this for example: _findall((SH,MV,HH,HB,NI,ST,BE,BB),germany(SH,MV,HH,HB,NI,ST,BE,BB),L),length(L,N)._

Or not, since with me, length N of solution list L is not shown - as if it has been forgotten!

Later, I downsized the problem to only three colors and four simple variables A, B, C, D with these rules:

```
color(red).
color(green).
color(blue).

color_map(A, B, C, D) :-
color(A),
color(B),
color(C),
color(D),
A \= B,
A \= C,
B \= D,
C \= D.
```

..and query: _findall((A, B, C, D), color_map(A, B, C, D), L), length(L, N)._ ..and voilà: length _N = 18_ is also provided correctly:

![plot](./tau_prolog_graph_4coloring_Germany%20-%20Tau%20sandbox%20OK%20with%20surrogate%20problem.png)

<br/>

What can already say about Tau Prolog is this from my point of view:

> [!CAUTION]
> At the moment, with version 0.3.4 beta, it's adviced to re-check results and returns provided by Tau Prolog with another, more established Prolog dialect. 


<br/>

However, this is not the way I started with Tau Prolog. Emboldened with my success of TBD

TBD

##_end
