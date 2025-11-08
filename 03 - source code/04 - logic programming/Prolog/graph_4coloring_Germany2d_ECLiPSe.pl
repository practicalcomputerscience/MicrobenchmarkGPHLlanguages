/* graph_4coloring_Germany2d_ECLiPSe.pl

   source: https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/

   test on Version 7.2development #4 (x86_64_linux): OK

   run program in Ubuntu 24 LTS:   $ eclipse -f ./graph_4coloring_Germany2d_ECLiPSe
                                   $ time eclipse -f ./graph_4coloring_Germany2d_ECLiPSe => real	0m0,583s <<<!!


   $ eclipse
   ...
   Version 7.2development #4 (x86_64_linux), Fri Nov  7 10:57 2025
   [eclipse 1]: halt.
   $

*/


:- local initialization(main).  % this local initialization is individual for ECLiPSe

:- use_module(library(lists)).  /* this is different from GNU and SWI Prolog */

color(red).
color(green).
color(blue).
color(yellow).

neighbor(StateAColor, StateBColor) :- color(StateAColor), color(StateBColor), StateAColor \= StateBColor.
/* \= is the negated explicit unification operator */

germany( SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY ) :-
neighbor(SH, NI),
neighbor(SH, HH),
neighbor(SH, MV),
neighbor(HH, NI),
neighbor(MV, NI),
neighbor(MV, BB),
neighbor(NI, HB),
neighbor(NI, BB),
neighbor(NI, ST),
neighbor(NI, TH),
neighbor(NI, HE),
neighbor(NI, NW),
neighbor(ST, BB),
neighbor(ST, SN),
neighbor(ST, TH),
neighbor(BB, BE),
neighbor(BB, SN),
neighbor(NW, HE),
neighbor(NW, RP),
neighbor(SN, TH),
neighbor(SN, BY),
neighbor(RP, SL),
neighbor(RP, HE),
neighbor(RP, BW),
neighbor(HE, BW),
neighbor(HE, TH),
neighbor(HE, BY),
neighbor(TH, BY),
neighbor(BW, BY).


main :- findall( (SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY),
                               germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY),
                               L),
                      length(L, X),
                      write_solutions(L, X),
                      write('\n'),
        halt.  /* exit this program; this is individual for ECLiPSe as halt. without argument */

write_solutions(L, X) :- write('number N of different solutions = '), write(X), nl, write_elements(L, X).

write_elements(L, X)  :- write_header,
                         % nth(1, L, Elem_0),
                         element_at(Elem_0, L, 1),  % custom predicate
                         write('1st solution = '), write(Elem_0), nl, Last is X - 1,
                         % nth(Last, L, Elem_N),
                         element_at(Elem_N, L, Last),  % custom predicate
                         write('...'), nl,
                         write('Last solution = '),
                         write(Elem_N).
                         /* nth() is different from GNU and SWI Prolog */

write_header :- nl, write('               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY'), nl.


% MS Bing AI on prompt "ECLiPSe nth element of list predicate":
% element_at(Element, List, Index)
element_at(X, [X|_], 1).
element_at(X, [_|T], I) :-
    I > 1,
    I1 is I - 1,
    element_at(X, T, I1).


/* output:
$ eclipse -f ./graph_4coloring_Germany2d_ECLiPSe.pl
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red, blue, blue, red, green, blue, green, red, green, red, blue, red, green, red, red, yellow
...
Last solution = yellow, green, green, yellow, blue, green, blue, yellow, blue, yellow, green, yellow, blue, green, yellow, red
$
*/

/* end of graph_4coloring_Germany2d_ECLiPSe.pl */
