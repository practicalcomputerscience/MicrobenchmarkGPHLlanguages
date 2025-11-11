/* graph_4coloring_Germany2e_XSB.pl

   source: https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/

   test on XSB Version 5.0.0 (Green Tea), 2025-11-11: OK!

   build program in Ubuntu 24 LTS: $ xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl

   run program in Ubuntu 24 LTS:   $ xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl --quietload
                                   $ time xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl --quietload => real	0m0,616s
                                   $ ./exe_times_statistics_for_one_test_case_in_cwd2 "xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl --quietload"



   $ xsb -v
   XSB Version 5.0.0 (Green Tea) of May 15, 2022
   [x86_64-pc-linux-gnu 64 bits; mode: optimal; engine: slg-wam; scheduling: local]
   [Build date: 2025-11-11]
   ...
   $

*/

:- initialization(main).

:- import length/2, nth0/3 from lists.  % this is new for XSB Prolog

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
        halt(0).  /* exit this program */

write_solutions(L, X) :- write('number N of different solutions = '), write(X), nl, write_elements(L, X).

write_elements(L, X)  :- write_header, nth0(0, L, Elem_0), write('1st solution = '), write(Elem_0), nl,
                      Last is X - 1, nth0(Last, L, Elem_N),
                      write('...'), nl,
                      write('Last solution = '), write(Elem_N).

write_header :- nl, write('               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY'), nl.


/* output:
$ xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl
[xsb_configuration loaded]
[sysinitrc loaded]
[xsbbrat loaded]
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
$
*/

/* end of graph_4coloring_Germany2e_XSB.pl */
