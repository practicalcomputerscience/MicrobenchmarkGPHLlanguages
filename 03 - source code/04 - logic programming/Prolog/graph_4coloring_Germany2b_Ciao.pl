% graph_4coloring_Germany2b_Ciao.pl
%
%  source: https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/
%
%  test on Ciao Prolog Ciao 1.25.0 [LINUXx86_64], 2025-11-06: OK!!!
%
%  build program in Ubuntu 24 LTS: $ ciao comp -S ./graph_4coloring_Germany2b_Ciao.pl  # -S for standalone executable
%
%  run program in Ubuntu 24 LTS:   $ ./graph_4coloring_Germany2b_Ciao
%                                  $ time ./graph_4coloring_Germany2b_Ciao => real	0m0,770s <<<!!
%
%
%
%  $ xxx  # version info??
%
%

:- initialization(main).

:- use_module(library(lists)).  % this is different from GNU and SWI Prolog

color(red).
color(green).
color(blue).
color(yellow).

neighbor(StateAColor, StateBColor) :- color(StateAColor), color(StateBColor), StateAColor \= StateBColor.
% \= is the negated explicit unification operator

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
        halt(0).  % exit this program

write_solutions(L, X) :- write('number N of different solutions = '), write(X), nl, write_elements(L, X).

write_elements(L, X)  :- write_header, nth(1, L, Elem_0), write('1st solution = '), write(Elem_0), nl,
                      Last is X - 1, nth(Last, L, Elem_N),
                      write('...'), nl,
                      write('Last solution = '), write(Elem_N).
                      % nth() is different from GNU and SWI Prolog

write_header :- nl, write('               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY'), nl.


% output:
%   $ ./graph_4coloring_Germany2b_Ciao
%   number N of different solutions = 191808
%
%                  SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
%   1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
%   ...
%   Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,green,yellow,red
%                   the last solution is different from GNU and SWI Prolog!!
%   $

% end of graph_4coloring_Germany2b_Ciao.pl
