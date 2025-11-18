/* graph_4coloring_Australia_ALS.pro

   source: https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/

   test on ALS Prolog (Threaded) Version 3.5.0-297-gd921ab03 [linux], 2025-11-18: OK!!

   run in Ubuntu 24 LTS: $ alspro ./graph_4coloring_Australia_ALS.pro -g main. -q
                         $ time alspro ./graph_4coloring_Australia_ALS.pro -g main. -q => real	0m0,034s

   $ alspro
   ALS Prolog (Threaded) Version 3.5.0-297-gd921ab03 [linux]
   ...
   $

   $ alspro --help
   ...
   $

*/


color(red).
color(green).
color(blue).
color(yellow).

neighbor(StateAColor, StateBColor) :- color(StateAColor), color(StateBColor), StateAColor \= StateBColor.
/* \= is the negated explicit unification operator */

australia( NT, QL, NSW, VIC, SA, WA, TAS ) :-
neighbor(WA, NT),
neighbor(WA, SA),
neighbor(NT, SA),
neighbor(NT, QL),
neighbor(SA, QL),
neighbor(SA, NSW),
neighbor(SA, VIC),
neighbor(QL, NSW),
neighbor(VIC, NSW),
neighbor(TAS, VIC).

main :- findall( (NT, QL, NSW, VIC, SA, WA, TAS),
                      australia(NT, QL, NSW, VIC, SA, WA, TAS), L),
                      length(L, X),
                      write_solutions(L, X),
                      nl,
                      halt.

write_solutions(L, X) :- write('number N of different solutions = '), write(X), nl, write_elements(L).

write_elements(L)     :- write_header,
                         % sublist(List,Start,Length,Result): https://alsprolog.com/docs/ref/listutl2.html
                         sublist(L, 0, 1, Elem_0a),
                         last(Elem_0a, Elem_0),
                         write('1st solution = '),
                         write(Elem_0),
                         nl, write('...'), nl,
                         last(L, Last),
                         write('Last solution = '),
                         write(Last).

write_header :- nl, write('               NT, QL, NSW, VIC, SA, WA, TAS'), nl.


/* output:
$ alspro ./graph_4coloring_Australia_ALS.pro -g main. -q
number N of different solutions = 576

               NT, QL, NSW, VIC, SA, WA, TAS
1st solution = green, red, green, red, blue, red, green
...
Last solution = blue, yellow, blue, yellow, green, yellow, blue
$

*/

/* end of graph_4coloring_Australia_ALS.pro */
