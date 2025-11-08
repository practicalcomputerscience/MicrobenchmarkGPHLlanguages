/* Prolog version of quicksort benchmark */
/* quicksort_benchmark.pl

2025-11-08

sources:
  https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/
  https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/bench.txt
  https://github.com/sasagawa888/nprolog/blob/master/bench/qsort.pl

in YAP Prolog run like this for example: $ yap -L quicksort_benchmark.pl
                                         $ time yap -L quicksort_benchmark.pl

"% new" mark my modifications

*/

:- initialization(main).  % new for automatic program start

main :- range(1,I,9999), qsort(_), fail.
main :- write_s0, nl,  % new: show the original list of integer numbers
        qsort(S), write(S), nl,
        halt(0).       % new: exit this program

range(L,L,H).
range(L,I,H) :- L<H, L1 is L+1, range(L1,I,H).

% new:
write_s0 :- write([27,74,17,33,94,18,46,83,65, 2,
		   32,53,28,85,99,47,28,82, 6,11,
		   55,29,39,81,90,37,10, 0,66,51,
		    7,21,85,27,31,63,75, 4,95,99,
		   11,28,61,74,18,92,40,53,59, 8]).

qsort(S) :- qsort([27,74,17,33,94,18,46,83,65, 2,
		   32,53,28,85,99,47,28,82, 6,11,
		   55,29,39,81,90,37,10, 0,66,51,
		    7,21,85,27,31,63,75, 4,95,99,
		   11,28,61,74,18,92,40,53,59, 8],S,[]).

qsort([X|L],R,R0) :-
	partition(L,X,L1,L2),
	qsort(L2,R1,R0),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

partition([Y|L],X,[Y|L1],L2) :- Y=<X, partition(L,X,L1,L2).
partition([Y|L],X,L1,[Y|L2]) :- Y>X,  partition(L,X,L1,L2).
partition([],_,[],[]).

% end of quicksort_benchmark.pl
