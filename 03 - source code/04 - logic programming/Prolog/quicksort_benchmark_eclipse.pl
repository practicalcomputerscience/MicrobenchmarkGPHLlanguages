/* Prolog version of quicksort benchmark */
/* quicksort_benchmark_eclipse.pl

2025-11-08

sources:
  https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/
  https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/bench.txt
  https://github.com/sasagawa888/nprolog/blob/master/bench/qsort.pl

test on Ubuntu 24 LTS with Version 7.1beta #13 (x86_64_linux): OK, but warnings (see below)

run like: $ eclipse -f ./quicksort_benchmark_eclipse.pl
          $ time eclipse -f ./quicksort_benchmark_eclipse.pl => real	0m0,175s (on Intel Core i7-11700K, 3.6GHz, 32GB DDR4)
            <=> Aquarius -- C -- opt.C: 2.8	-- 3.3	-- 1.4 [sec]
                user time with time command on a 25 MHz MIPS processor in 1990:
                25 MHz MIPS processor: Motorola 68020 ?? https://en.wikipedia.org/wiki/Motorola_68020


"% new" mark my modifications

*/

:- local initialization(main).  % new for automatic program start in ECLiPSe, but as local

main :- range(1,I,9999), qsort(_), fail.
main :- write_s0, nl,           % new: show the original list of integer numbers
        qsort(S), write(S), nl,
        halt.                   % new: exit this program, but without argument (halt(0)) in ECLiPSe

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


/* output:
$ time eclipse -f ./quicksort_benchmark_eclipse.pl
File quicksort_benchmark_eclipse.pl, line 26: Singleton variable I
File quicksort_benchmark_eclipse.pl, line 31: Singleton variable H
[27, 74, 17, 33, 94, 18, 46, 83, 65, 2, 32, 53, 28, 85, 99, 47, 28, 82, 6, ...]
[0, 2, 4, 6, 7, 8, 10, 11, 11, 17, 18, 18, 21, 27, 27, 28, 28, 28, 29, ...]

real	0m0,175s
user	0m0,152s
sys	0m0,022s
$
*/

% end of quicksort_benchmark_eclipse.pl
