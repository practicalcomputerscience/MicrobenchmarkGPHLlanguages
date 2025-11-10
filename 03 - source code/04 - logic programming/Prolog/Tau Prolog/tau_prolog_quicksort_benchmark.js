// tau_prolog_quicksort_benchmark.js
//
// Prolog version of quicksort benchmark: Prolog embedded in JavaScript
//
// 2025-11-08/09
//
// sources:
//   https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/
//   https://www.cs.cmu.edu/Groups/AI/lang/prolog/impl/prolog/aquarius/bench.txt
//   http://tau-prolog.org/manual/compatibility-with-nodejs
//
// test on Ubuntu 24 LTS with tau-prolog@0.3.4: OK!!
//
// run like: $ nodejs ./tau_prolog_quicksort_benchmark.js
//           $ time nodejs ./tau_prolog_quicksort_benchmark.js => real 0m12,344s (on Intel Core i7-11700K 3.6GHz, 32GB DDR4)
//
//
// $ nodejs --version
// v18.19.1
// $ npm list tau-prolog
// ...
// └── tau-prolog@0.3.4
// $

const pl = require('tau-prolog');
const session = pl.create(1000);
const show = x => console.log(session.format_answer(x));

const runs = 9999;
// const runs = 10;  // for testing

// unsorted input list:
const s0 = [27,74,17,33,94,18,46,83,65, 2,
    		    32,53,28,85,99,47,28,82, 6,11,
    		    55,29,39,81,90,37,10, 0,66,51,
    		     7,21,85,27,31,63,75, 4,95,99,
    		    11,28,61,74,18,92,40,53,59, 8];

// Define the quicksort program:
const program = `
    % MS Bing AI solution: this is working principally, but not practically
    % loop_n_times(0, _) :- !.  
    % loop_n_times(N, Goal) :-
    %     N > 0,
    %     call(Goal),
    %     N1 is N - 1,
    %     loop_n_times(N1, Goal).


    % leave the orginal construct untouched:
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
`;    


// Define the goal:
const goal = `
    qsort(S).
    % loop_n_times(1000, qsort(S)). % this is not coming back!
    % loop_n_times(200, qsort(S)).  % this takes 44 sec with: pl.create(10000000);
`;

// const goal_last = `
//     qsort(S).  % OK
// `;


console.log(`${s0}`);  // show unsorted input list like in the other Prolog programs

// do the looping with goal:
// session.consult(program, {
//     success: function() {
//         session.query(goal, {
//             success: function() {
//                session.answers(show);
//             }
//         })
//         // session.query(goal)
//     }
// });
//
// do the looping in JavaScript to get 10,000 iterations done:
for (let i = 0; i < runs; i++) {
  session.consult(program, {
      success: function() {
          session.query(goal, {
              success: function() {
                  // session.answers(show);
                  // no operation here
              }
          })
      }
  });
}

// do one last run to show the sorted result like in the other Prolog programs:
session.consult(program, {
    success: function() {
        // session.query(goal_last, {
        session.query(goal, {
            success: function() {
               session.answers(show);
            }
        })
    }
});


// output:
//   $ time nodejs ./tau_prolog_quicksort_benchmark.js 
//   27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18,92,40,53,59,8
//   S = [0,2,4,6,7,8,10,11,11,17,18,18,21,27,27,28,28,28,29,31,32,33,37,39,40,46,47,51,53,53,55,59,61,63,65,66,74,74,75,81,82,83,85,85,90,92,94,95,99,99]
//   false
//   
//   real	0m12,344s
//   user	0m12,554s
//   sys	0m0,198s
//   $ 

// end of tau_prolog_quicksort_benchmark.js
