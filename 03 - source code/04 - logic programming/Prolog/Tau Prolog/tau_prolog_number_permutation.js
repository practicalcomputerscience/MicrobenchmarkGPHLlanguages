// tau_prolog_number_permutation.js
//
// Prolog embedded in JavaScript
//
// 2025-11-09
//
// source: https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/
//
// test on Ubuntu 24 LTS with tau-prolog@0.3.4: OK!!
//
// run like: $ nodejs ./tau_prolog_number_permutation.js 
//
//
// $ nodejs --version
// v18.19.1
// $ npm list tau-prolog
// ...
// └── tau-prolog@0.3.4
// $

const pl = require('tau-prolog');
const session = pl.create(100000);
const show = x => console.log(session.format_answer(x));

// Define the Prolog program:
const program = `
    % Duck.ai on prompt: "Tau Prolog all permuations of numbers 1, 2, 3, 4"
    %
    % Base case: the permutation of an empty list is an empty list
    permute([], []).
    
    % Recursive case: permute a list by fixing each element and permuting the rest
    permute([H|T], Permutation) :-
        permute(T, RestPermutation),
        insert(H, RestPermutation, Permutation).
    
    % Insert an element into all possible positions in a list
    insert(X, L, [X|L]).  % Insert X at the head
    insert(X, [Y|Rest], [Y|L]) :-
        insert(X, Rest, L).  % Insert X into the tail
`;


// Define the Prolog goal: this is the query
const goal = `
    permute([1, 2, 3, 4], Permutation).
`;

// console.log(`${s0}`);  // show inputs before executing the Prolog program

session.consult(program, {
    success: function() {
        session.query(goal, {
            success: function() {
               session.answers(show);
            }
        })
    }
});


// output:
//   $ nodejs ./tau_prolog_number_permutation.js 
//   Permutation = [1,2,3,4]
//   Permutation = [2,1,3,4]
//   Permutation = [2,3,1,4]
//   Permutation = [2,3,4,1]
//   Permutation = [1,3,2,4]
//   Permutation = [3,1,2,4]
//   Permutation = [3,2,1,4]
//   Permutation = [3,2,4,1]
//   Permutation = [1,3,4,2]
//   Permutation = [3,1,4,2]
//   Permutation = [3,4,1,2]
//   Permutation = [3,4,2,1]
//   Permutation = [1,2,4,3]
//   Permutation = [2,1,4,3]
//   Permutation = [2,4,1,3]
//   Permutation = [2,4,3,1]
//   Permutation = [1,4,2,3]
//   Permutation = [4,1,2,3]
//   Permutation = [4,2,1,3]
//   Permutation = [4,2,3,1]
//   Permutation = [1,4,3,2]
//   Permutation = [4,1,3,2]
//   Permutation = [4,3,1,2]
//   Permutation = [4,3,2,1]
//   false
//   $ 

// end of tau_prolog_number_permutation.js
