/* graph_4coloring_Australia.m

2025-11-15

This program is based on the eqneq solver defined in eqneq.m,
which is used in example programs test_eqneq.m + sudoku.m

build and run this program like this:
  Have a project directory named graph_4coloring_Australia with files:
  $ ls -l
  eqneq.m
  graph_4coloring_Australia.m
  Makefile
  Mercury.options
  $
  Then do in this dir:
  $ make
  ...
  $
  Now execute the generated link:
  (it links to actual program: ~/Mercury/asm_fast.gc.tr/x86_64-pc-linux-gnu/Mercury/bin/graph_4coloring_Australia)
  $ ./graph_4coloring_Australia
  NT, QL, NSW, VIC, SA, WA, TAS
  ["red", "green", "red", "green", "blue", "green", "red"]
  bye.
  $

*/


:- module graph_4coloring_Australia.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module eqneq.
:- import_module int, list, string.

:- pragma require_feature_set([trailing]).

%-----------------------------------------------------------------------------%

main(!IO) :-

    ( if solve_map(StateColors) then
        io.print_line("NT, QL, NSW, VIC, SA, WA, TAS", !IO),
        io.print_line(StateColors, !IO)
      else
        io.print_line("No solutions.", !IO)
    ),

    io.write_string("bye.\n", !IO).

%-----------------------------------------------------------------------------%

:- pred solve_map(list(string)::out) is nondet.
solve_map(StateColors) :-
    ( if
        eqneq.n_new(7, Cs),       % 7 State colors
        Cs = [C_NT, C_QL, C_NSW, C_VIC, C_SA, C_WA, C_TAS],

        % Set up the list with the States colors
        StartColors = [C_NT,    % color variable of NT
                       C_QL,    % color variable of QL
                       C_NSW,   % color variable of NSW
                       C_VIC,   % color variable of VIC
                       C_SA,    % color variable of SA
                       C_WA,    % color variable of WA
                       C_TAS],  % color variable of TAS

        % these State colors must be different:
        eqneq.all_different([C_WA, C_NT]),
        eqneq.all_different([C_WA, C_SA]),
        eqneq.all_different([C_NT, C_SA]),
        eqneq.all_different([C_NT, C_QL]),
        eqneq.all_different([C_SA, C_QL]),
        eqneq.all_different([C_SA, C_NSW]),
        eqneq.all_different([C_SA, C_VIC]),
        eqneq.all_different([C_QL, C_NSW]),
        eqneq.all_different([C_VIC, C_NSW]),
        eqneq.all_different([C_TAS, C_VIC])

      then
          % find **one** solution:
          label_map(StartColors, StateColors)
      else
          false
    ).

%-----------------------------------------------------------------------------%
%                                   ia = integer, any
:- pred label_map(list(eqneq(string))::ia, list(string)::out) is nondet.
label_map([], []).
label_map([EqNeqColor | EqNeqColors], [Color | Colors]) :-

    % the State color can be any of these values; type is string here:
    (Color = "red"; Color = "green"; Color = "blue"; Color = "yellow"),

    eqneq.bind(EqNeqColor, Color),

    label_map(EqNeqColors, Colors).

% end of graph_4coloring_Australia.m
