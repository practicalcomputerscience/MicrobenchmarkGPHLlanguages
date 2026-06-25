% graph_4coloring_Australia_clingo.lp
%
% 2026-06-25
%
% source: Google AI
%
% Ubuntu 24 LTS:
% $ sudo apt install gringo
%
% $ clingo --models=0 --quiet=2 ./graph_4coloring_Australia_clingo.lp  # --models=0 means all models
% clingo version 5.6.2
% Reading from ./graph_4coloring_Australia_clingo.lp
% Solving...
% SATISFIABLE
%
% Models       : 576
% Calls        : 1
% Time         : 0.001s (Solving: 0.00s 1st Model: 0.00s Unsat: 0.00s)
% CPU Time     : 0.001s
% $


% ---------- Domain ----------
color(red; green; blue; yellow).

% Define the graph nodes (countries/regions):
region(nt; ql; nsw; vic; sa; wa; tas).

% ---------- Adjacency (undirected) ----------
% (A,B) means A is neighbor of B
edge(wa, nt).
edge(wa, sa).
edge(nt, sa).
edge(nt, ql).
edge(sa, ql).
edge(sa, nsw).
edge(sa, vic).
edge(ql, nsw).
edge(vic, nsw).
edge(tas, vic).

% Make the edges bidirectional:
neighbor(X,Y) :- edge(X,Y).
neighbor(X,Y) :- edge(Y,X).

% ---------- Exactly-one color per region ----------
1 { assign(X,C) : color(C) } 1 :- region(X).

% ---------- Constraint: adjacent regions must differ ----------
:- neighbor(X,Y), assign(X,C), assign(Y,C).

% Display the results cleanly:
#show assign/2.

% end of graph_4coloring_Australia_clingo.lp
