% graph_4coloring_Germany_clingo.lp
%
% 2026-06-25
%
% source: graph_4coloring_Australia_clingo.lp
%
% Ubuntu 24 LTS:
% $ sudo apt install gringo
%
% $ clingo --models=0 --quiet=2 ./graph_4coloring_Germany_clingo.lp  # --models=0 means all models
% clingo version 5.6.2
% Reading from ./graph_4coloring_Germany_clingo.lp
% Solving...
% SATISFIABLE
% 
% Models       : 191808
% Calls        : 1
% Time         : 0.064s (Solving: 0.06s 1st Model: 0.00s Unsat: 0.00s)
% CPU Time     : 0.064s
% $


% ---------- Domain ----------
color(red; green; blue; yellow).

% Define the graph nodes (countries/regions):
region(sh; mv; hh; hb; ni; st; be; bb; sn; nw; he; th; rp; sl; bw; by).

% ---------- Adjacency (undirected) ----------
% (A,B) means A is neighbor of B
edge(sh, ni).
edge(sh, hh).
edge(sh, mv).
edge(hh, ni).
edge(mv, ni).
edge(mv, bb).
edge(ni, hb).
edge(ni, bb).
edge(ni, st).
edge(ni, th).
edge(ni, he).
edge(ni, nw).
edge(st, bb).
edge(st, sn).
edge(st, th).
edge(bb, be).
edge(bb, sn).
edge(nw, he).
edge(nw, rp).
edge(sn, th).
edge(sn, by).
edge(rp, sl).
edge(rp, he).
edge(rp, bw).
edge(he, bw).
edge(he, th).
edge(he, by).
edge(th, by).
edge(bw, by).

% Make the edges bidirectional:
neighbor(X,Y) :- edge(X,Y).
neighbor(X,Y) :- edge(Y,X).

% ---------- Exactly-one color per region ----------
1 { assign(X,C) : color(C) } 1 :- region(X).

% ---------- Constraint: adjacent regions must differ ----------
:- neighbor(X,Y), assign(X,C), assign(Y,C).

% Display the results cleanly:
#show assign/2.

% end of graph_4coloring_Germany_clingo.lp
