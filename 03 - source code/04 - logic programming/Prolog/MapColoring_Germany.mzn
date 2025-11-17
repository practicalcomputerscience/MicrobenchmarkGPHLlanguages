% MapColoring_Germany.mzn
%
% 2025-11-17
%
% test on Ubuntu 24 LTS: OK, but not a super-elegant solution;
%                        On the other side: this is a "low-code" program.
%
% run like this: $ minizinc ./MapColoring_Germany.mzn --all-solutions -s  % -s for stats
%                $ minizinc ./MapColoring_Germany.mzn --all-solutions -s -solver gecode  # use the default solver
%                which is: Gecode 6.3.0 (org.gecode.gecode, default solver, cp, int, float, set, restart)
%                see from: $ minizinc --solvers
%
% how to suppress printing individual solutions, and only showing
% the first solution, the last solution and the total number of solutions?
% (like the Prolog programs)
%
% $ time minizinc ./MapColoring_Germany.mzn --all-solutions | grep "^-" | wc
%  191808  191808 2109888
%
% real	0m3,292s
% user	0m4,672s
% sys	0m0,555s
% $
%
%
% $ minizinc --version
% MiniZinc to FlatZinc converter, version 2.9.4, build 2068644735
% Copyright (C) 2014-2025 Monash University, NICTA, Data61
% $

int: nc = 4;

var 1..nc: SH;
var 1..nc: MV;
var 1..nc: HH;
var 1..nc: HB;
var 1..nc: NI;
var 1..nc: ST;
var 1..nc: BE;
var 1..nc: BB;
var 1..nc: SN;
var 1..nc: NW;
var 1..nc: HE;
var 1..nc: TH;
var 1..nc: RP;
var 1..nc: SL;
var 1..nc: BW;
var 1..nc: BY;

% Adjacency constraints
constraint SH != NI;
constraint SH != HH;
constraint SH != MV;
constraint HH != NI;
constraint MV != NI;
constraint MV != BB;
constraint NI != HB;
constraint NI != BB;
constraint NI != ST;
constraint NI != TH;
constraint NI != HE;
constraint NI != NW;
constraint ST != BB;
constraint ST != SN;
constraint ST != TH;
constraint BB != BE;
constraint BB != SN;
constraint NW != HE;
constraint NW != RP;
constraint SN != TH;
constraint SN != BY;
constraint RP != SL;
constraint RP != HE;
constraint RP != BW;
constraint HE != BW;
constraint HE != TH;
constraint HE != BY;
constraint TH != BY;
constraint BW != BY;

solve satisfy;

% output [];  % don't show all outputs here
% commenting any output will show the default output!

% end of MapColoring_Germany.mzn

