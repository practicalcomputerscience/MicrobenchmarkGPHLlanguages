/* graph_4coloring_Germany_Win11_for_cmd.oz

test with Mozart Engine 1.4.0 (20080703) playing Oz 3, 2025-12-04: OK

build in Windows 11: >ozc -x graph_4coloring_Germany_Win11_for_cmd.oz
run in Windows 11:   >graph_4coloring_Germany_Win11_for_cmd

or:                  >ozc -c graph_4coloring_Germany_Win11_for_cmd.oz
                     >ozengine graph_4coloring_Germany_Win11_for_cmd.ozf
                     => same solution than with graph_4coloring_Germany_Win11_for_cmd.exe

In PowerShell (Windows Terminal Version: 1.23.12811.0):
exe time measuring:  > powershell -Command "Measure-Command {.\graph_4coloring_Germany_Win11_for_cmd | Out-Default}"
                     => ...TotalMilliseconds : 304,0598 (best run out of 3)

                     > powershell -Command "Measure-Command {ozengine .\graph_4coloring_Germany_Win11_for_cmd.ozf | Out-Default}"
                     => ...TotalMilliseconds : 285,932 (best run out of 3) <<<<<


environment: Intel Core i5 14600K, Windows 11 Pro 64-bit, ASUS PRIME B760-PLUS

source:
  this solution is based on file: platform-test/fd/color.oz
  https://github.com/mozart/mozart2/blob/83c83da2f670fbd1d08d4145eca3d88f1687582c/platform-test/fd/color.oz

*/

functor
import
  System
  Application
  FD       % Finite Domain functor in lib/main/cp/FD.oz
  % FS       % Finite Integer Set Constraint functor in lib/main/cp/FS.oz
  Search   % Search functor in lib/main/cp/Search.oz

% declare Color

define
    proc {Color ?MapColor}  % ? symbol to denote that MapColor is an output argument
           SH
           MV
           HH
           HB
           NI
           ST
           BE
           BB
           SN
           NW
           HE
           TH
           RP
           SL
           BW
           BY
    in
       map(
           % map() is just the name of an Oz record:
           % label(feature1:value1 feature2:value2)
           % from: https://strasheela.sourceforge.net/strasheela/doc/Basics-1.html
           sh : SH
           mv : MV
           hh : HH
           hb : HB
           ni : NI
           st : ST
           be : BE
           bb : BB
           sn : SN
           nw : NW
           he : HE
           th : TH
           rp : RP
           sl : SL
           bw : BW
           by : BY
          ) = MapColor

       {Record.forAll MapColor proc {$ Country} Country :: 1#4 end}
                              % $ transforms a statement to an expression
                              % which can be used to mimic anonymous procedures;
                              % the :: operator imposes a basic finite domain,
                              % here with a finite set of integers 1..4 (inclusive);
                              % # is a pairing symbol
       SH \=: NI
       SH \=: HH
       SH \=: MV
       HH \=: NI
       MV \=: NI
       MV \=: BB
       NI \=: HB
       NI \=: BB
       NI \=: ST
       NI \=: TH
       NI \=: HE
       NI \=: NW
       ST \=: BB
       ST \=: SN
       ST \=: TH
       BB \=: BE
       BB \=: SN
       NW \=: HE
       NW \=: RP
       SN \=: TH
       SN \=: BY
       RP \=: SL
       RP \=: HE
       RP \=: BW
       HE \=: BW
       HE \=: TH
       HE \=: BY
       TH \=: BY
       BW \=: BY
       {FD.distribute ff MapColor}  % branching and exploration algorithm ff = first-fail ??
                                     % I looked this up from Picat's CP module (for Constraint Programming)
    end

    ColorSolutions = {Search.base.all Color}

    % {Browse ColorSolutions}
    % {System.showInfo "ColorSolutions = "#{Value.toVirtualString ColorSolutions 10000 1000}}

    % {Browse {Length ColorSolutions}}
    {System.showInfo "number N of different solutions = "#{Value.toVirtualString {Length ColorSolutions} 0 0}}

    FirstSolution | _ = ColorSolutions
    {System.showInfo "\n1st solution  = "#{Value.toVirtualString FirstSolution 100 100}}
    {System.showInfo "..."}

    ReverseColorSolutions = {Reverse ColorSolutions}
    LastSolution | _ = ReverseColorSolutions
    {System.showInfo "Last solution = "#{Value.toVirtualString LastSolution 100 100}}


    {Application.exit 0}
end

/*
output:
>graph_4coloring_Germany_Win11_for_cmd
number N of different solutions = 191808

1st solution  = map(bb:1 be:2 bw:1 by:2 hb:1 he:4 hh:2 mv:2 ni:3 nw:1 rp:2 sh:1 sl:1 sn:3 st:2 th:1)
...
Last solution = map(bb:4 be:3 bw:4 by:3 hb:4 he:1 hh:3 mv:3 ni:2 nw:4 rp:3 sh:4 sl:4 sn:2 st:3 th:4)

>
*/

% end of graph_4coloring_Germany_Win11_for_cmd.oz
