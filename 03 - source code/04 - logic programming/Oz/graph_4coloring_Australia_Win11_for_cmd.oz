/* graph_4coloring_Australia_Win11_for_cmd.oz

test with Mozart Engine 1.4.0 (20080703) playing Oz 3, 2025-12-04: OK!!

build in Windows 11: > ozc -x graph_4coloring_Australia_Win11_for_cmd.oz
run in Windows 11:   > graph_4coloring_Australia_Win11_for_cmd


Alternatively: compile and execute:
                     > ozc -c graph_4coloring_Australia_Win11_for_cmd.oz
                     > ozengine graph_4coloring_Australia_Win11_for_cmd.ozf

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
           NT
           QL
           NSW
           VIC
           SA
           WA
           TAS
    in
       map(
           % map() is just the name of an Oz record:
           % label(feature1:value1 feature2:value2)
           % from: https://strasheela.sourceforge.net/strasheela/doc/Basics-1.html
           nt  : NT
           ql  : QL
           nsw : NSW
           vic : VIC
           sa  : SA
           wa  : WA
           tas : TAS
          ) = MapColor

       {Record.forAll MapColor proc {$ Country} Country :: 1#4 end}
                              % $ transforms a statement to an expression
                              % which can be used to mimic anonymous procedures;
                              % the :: operator imposes a basic finite domain,
                              % here with a finite set of integers 1..4 (inclusive);
                              % # is a pairing symbol
       WA  \=: NT
       WA  \=: SA
       NT  \=: SA
       NT  \=: QL
       SA  \=: QL
       SA  \=: NSW
       SA  \=: VIC
       QL  \=: NSW
       VIC \=: NSW
       TAS \=: VIC
       {FD.distribute ff MapColor}  % branching and exploration algorithm ff = first-fail ??
                                     % I looked this up from Picat's CP module (for Constraint Programming)
    end

    ColorSolutions = {Search.base.all Color}

    % {Browse ColorSolutions}
    % {System.showInfo "ColorSolutions = "#{Value.toVirtualString ColorSolutions 10000 1000}}
    % ColorSolutions = [map(nsw:1 nt:1 ql:2 sa:3 tas:1 vic:2 wa:2) map(nsw:1 nt:1 ql:2 sa:3 tas:3 vic:2 wa:2) ...
    %                   map(nsw:4 nt:4 ql:3 sa:2 tas:4 vic:3 wa:3)]

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
>ozengine graph_4coloring_Australia_Win11_for_cmd.ozf
number N of different solutions = 576

1st solution  = map(nsw:1 nt:1 ql:2 sa:3 tas:1 vic:2 wa:2)
...
Last solution = map(nsw:4 nt:4 ql:3 sa:2 tas:4 vic:3 wa:3)

>
*/

% end of graph_4coloring_Australia_Win11_for_cmd.oz
