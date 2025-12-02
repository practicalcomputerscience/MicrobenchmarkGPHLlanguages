/* deterministic_logic_programming_test.oz

test with Mozart Compiler 2.0.1, playing Oz 3, 2025-12-02: OK

run program in Ubuntu 24 LTS: $ ozc -c deterministic_logic_programming_test.oz
                              $ ozengine deterministic_logic_programming_test.ozf

source: Logic Programming in Oz with Mozart, Peter Van Roy, 1999: first example


version: Mozart Compiler 2.0.1 (Wed, 5 Sep 2018 03:16:51 +0200) playing Oz 3

*/

functor
import
  System
  Application

define
    % definition of Append predicate:
    % declare -- not to be used here
    proc {Append L1 L2 L3}
        case L1
        of nil then L2=L3
        [] X|M1 then L3=X|{Append M1 L2}
        end
    end

    % {declare A in} -- not to be used here
    A = {Append [1 2 3] [4 5 6]}
    {System.showInfo "with a procedure: A = "#{Value.toVirtualString A 0 0}}

    % {Browse {Append [1 2 3] [4 5 6]} -- becomes:
    {System.showInfo {Value.toVirtualString {Append [1 2 3] [4 5 6]} 0 0}}

    % my extra definition of a function from Big AI:
    fun {AppendF L1 L2}
        % {Append L1 L2}  % joking, but working
        case L1
        of nil then L2  % If first list is empty, return second list
        [] H|T then H | {AppendF T L2}  % Otherwise, keep head and append recursively
        end
    end

    A1 = {AppendF [1 2 3] [4 5 6]}
    {System.showInfo "with a function: A1 = "#{Value.toVirtualString A1 0 0}}

    {Application.exit 0}
end

/*
output:
$ ozengine deterministic_logic_programming_test.ozf
with a procedure: A = [1 2 3 4 5 6]
[1 2 3 4 5 6]
with function: A1 = [1 2 3 4 5 6]
$
*/

% end of deterministic_logic_programming_test.oz
