/* prolog_system_test.oz

test with Mozart Compiler 2.0.1, playing Oz 3, 2025-12-01/02: OK

run program in Ubuntu 24 LTS: $ ozc -c prolog_system_test.oz     # Compile the source file to a distributable object file (.ozf)
                              $ ozengine prolog_system_test.ozf  # Execute the compiled object file using the ozengine


version: Mozart Compiler 2.0.1 (Wed, 5 Sep 2018 03:16:51 +0200) playing Oz 3

*/

functor
import
  System
  Application

define
    L1 = [1 2]
    L2 = [3 4]

    {System.showInfo "L1 = "#{Value.toVirtualString L1 0 0}}
    % http://mozart2.org/mozart-v1/doc-1.4.0/op/node4.html
    % symbol # is for virtual string concatenation; Depth Width parameters are both 0 here

    {System.showInfo "L2 = "#{Value.toVirtualString L2 0 0}}

    L3 = {Append L1 L2}
    {System.showInfo "L3 = "#{Value.toVirtualString L3 0 0}}

    L4 = {Append nil L1}
    {System.showInfo "L4 = "#{Value.toVirtualString L4 0 0}}

    L5 = [L1|L2]
    {System.showInfo "L5 = "#{Value.toVirtualString L5 0 0}}

    % L6 = []  % this is not working
    % {System.showInfo "L6 = "#{Value.toVirtualString L6 0 0}}


    {Application.exit 0}
end

/*
Picat> append([], L, L).
yes
Picat> append([X | [1,2]], [3,4], [X | N]).
N = [1,2,3,4]
yes
Picat> append([1,2], [3,4], N).
N = [1,2,3,4]
yes
Picat> halt.
*/


/*
output:
$ ozengine prolog_system_test.ozf
L1 = [1 2]
L2 = [3 4]
L3 = [1 2 3 4]
L4 = [1 2]
L5 = [[[1 2] 3 4]]
$
*/

% end of prolog_system_test.oz
