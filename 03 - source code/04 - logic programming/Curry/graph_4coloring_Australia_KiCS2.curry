{-
graph_4coloring_Australia_KiCS2.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-20

build on Ubuntu 24 LTS: $ curry :load graph_4coloring_Australia_KiCS2.curry :save :quit  # building an executable
run on Ubuntu 24 LTS:   $ ./graph_4coloring_Australia_KiCS2
                        number N of different solutions = 576

                                       [NT,QL,NSW,VIC,SA,WA,TAS]
                        1st solution = [Green,Red,Green,Red,Blue,Red,Green]
                        ...
                        Last solution = [Blue,Yellow,Blue,Yellow,Green,Yellow,Blue]
                        $


source: graph_4coloring_Australia_Curry2Go.curry


$ curry2go --version
--------------------------------------------------------------
Curry2Go Interactive Environment (Version 1.6.0 of 2025-10-30)
--------------------------------------------------------------

$
-}


import Control.Search.SearchTree  -- getSearchTree function


data Color = Red | Green | Blue | Yellow
  deriving (Show, Eq)

aColor = Red
aColor = Green
aColor = Blue
aColor = Yellow

True  && x = x
False && _ = False

cond True x = x

-- 4 colors => 4 * (4 - 1) = 12 pairs of difference:
diff Red    Green   = True
diff Red    Blue    = True
diff Red    Yellow  = True
diff Green  Red     = True
diff Green  Blue    = True
diff Green  Yellow  = True
diff Blue   Red     = True
diff Blue   Green   = True
diff Blue   Yellow  = True
diff Yellow Red     = True
diff Yellow Green   = True
diff Yellow Blue    = True


-- correct coloring where non-deterministic generators are provided
-- 7 Australian States:
correct NT QL NSW VIC SA WA TAS =
  cond (diff WA  NT  &&
        diff WA  SA  &&
        diff NT  SA  &&
        diff NT  QL  &&
        diff SA  QL  &&
        diff SA  NSW &&
        diff SA  VIC &&
        diff QL  NSW &&
        diff VIC NSW &&
        diff TAS VIC)
       [NT, QL, NSW, VIC, SA, WA, TAS]


-- main = correct aColor aColor aColor aColor aColor aColor aColor  -- original code: OK
--
-- expanded program:
solution = correct aColor aColor aColor aColor aColor aColor aColor

main :: IO ()
main = do
  t <- getSearchTree solution
  let sols = allValuesDFS t
  putStrLn ("number N of different solutions = " ++ show (length sols))
  putStrLn ("\n               [NT,QL,NSW,VIC,SA,WA,TAS]")
  putStrLn ("1st solution = " ++ show (head sols))
  putStrLn ("...")
  putStrLn ("Last solution = " ++ show (head (reverse sols)))


-- end of graph_4coloring_Australia_KiCS2
