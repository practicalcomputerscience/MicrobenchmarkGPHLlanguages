{-
graph_4coloring_Australia_Curry2Go.curry

a program for the Curry2Go compiler: https://www.curry-lang.org/curry2go/

2026-06-20

build on Ubuntu 24 LTS: $ curry2goc graph_4coloring_Australia_Curry2Go
run on Ubuntu 24 LTS:   $ ./graph_4coloring_Australia_Curry2Go
                        <see output below>
                        $ ./graph_4coloring_Australia_Curry2Go | wc -l
                        576  # this number of solutions is correct
                        $


sources:
  https://github.com/curry-language/curry2go/blob/master/examples/Colormap.curry
  https://github.com/curry-language/curry2go/tree/master
  graph_4coloring_Australia_ALS.pro


$ curry2go --version
--------------------------------------------------------------
Curry2Go Interactive Environment (Version 1.6.0 of 2025-10-30)
--------------------------------------------------------------

$
-}


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


main = correct aColor aColor aColor aColor aColor aColor aColor  -- original code: OK

{-
there are a couple of warnings during compilation,
but a working executable is being compiled:

$ ./graph_4coloring_Australia_Curry2Go_Curry2Go
[Red, Yellow, Red, Yellow, Blue, Yellow, Red
[Red, Yellow, Red, Yellow, Blue, Yellow, Red]
[Red, Yellow, Red, Green, Blue, Yellow, Red]
...
[Yellow, Green, Yellow, Green, Blue, Green, Blue]
[Blue, Green, Blue, Green, Yellow, Green, Yellow]
[Blue, Green, Blue, Green, Yellow, Green, Blue]
$
-}

-- end of graph_4coloring_Australia_Curry2Go
