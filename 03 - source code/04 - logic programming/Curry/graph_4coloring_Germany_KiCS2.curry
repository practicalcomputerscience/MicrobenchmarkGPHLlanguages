{-
graph_4coloring_Germany_KiCS2.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-20

build on Ubuntu 24 LTS: $ curry :load graph_4coloring_Germany_KiCS2.curry :save :quit  # building an executable
run on Ubuntu 24 LTS:   $ time ./graph_4coloring_Germany_KiCS2
                        number N of different solutions = 191808

                                       [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]
                        1st solution = [Red,Blue,Blue,Red,Green,Blue,Green,Red,Green,Red,Blue,Red,Green,Red,Red,Yellow]
                        ...
                        Last solution = [Yellow,Green,Green,Yellow,Blue,Green,Blue,Yellow,Blue,Yellow,Green,Yellow,Blue,Yellow,Yellow,Red]

                        real	0m29.179s <<<<<<<<<<<<<<<
                        ...
                        $


source: graph_4coloring_Australia_KiCS2.curry


$ curry -V
 _  _  ____  ___  ___  ___
( )/ )(_  _)/ __)/ __)(__ \
 )  (  _)(_( (__ \__ \ / _/
(_)\_)(____)\___)(___/(____)

Version 3.5.0-b2 of 2025-12-15 (installed at Mon Dec 15 22:46:06 CET 2025)
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
-- 16 German States:
correct SH MV HH HB NI ST BE BB SN NW HE TH RP SL BW BY =
  cond (diff SH  NI  &&
        diff SH  HH  &&
        diff SH  MV  &&
        diff HH  NI  &&
        diff MV  NI  &&
        diff MV  BB  &&
        diff NI  HB  &&
        diff NI  BB  &&
        diff NI  ST  &&
        diff NI  TH  &&
        diff NI  HE  &&
        diff NI  NW  &&
        diff ST  BB  &&
        diff ST  SN  &&
        diff ST  TH  &&
        diff BB  BE  &&
        diff BB  SN  &&
        diff NW  HE  &&
        diff NW  RP  &&
        diff SN  TH  &&
        diff SN  BY  &&
        diff RP  SL  &&
        diff RP  HE  &&
        diff RP  BW  &&
        diff HE  BW  &&
        diff HE  TH  &&
        diff HE  BY  &&
        diff TH  BY  &&
        diff BW  BY)
       [SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY]


solution = correct aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor aColor

main :: IO ()
main = do
  t <- getSearchTree solution
  let sols = allValuesDFS t
  putStrLn ("number N of different solutions = " ++ show (length sols))
  putStrLn ("\n               [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]")
  putStrLn ("1st solution = " ++ show (head sols))
  putStrLn ("...")
  putStrLn ("Last solution = " ++ show (head (reverse sols)))


-- end of graph_4coloring_Germany_KiCS2.curry
