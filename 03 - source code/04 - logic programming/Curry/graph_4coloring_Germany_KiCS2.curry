{-
graph_4coloring_Germany_KiCS2.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-20
2026-11-11: refactoring for a slight exe speed improvement by Duck.ai; but better code with no more compilation warnings


build on Ubuntu 24 LTS: $ curry :load graph_4coloring_Germany_KiCS2.curry :save :quit  # building an executable
run on Ubuntu 24 LTS:   $ time ./graph_4coloring_Germany_KiCS2
                        number N of different solutions = 191808

                                       [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]
                        1st solution = [Red,Blue,Blue,Red,Green,Blue,Green,Red,Green,Red,Blue,Red,Green,Red,Red,Yellow]
                        ...
                        Last solution = [Yellow,Green,Green,Yellow,Blue,Green,Blue,Yellow,Blue,Yellow,Green,Yellow,Blue,Yellow,Yellow,Red]

                        real	0m25.083s
                        ...
                        $


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

aColor :: Color
aColor = Red ? Green ? Blue ? Yellow

diff :: Color -> Color -> Bool
diff x y = x /= y

-- correct coloring of the 16 German States:
correct
  :: Color -> Color -> Color -> Color -> Color -> Color
  -> Color -> Color -> Color -> Color -> Color -> Color
  -> Color -> Color -> Color -> Color -> [Color]
correct sh mv hh hb ni st be bb sn nw he th rp sl bw by =
  if diff sh ni
     && diff sh hh
     && diff sh mv
     && diff hh ni
     && diff mv ni
     && diff mv bb
     && diff ni hb
     && diff ni bb
     && diff ni st
     && diff ni th
     && diff ni he
     && diff ni nw
     && diff st bb
     && diff st sn
     && diff st th
     && diff bb be
     && diff bb sn
     && diff nw he
     && diff nw rp
     && diff sn th
     && diff sn by
     && diff rp sl
     && diff rp he
     && diff rp bw
     && diff he bw
     && diff he th
     && diff he by
     && diff th by
     && diff bw by
  then [sh, mv, hh, hb, ni, st, be, bb, sn, nw, he, th, rp, sl, bw, by]
  else failed

solution :: [Color]
solution =
  correct aColor aColor aColor aColor aColor aColor aColor aColor
          aColor aColor aColor aColor aColor aColor aColor aColor

-- Traverse the result list once, retaining its count and endpoints.
summarize :: [a] -> (Int, Maybe a, Maybe a)
summarize [] = (0, Nothing, Nothing)
summarize (x:xs) = go 1 (Just x) x xs
  where
    go n first lastSeen [] = (n, first, Just lastSeen)
    go n first _ (y:ys) = go (n + 1) first y ys

main :: IO ()
main = do
  tree <- getSearchTree solution
  let sols = allValuesDFS tree
      (n, first, lastSeen) = summarize sols

  putStrLn ("number N of different solutions = " ++ show n)
  putStrLn "\n               [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]"
  case (first, lastSeen) of
    (Just f, Just l) -> do
      putStrLn ("1st solution = " ++ show f)
      putStrLn "..."
      putStrLn ("Last solution = " ++ show l)
    _ ->
      putStrLn "No solutions found."

-- end of graph_4coloring_Germany_KiCS2.curry
