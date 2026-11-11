{-
graph_4coloring_Germany.hs

2026-11-11

build on Ubuntu 24 LTS: $ ghc graph_4coloring_Germany.hs -o graph_4coloring_Germany_devel  # for development
                        $ ghc -O2 graph_4coloring_Germany.hs -o graph_4coloring_Germany  # for production


run on Ubuntu 24 LTS:   $ time ./graph_4coloring_Germany
                        number N of different solutions = 191808

                                        [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]
                        1st solution  = [Red,Green,Green,Red,Blue,Green,Green,Red,Blue,Red,Green,Red,Blue,Red,Red,Yellow]
                        ...
                        Last solution = [Yellow,Blue,Blue,Yellow,Green,Blue,Blue,Yellow,Green,Yellow,Blue,Yellow,Green,Yellow,Yellow,Red]

                        real	0m0.121s
                        user	0m0.092s
                        sys	0m0.029s
                        $


$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.3
$


transpiled from graph_4coloring_Germany_KiCS2.curry (for the Kiel Curry System KiCS) with Google AI and Duck.ai
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (guard)  -- Control.Monad is part of the base library

data Color = Red | Green | Blue | Yellow deriving (Show, Eq)

aColor :: [Color]
aColor = [Red, Green, Blue, Yellow]

diff :: Color -> Color -> Bool
diff c1 c2 = c1 /= c2

correct :: [[Color]]
correct = do
  sh <- aColor
  hh <- aColor; guard (diff sh hh)
  mv <- aColor; guard (diff sh mv)
  ni <- aColor; guard (diff sh ni && diff hh ni && diff mv ni)
  hb <- aColor; guard (diff ni hb)
  bb <- aColor; guard (diff mv bb && diff ni bb)
  be <- aColor; guard (diff bb be)
  st <- aColor; guard (diff ni st && diff bb st)
  sn <- aColor; guard (diff st sn && diff bb sn)
  nw <- aColor; guard (diff ni nw)
  he <- aColor; guard (diff ni he && diff nw he)
  th <- aColor; guard (diff ni th && diff st th && diff sn th && diff he th)
  rp <- aColor; guard (diff nw rp && diff he rp)
  sl <- aColor; guard (diff rp sl)
  bw <- aColor; guard (diff rp bw && diff he bw)
  by <- aColor; guard (diff sn by && diff he by && diff th by && diff bw by)

  return [sh, mv, hh, hb, ni, st, be, bb, sn, nw, he, th, rp, sl, bw, by]

solution :: [[Color]]
solution = correct

main :: IO ()
main = do
  let sols = solution
  putStrLn ("number N of different solutions = " ++ show (length sols))
  putStrLn ("\n                [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]")
  case sols of
    [] ->
      putStrLn "No solutions found."
    first:rest -> do  -- the first:rest pattern handles the empty-list case explicitly
      let final = foldl (\_ x -> x) first rest
      putStrLn ("1st solution  = " ++ show first)
      putStrLn "..."
      putStrLn ("Last solution = " ++ show final)

-- end of graph_4coloring_Germany.hs
