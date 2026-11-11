{-
graph_4coloring_Australia_KiCS2.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-20
2026-11-11: refactoring with Duck.ai for better code with no more compilation warnings


build on Ubuntu 24 LTS: $ curry :load graph_4coloring_Australia_KiCS2.curry :save :quit  # building an executable
run on Ubuntu 24 LTS:   $ ./graph_4coloring_Australia_KiCS2
                        number N of different solutions = 576

                                       [NT,QL,NSW,VIC,SA,WA,TAS]
                        1st solution = [Green,Red,Green,Red,Blue,Red,Green]
                        ...
                        Last solution = [Blue,Yellow,Blue,Yellow,Green,Yellow,Blue]
                        $


source: graph_4coloring_Australia_Curry2Go.curry


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

-- correct coloring of the 7 Australian States:
correct
  :: Color -> Color -> Color -> Color -> Color -> Color -> Color -> [Color]
correct nt ql nsw vic sa wa tas =
  if diff wa nt
     && diff wa  sa
     && diff nt  sa
     && diff nt  ql
     && diff sa  ql
     && diff sa  nsw
     && diff sa  vic
     && diff ql  nsw
     && diff vic nsw
     && diff tas vic
  then [nt, ql, nsw, vic, sa, wa, tas]
  else failed

solution :: [Color]
solution = correct aColor aColor aColor aColor aColor aColor aColor

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
  putStrLn ("\n               [NT,QL,NSW,VIC,SA,WA,TAS]")
  case (first, lastSeen) of
    (Just f, Just l) -> do
      putStrLn ("1st solution = " ++ show f)
      putStrLn "..."
      putStrLn ("Last solution = " ++ show l)
    _ ->
      putStrLn "No solutions found."

-- end of graph_4coloring_Australia_KiCS2
