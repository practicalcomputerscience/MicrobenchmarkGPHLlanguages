------------------------------------------------------------------------------
--- factorial.curry
---
--- a program for the Curry2Go compiler: https://www.curry-lang.org/curry2go/
---
--- 2026-06-20
---
--- build on Ubuntu 24 LTS: $ curry2goc factorial
--- run on Ubuntu 24 LTS:   $ ./factorial 5
---                         120
---                         $
---
--- with the help of Google AI
---
--- $ $ curry2go --version
--- --------------------------------------------------------------
--- Curry2Go Interactive Environment (Version 1.6.0 of 2025-10-30)
--- --------------------------------------------------------------
---
--- $
---
------------------------------------------------------------------------------

import System.Environment (getArgs)

factorial :: Int -> Int
factorial n | n == 0    = 1
            | n > 0     = n * factorial (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:_) -> print (factorial (read x))
    _     -> putStrLn "Usage: program <n>"

-- end of factorial.curry
