-- random_streams_for_perf_stats.purs
--
-- 2026-11-18
--
-- build on Ubuntu 24 LTS: do this only once:
--                         $ mkdir random_streams_for_perf_stats
--                         $ cd random_streams_for_perf_stats
--                         $ spago init
--                         $ spago test
--                         # this is for bundling the PureScript app into one JavaScript file at ./random_streams_for_perf_stats/index.js:
--                         $ npm install --save-dev esbuild
--                         $ npm approve-scripts esbuild
--                         $ spago install random      -- install if not done yet
--                         $ spago install lists       -- install if not done yet
--                         $ spago install strings     -- install if not done yet
--                         $ spago install exceptions  -- install if not done yet
--                         $ spago install node-fs     -- install if not done yet
--                         add: "type": "module",  to top of config file ./random_streams_for_perf_stats/package.json to
--                              pars this file as a ES module and thus avoid any performance overhead by
--                              not testing it for CommonJS.
--
--                         do this with every source code change:
--                         edit Main.purs (this file) in subdir ./random_streams_for_perf_stats/src/Main.purs
--                         $ PATH="$PWD/node_modules/.bin:$PATH" spago bundle --platform node  # expand PATH to the esbuild executable
--
-- run on Ubuntu 24 LTS:   $ time node ./index.js => real	0m0.147s
--
--
-- $ purs --version
-- 0.15.16
-- $ spago --version
-- 1.0.5
-- $ node --version
-- v24.13.0
-- $
--
--
-- Using Array and Array.snoc for appending with end = 62500 needs 11 sec only for an array of random int numbers!!
-- Using List and List.Cons dramatically reduces it to only 24 milliseconds! (without printing ~62500 numbers)


module Main where


import Prelude

import Effect (Effect)
import Effect.Console (log)  -- log is an effectful function
import Effect.Exception (catchException)

import Effect.Random (randomInt)
-- see for example at: https://github.com/purescript/purescript-lcg/blob/master/src/Random/LCG.purs

import Data.Array as Array  -- Helper functions for working with immutable Javascript arrays.
                            -- https://pursuit.purescript.org/packages/purescript-arrays/7.3.1/docs/Data.Array

import Data.List as List  -- https://github.com/purescript/purescript-lists
                          -- This module is an improvement over Data.Array when working with immutable lists of data
                          -- in a purely-functional setting, but does not have good random-access performance.

import Data.Int (toStringAs, binary, hexadecimal)
-- https://github.com/purescript/purescript-strings/blob/master/src/Data/String/CodePoints.purs

import Data.String as String
import Data.String.CodeUnits as SCU

import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)  -- Google AI


-- Constants
end :: Int  -- explicit type declaration; this can only be done individually
            -- name END is not working: always use small (initial) letters for names of own variables
end = 62500  -- 62500 for exactly 1M binary digits
-- end = 10  -- for testing

m :: Int
m      = 65521  -- = 2^16 - 15
a :: Int
a      = 17364
c :: Int
c      = 0

file_bits_x :: String
file_bits_x   = "random_bitstring.bin"
file_bits_hex :: String
file_bits_hex = "random_bitstring.byte"


-----------------------------------------------------------------------------
-- user defined functions

-- Google AI (~Mercury names):
pad_left :: String -> Int -> String
pad_left s width =
  let
    pad_count = width - String.length s
  in
    if pad_count <= 0 then s
    else SCU.fromCharArray (Array.replicate pad_count '0') <> s

-- end of user defined functions
-----------------------------------------------------------------------------


main :: Effect Unit
main = do  -- do to allow multiple effectful statements inside the function body

  x0 <- randomInt 1 (m - 1)  -- both edges are inclusive; randomInt is an effectful function: use <- for actions in a monad
  -- https://pursuit.purescript.org/packages/purescript-random/6.0.1/docs/Effect.Random#v:randomInt
  -- log ("x0 = " <> show x0)  -- for testing

  log "\ngenerating a random bit stream..."

  let
    masterloop :: Int -> Int -> List.List Int -> List.List String -> List.List String
                  -> { x_ :: Array Int, bits_x_ :: Array String, bits_hex_ :: Array String }
                  -- return a record; it's an inbuilt type
    masterloop n seed acc_nbr_list acc_bits_x_list acc_bits_hex_list =  -- no do here, only =, because this function is pure!
      if n < 1 then
        -- build the return record:
        { x_:         List.toUnfoldable (List.reverse acc_nbr_list)
        , bits_x_:    List.toUnfoldable (List.reverse acc_bits_x_list)
        , bits_hex_:  List.toUnfoldable (List.reverse acc_bits_hex_list)
        }
      else
        let
          new_seed = (a * seed + c) `mod` m
          bits_x_str   = pad_left (toStringAs binary new_seed) 16
          bits_hex_str = pad_left (toStringAs hexadecimal new_seed) 4
        in masterloop
             (n - 1) new_seed
             (List.Cons new_seed acc_nbr_list)
             (List.Cons bits_x_str acc_bits_x_list)
             (List.Cons bits_hex_str acc_bits_hex_list)


  let result_rec = masterloop end x0 List.Nil List.Nil List.Nil
  -- log ("x_ = " <> show result_rec.x_)  -- for testing
  -- log ("bits_x_ = " <> show result_rec.bits_x_)  -- for testing
  -- log ("bits_hex_ = " <> show result_rec.bits_hex_)   -- for testing

  let bits_x   = String.joinWith "" result_rec.bits_x_
  -- exe speed test: fold result_rec.bits_x_ isn't faster than String.joinWith "" result_rec.bits_x_
  
  let bits_hex = String.joinWith "" result_rec.bits_hex_
  -- log ("bits_x = " <> show bits_x)  -- for testing
  -- log ("bits_hex = " <> show bits_hex)   -- for testing

   -- write bit stream to disk
  catchException
    (\err -> log $ "could not write to file: " <> file_bits_x <> " ! -- " <> show err)
      -- \<parameter list> starts an inlined lambda (anonymous function)
      -- <f1> $ <f2> is for chaining function calls: the result of <f2> is applied to <f1>
      -- => reducing parentheses in nested expressions to make code cleaner like this example:
      --    log $ show $ 1 + 2  ==  log (show (1 + 2))
    (do
      writeTextFile UTF8 file_bits_x bits_x
      log $ "Bit stream has been written to disk under name:  " <> file_bits_x)

   -- write byte stream to disk
  catchException
    (\err -> log $ "could not write to file: " <> file_bits_x <> " ! -- " <> show err)
    (do
      writeTextFile UTF8 file_bits_hex bits_hex
      log $ "Byte stream has been written to disk under name: " <> file_bits_hex)

-- end of random_streams_for_perf_stats.purs
