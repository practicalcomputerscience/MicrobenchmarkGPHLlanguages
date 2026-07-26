-- random_streams_for_perf_stats.hs
--
-- 2026-07-26
--
-- build on Ubuntu 24 LTS: do this only once:
--                         $ cabal install --lib vector  # install the vector library
--                         $ cabal install --lib random  # install the random library
--
--                         do this after every source code change:
--                         $ ghc random_streams_for_perf_stats.hs -o random_streams_for_perf_stats_devel  # for development
--                         $ ghc -O2 random_streams_for_perf_stats.hs -o random_streams_for_perf_stats  # for production
--
--
-- run on Ubuntu 24 LTS:   $ time random_streams_for_perf_stats => real	0m0.044s
--
--
-- $ ghc --version
-- The Glorious Glasgow Haskell Compilation System, version 9.10.3
-- $
--
--
-- transpiled from random_streams_for_perf_stats3.sml (Standard ML for MLton) with Google AI
-- with manual corrections for structural compliance with the other implementations
-- and Google AI improvements for better execution speed.
-- Of all implementations in functional languages so far, Standard ML is the closest to Haskell according to Google AI:
--     Primary Architectural Advantages: identical type inference (Hindley-Milner, HM),
--                                       matching ADT (algebraic data types) structures, no objects
--     Primary Structural Challenge:     bridging strict vs. lazy evaluation


{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.Random (randomRIO)
import Control.Exception (IOException, catch)
import Data.Word (Word64)
import System.IO (hPutStr, hFlush, stdout, stderr)

-- Requires the 'vector' package:
--   a standard Int array is "boxed," meaning the array stores a list of memory pointers
--   that point to the actual numbers elsewhere on the heap (Google AI)
--   --> Unboxed Vector, which forces Haskell to store the numbers as raw,
--       consecutive binary digits in memory—exactly like a C or SML array:
import qualified Data.Vector.Mutable as BMV         -- Boxed Mutable Vector (for Strings)
import qualified Data.Vector.Unboxed.Mutable as UMV -- Unboxed Mutable Vector (for Integers)


-- *********************************************************
-- user defined functions

integer_to_bin_string :: Int -> String
integer_to_bin_string n = toBinary 16 n ""
  where
    toBinary :: Int -> Int -> String -> String
    toBinary 0 _ acc = acc  -- Force stop exactly at 16 characters
    toBinary count 0 acc = toBinary (count - 1) 0 ('0' : acc)  -- If the number is fully converted, pad left with '0'
    toBinary count k acc =
      let !bit = if k `mod` 2 == 0 then '0' else '1'
          !nextK = k `div` 2
       in toBinary (count - 1) nextK (bit : acc)


integer_to_hex_string :: Int -> String
integer_to_hex_string n = toHex 4 n ""
  where
    toHex :: Int -> Int -> String -> String
    toHex 0 _ acc = acc  -- Force stop exactly at 4 characters
    toHex count 0 acc = toHex (count - 1) 0 ('0' : acc) -- If the number is fully converted, pad left with '0'
    toHex count k acc =
      let !remainder = k `mod` 16
          !nextK     = k `div` 16
          !char      = hexChar remainder
       in toHex (count - 1) nextK (char : acc)

    hexChar :: Int -> Char
    hexChar 0  = '0'
    hexChar 1  = '1'
    hexChar 2  = '2'
    hexChar 3  = '3'
    hexChar 4  = '4'
    hexChar 5  = '5'
    hexChar 6  = '6'
    hexChar 7  = '7'
    hexChar 8  = '8'
    hexChar 9  = '9'
    hexChar 10 = 'a'
    hexChar 11 = 'b'
    hexChar 12 = 'c'
    hexChar 13 = 'd'
    hexChar 14 = 'e'
    hexChar 15 = 'f'
    hexChar _  = '0'


write_to_file :: FilePath -> String -> String -> IO ()
write_to_file filename content fileType = catch action errorHandler
  where
    action :: IO ()
    action = do
      writeFile filename content
      if fileType == "bit"
        then putStr ("\nBit stream has been written to disk under name:  " ++ filename)
        else putStr ("\nByte stream has been written to disk under name: " ++ filename)

    errorHandler :: IOException -> IO ()
    errorHandler _ = putStr ("\ncould not write to file: " ++ filename)


-- Helper functions for each vector type to extract array values into a normal Haskell List
-- MV = mutable vector
vectorToListBoxed :: BMV.IOVector a -> IO [a]
vectorToListBoxed vec = mapM (BMV.read vec) [0 .. BMV.length vec - 1]

vectorToListUnboxed :: (UMV.Unbox a) => UMV.IOVector a -> IO [a]
vectorToListUnboxed vec = mapM (UMV.read vec) [0 .. UMV.length vec - 1]


-- end of user defined functions
-- *********************************************************


-- Constants
upper_limit, m, a, c :: Int
-- END is not working here because any identifier that begins with a capital letter
-- is strictly reserved for Data Constructors or Type Names.

upper_limit = 62499  -- 62499 for exactly 1M binary digits
-- upper_limit = 10  -- for testing
m      = 65521  -- = 2^16 - 15
a      = 17364
c      = 0

file_bits_x, file_bits_hex :: FilePath
file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"


main :: IO ()
main = do
  -- randomRIO (1, m - 1) pulls a cryptographically sound random seed bounded between [1 and (m - 1)]
  start_seed :: Int <- randomRIO (1, m - 1)

  -- Allocate the Unboxed Vector for Ints, and Boxed Vectors for Strings (Haskell IOVectors):
  x        <- UMV.replicate (upper_limit + 1) (0 :: Int)
  bits_x   <- BMV.replicate (upper_limit + 1) "0000000000000000"
  bits_hex <- BMV.replicate (upper_limit + 1) "0000"


  putStr "\ngenerating a random bit stream..."
  -- hFlush stdout  -- Ensure string outputs immediately like SML's print: not needed here

  let masterloop :: Int -> Int -> IO ()
      masterloop _ seed = loop 0 seed
        where
          loop :: Int -> Int -> IO ()
          loop i current_seed = do
            let new_seed = (a * current_seed + c) `mod` m
                bits_x_str   = integer_to_bin_string new_seed
                bits_hex_str = integer_to_hex_string new_seed

            -- Perform array updates in-place
            UMV.write x i new_seed  -- MV = mutable vector
            BMV.write bits_x i bits_x_str
            BMV.write bits_hex i bits_hex_str
            
            -- putStrLn ("\n" ++ show (new_seed))  -- for testing
            -- putStrLn bits_x_str  -- for testing
            -- putStrLn bits_hex_str  -- for testing

            if i < upper_limit
              then loop (i + 1) new_seed  -- Tail recursion
              else return ()

  masterloop 0 start_seed

  -- Convert mutable arrays back into lists to concatenate them
  -- x_list        <- vectorToListUnboxed x  -- for testing
  bits_x_list   <- vectorToListBoxed bits_x
  bits_hex_list <- vectorToListBoxed bits_hex

  let bits_x_str_total    = concat bits_x_list
      bits_hex_str_total  = concat bits_hex_list
      
  -- putStrLn ("\n" ++ bits_x_str_total)  -- for testing
  -- putStrLn bits_hex_str_total  -- for testing

  -- write bit stream to disk
  write_to_file file_bits_x bits_x_str_total "bit"

  -- write byte stream to disk
  write_to_file file_bits_hex bits_hex_str_total "byte"

  putStr "\n"

-- end of random_streams_for_perf_stats.hs
