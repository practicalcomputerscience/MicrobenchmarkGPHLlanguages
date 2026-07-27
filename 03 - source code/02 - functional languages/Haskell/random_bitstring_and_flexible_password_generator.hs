{-
random_bitstring_and_flexible_password_generator.hs

2026-07-27

build on Ubuntu 24 LTS: do this only once:
                        $ cabal install --lib vector      # install the vector library
                        $ cabal install --lib random      # install the random library
                        $ cabal install --lib regex-tdfa  # install the random library

                        do this after every source code change:
                        $ ghc random_bitstring_and_flexible_password_generator.hs -o random_bitstring_and_flexible_password_generator_devel  # for development
                        $ ghc -O2 -threaded random_bitstring_and_flexible_password_generator.hs -o random_bitstring_and_flexible_password_generator  # for production


run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.3
$


transpiled from random_bitstring_and_flexible_password_generator.sml (Standard ML for MLton) with Google AI
with manual corrections for structural compliance with the other implementations
and Google AI improvements for better execution speed.
Of all implementations in functional languages so far, Standard ML is the closest to Haskell according to Google AI:
    Primary Architectural Advantages: identical type inference (Hindley-Milner, HM),
                                      matching ADT (algebraic data types) structures, no objects
    Primary Structural Challenge:     bridging strict vs. lazy evaluation
-}

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
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

import Data.Char (isDigit, chr)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))  -- do implicit RegEx compilation


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
-- MV = mutable vector, BMV = Boxed MV, UMV = Unboxed MV
vectorToListBoxed :: BMV.IOVector a -> IO [a]  -- IO for accessing mutable memory, which represents a side effect
vectorToListBoxed vec = mapM (BMV.read vec) [0 .. BMV.length vec - 1]

vectorToListUnboxed :: (UMV.Unbox a) => UMV.IOVector a -> IO [a]
vectorToListUnboxed vec = mapM (UMV.read vec) [0 .. UMV.length vec - 1]


input_a_valid_number :: Int -> IO Int
input_a_valid_number n_char = do
  -- Print the prompt (using putStr without newline, then flushing stdout so it shows up immediately)
  putStr ("\nPassword of " ++ show n_char ++ " printable chars OK? 'y' or another integer number >= 8: ")
  -- show is polymorphic in Haskell
  hFlush stdout  -- really needed here!

  -- SML's TextIO.inputLine leaves '\n' at the end.
  -- Haskell's getLine automatically strips the trailing newline, so we don't need String.substring math.
  answer_str <- getLine

  if answer_str == "y"
    then return n_char
    else if not (null answer_str) && all isDigit answer_str
      then case readMaybe answer_str :: Maybe Int of
        Just n_char_ ->
          if n_char_ >= 8
            then return n_char_
            else do
              putStrLn "enter an integer number >= 8 or 'y'"
              input_a_valid_number n_char
        Nothing -> do
          putStrLn "enter an integer number >= 8 or 'y'"
          input_a_valid_number n_char
      else do
        putStrLn "enter an integer number >= 8 or 'y'"
        input_a_valid_number n_char


answer_yes_or_no :: IO Bool  -- no '->' symbols here because there will be no input arguments!
answer_yes_or_no = do
  putStr ("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  hFlush stdout
  answer_str <- getLine
  return (answer_str == "y")  -- Google AI: "Extra Pro-Tip (Idiomatic Haskell)"


bin_string_to_integer :: String -> Int
bin_string_to_integer binStr = parseBinary binStr 0  -- 0 is the ini value of the recursive loop
  where
    parseBinary :: String -> Int -> Int
    parseBinary [] acc = acc
    parseBinary (c:cs) acc
      | c == '0'  = parseBinary cs (2 * acc)
      | c == '1'  = parseBinary cs (2 * acc + 1)
      | otherwise = 0


-- end of user defined functions
-- *********************************************************


-- Constants
upper_limit, m, a, c :: Int
-- END is not working here because any identifier that begins with a capital letter
-- is strictly reserved for Data Constructors or Type Names.

upper_limit = 62499  -- 62499 for exactly 1M binary digits
-- upper_limit = 25  -- for testing
m      = 65521  -- = 2^16 - 15
a      = 17364
c      = 0

file_bits_x, file_bits_hex :: FilePath
file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"

print_re = "^[!-~]$"
alnum_re = "^[A-Za-z0-9]$"


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
  -- this is not a pure function!
  -- It deals with x, which is a mutable vector and dealing with mutable memory is a side effect!
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

            -- putStrLn ("\n" ++ show new_seed)  -- for testing
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


  -- make a password of n_char printable chars: user input requested here
  let n_char_default = 12
  n_char :: Int <- input_a_valid_number n_char_default  --  <- strips away 'IO' and binds the plain Int
  -- putStrLn (show n_char)  -- for testing

  with_special_chars :: Bool <- answer_yes_or_no
  -- putStrLn (show with_special_chars)  -- for testing

  let pattern = if with_special_chars then print_re else alnum_re


  -- Freeze the mutable vector into an immutable Vector, then convert to a pure list.
  -- This makes live much easier at the following pw_generator function, which is now a pure function!
  frozen_x   <- UV.freeze x
  let x_list =  UV.toList frozen_x  -- x_list is now a pure [Int]

  -- Define the pure function inside main (just like Standard ML)
  let pw_generator :: [Int] -> Int -> String
      pw_generator x_arr n = loop 0 ""
        where
          loop :: Int -> String -> String
          loop j pw_str =  -- j: counter for x_arr
            -- SML 'Array.sub (x, j)' becomes list indexing 'x_arr !! j'
            let bin0 = integer_to_bin_string (x_arr !! j)

                -- Slice the string (String.substring (bin0, start, length))
                -- In Haskell, 'take' and 'drop' are used for slicing lists/strings
                bin0_0 = take 8 bin0
                bin0_1 = take 8 (drop 8 bin0)

                -- Convert pieces back to integers
                char0  = bin_string_to_integer bin0_0
                char1  = bin_string_to_integer bin0_1

                -- Convert numeric values to standalone character strings
                char0a = [chr char0]
                char1a = [chr char1]

                -- Match patterns using the Text.Regex.TDFA (=~) operator
                -- SML: Option.isSome (Regex.find pattern ...)
                -- Haskell: character_string =~ pattern_string :: Bool
                char0_add = if char0a =~ pattern then char0a else ""
                char1_add = if (char1a =~ pattern) && ((length pw_str + 1) < n)
                              then char1a else ""

                -- Combine the new string fragments together
                new_pw_str  = pw_str ++ char0_add ++ char1_add
                new_pw_size = length new_pw_str

            -- Loop check (Recursion or termination)
            in if new_pw_size >= n
                 then new_pw_str
                 else loop (j + 1) new_pw_str

  -- Call the pure generator. Because it is pure, we use 'let' instead of '<-'
  let pw_chars = pw_generator x_list n_char

  putStrLn ("\nYour password of " ++ show n_char ++ " characters is: " ++ pw_chars)

-- end of random_bitstring_and_flexible_password_generator.hs
