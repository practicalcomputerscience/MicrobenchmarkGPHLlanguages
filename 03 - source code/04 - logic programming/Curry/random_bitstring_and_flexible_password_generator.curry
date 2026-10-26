{-
random_bitstring_and_flexible_password_generator.curry
is actually file ./random_bitstring_and_flexible_password_generator/src/Main.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-21/22
2026-06-29: ++ show fileBitsX --> ++ fileBitsX to not show "..." around file names anymore
2026-07-09: fixed initial random seed to [1..m_[
2026-07-09: refactored for more project compliant naming: masterloop: bitsXStr;
            main: bitsXStrTotal, det_bitsXStrTotal (hex the same)
2026-10-20: replaced the adapted version of original function convertToBase from the printf package
            into two specialized functions like in the Haskell solution => much faster program execution!


build on Ubuntu 24 LTS: do this only once:
                        use the CPM (Curry Package Manager) for this project:
                        $ cypm new random_bitstring_and_flexible_password_generator
                        $ cd random_bitstring_and_flexible_password_generator
                        $ cypm add time    # add this package

                        edit Main.curry (this file) in subdir ./random_bitstring_and_flexible_password_generator/src/Main.curry

run on Ubuntu 24 LTS:   $ cypm curry :load Main :eval main :quit


build on Ubuntu 24 LTS an executable:
                        $ cypm curry :load Main :save :quit
run this executable:    $ ./Main


$ curry -V
 _  _  ____  ___  ___  ___
( )/ )(_  _)/ __)/ __)(__ \
 )  (  _)(_( (__ \__ \ / _/
(_)\_)(____)\___)(___/(____)

Version 3.5.0-b2 of 2025-12-15 (installed at Mon Dec 15 22:46:06 CET 2025)
$

-}


module Main where  -- where for local variable declaration

-- for printing a non-deterministic list etc.: this important package is already included:
import Control.Search.AllValues (getAllValues, getOneValue)

-- see from System/Random.curry:
--   https://github.com/curry-packages/random/blob/master/src/System/Random.curry
--   I only need a random integer seed without bounds, without an
--   elaborate user defined function like: randomBetween :: Int -> Int -> IO Int
--   So, I just take the needed code from the official System/Random module:
import System.CPUTime ( getCPUTime )
import Data.Time ( CalendarTime(..), getClockTime, toUTCTime )
import System.IO ( hFlush, stdout )


m_ :: Int   -- m_ to prevent shadowing symbol m, which is used in two user defined functions below
m_ = 65521  -- = 2^16 - 15 < two16 = 65536
a :: Int
a = 17364
c :: Int
c = 0


------------------------------------------------------------------------------------------
--
-- user defined functions

-- see from System/Random module:
getRandomSeed :: IO Int
getRandomSeed =
  getClockTime >>= \time ->
  getCPUTime >>= \msecs ->
  let (CalendarTime y mo d h m s _) = toUTCTime time
   in return ((y+mo+d+h+(m+1)*(s+1)*(msecs+1)) `mod` (m_ - 2) + 1)  -- use m_ here! 2026-07-09, 2026-10-26: fixed range to [1..m_[


-- 2026-10-20: Duck.ai transpiled these specialized and user defined functions
-- in Haskell and of same names into KiCS2 Curry versions.
-- So, an adapted version of original function convertToBase from
-- file Format.curry from the printf package, and which is not exported,
-- is not longer used!
-- This modification brought down the execution time of the compiled program by over 40%!!
integer_to_bin_string :: Int -> String
integer_to_bin_string n = toBinary 16 n ""
  where
    toBinary :: Int -> Int -> String -> String
    toBinary count k acc
      | count == 0 = acc
      | k == 0     = toBinary (count - 1) 0 ('0' : acc)
      | otherwise  =
          let bit   = if k `mod` 2 == 0 then '0' else '1'
              nextK = k `div` 2
          in toBinary (count - 1) nextK (bit : acc)


integer_to_hex_string :: Int -> String
integer_to_hex_string n = toHex 4 n ""
  where
    toHex :: Int -> Int -> String -> String
    toHex count k acc
      | count == 0 = acc
      | k == 0     = toHex (count - 1) 0 ('0' : acc)
      | otherwise  =
          let remainder = k `mod` 16
              nextK     = k `div` 16
          in toHex (count - 1) nextK (hexChar remainder : acc)

    hexChar :: Int -> Char
    hexChar r =
      case r of
        0  -> '0'
        1  -> '1'
        2  -> '2'
        3  -> '3'
        4  -> '4'
        5  -> '5'
        6  -> '6'
        7  -> '7'
        8  -> '8'
        9  -> '9'
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        15 -> 'f'
        _  -> '0'


-- 2026-10-20: Duck.ai
convertToBasePad :: Int -> Int -> Int -> String
convertToBasePad targetLen base n =
  case (targetLen, base) of
    (16, 2)  -> integer_to_bin_string n
    (4, 16)  -> integer_to_hex_string n
    _        -> integer_to_hex_string n


-- The deterministic loop calculating the PRNG states and strings.
-- Curry can return tuple values directly instead of using output variables.
-- Keep this a pure function to make life easier.
-- 2026-10-20: don't use name length, use n instead,
-- since variable name length may shadow the inbuilt function of same name:
masterloop :: Int -> Int -> ([Int], [String], [String])
masterloop n seed
  | n == 0    = ([], [], [])
  | otherwise = (newSeed : restX, bitsXStr : restBitsX, bitsHexStr : restBitsHex)
  where
    newSeed    = (a * seed + c) `mod` m_

    -- bitsXStr = "0000000000000000"  -- just a test dummy
    bitsXStr   = convertToBasePad 16 2 newSeed

    -- bitsHexStr = "0000"  -- just a test dummy
    bitsHexStr = convertToBasePad 4 16 newSeed

    (restX, restBitsX, restBitsHex) = masterloop (n - 1) newSeed


-- Google AI from random_bitstring_and_flexible_password_generator.P for SWI Prolog
input_a_valid_number :: Int -> IO Int
input_a_valid_number nCharDefault = do
  putStr $ "\nPassword of " ++ show nCharDefault ++ " printable chars OK? 'y' or another integer number >= 8: "
  hFlush stdout
  answerStr <- getLine

  if answerStr == "y"
    then return nCharDefault
    else case isAllDigits answerStr of
           -- The cut operator (!) logic: if valid and >= 8, return it immediately
           Just n | n >= 8 -> return n
           -- Failure branch: prints error and loops back (recursive retry)
           _               -> do
             putStrLn "enter an integer number >= 8 or 'y'"
             input_a_valid_number nCharDefault

isAllDigits :: String -> Maybe Int
isAllDigits s
  | not (null s) && all isDigit s = Just (read s)
  | otherwise                     = Nothing


answer_yes_or_no :: Bool -> IO Bool
answer_yes_or_no selection = do
  putStr $ "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "
  hFlush stdout
  answerStr <- getLine

  if answerStr == "y"
    then return selection
    else return False


-- "Big AI" had a hard time with this function,
-- though they gave me some basic idea how to implement the recursion:
pw_generator :: Int -> [Int] -> String -> String -> String
pw_generator _ [] _ password = password  -- get rid of warning: Pattern matches are non-exhaustive
pw_generator length (firstRandNbr : newRandomNbrs) charPool password =
    if length > 0
      then pw_generator (length - char0_nbr_add - char1_nbr_add) newRandomNbrs charPool (password ++ char0_add ++ char1_add)
      else password
    where
      bin0 = convertToBasePad 16 2 firstRandNbr

      bin0_0 = take 8 bin0
      bin0_1 = drop 8 bin0

      char0 = bin_string_to_integer bin0_0
      char1 = bin_string_to_integer bin0_1

      char0a = chr char0
      char1a = chr char1
      -- char0a = 'a'  -- just a test dummy
      -- char1a = 'a'  -- just a test dummy

      (char0_add, char0_nbr_add) = if char0a `elem` charPool
                                    then ([char0a], 1)
                                    else ("", 0)

      (char1_add, char1_nbr_add) = if char1a `elem` charPool && (length - char0_nbr_add > 0)
                                    then ([char1a], 1)
                                    else ("", 0)


-- Google AI: Converts a binary string ("0101...") back into an Integer
bin_string_to_integer :: String -> Int
bin_string_to_integer = foldl (\acc d -> acc * 2 + (if d == '1' then 1 else 0)) 0


-- end of user defined functions
--
------------------------------------------------------------------------------------------


main :: IO ()
main = do
    let end = 62500  -- 62500 for exactly 1M binary digits; let for local variable declaration
    -- let end = 10  -- for testing

    let fileBitsX   = "random_bitstring.bin"
    let fileBitsHex = "random_bitstring.byte"

    x0 <- getRandomSeed
    -- putStrLn ("x0 = " ++ show x0)  -- for testing

    putStrLn ("\ngenerating a random bit stream...")

    let (x, bitsXList, bitsHexList) = masterloop end x0

    -- let nums = [1..5]  -- just a demo dummy
    -- mapM_ print nums  -- this works because of its determinism!
    det_x <- getAllValues x
    -- putStrLn ("x = " ++ show det_x)  -- for testing
    -- x = [[18437,4462,32346,9932,7976,49391,20955,24507,46174,50380]]  -- for example

    let bitsXStr   = concat bitsXList
    let bitsHexStr = concat bitsHexList
    det_bitsXStrTotal   <- getOneValue bitsXStr
    det_bitsHexStrTotal <- getOneValue bitsHexStr

    -- putStrLn ("det_bitsXStrTotal = " ++ show det_bitsXStrTotal)  -- for testing
    -- putStrLn ("det_bitsHexStrTotal = " ++ show det_bitsHexStrTotal)  -- for testing

    -- write bit stream to disk:
    case det_bitsXStrTotal of
      Just s  -> catch (do writeFile fileBitsX s
                           putStrLn ("Bit stream has been written to disk under name:  " ++ fileBitsX))  -- 2026-06-29
                 (\err -> putStrLn ("could not write to file: " ++ fileBitsX ++ " ! -- " ++ show err))  -- 2026-06-29
      Nothing -> error ("could not write to file: " ++ fileBitsX)  -- 2026-06-29

    -- write byte stream to disk:
    case det_bitsHexStrTotal of
      Just s  -> catch (do writeFile fileBitsHex s
                           putStrLn ("Byte stream has been written to disk under name: " ++ fileBitsHex))  -- 2026-06-29
                 (\err -> putStrLn ("could not write to file: " ++ fileBitsHex ++ " ! -- " ++ show err))  -- 2026-06-29
      Nothing -> error ("could not write to file: " ++ fileBitsHex)  -- 2026-06-29


    let nCharDefault = 12
    nChar <- input_a_valid_number(nCharDefault)
    -- putStrLn ("NChar = " ++ show nChar)  -- for testing

    with_special_chars <- answer_yes_or_no(True)
    -- putStrLn ("with_special_chars = " ++ show with_special_chars)  -- for testing


    let pattern = if with_special_chars
                then ['!' .. '~']
                else ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']
    -- putStrLn ("pattern = " ++ show pattern)  -- for testing


    -- !!super-important: do not pass x, pass det_x, which is its deterministic version!!
    let (det_x_inner_list : _) = det_x
    -- putStrLn ("det_x_inner_list = " ++ show det_x_inner_list)  -- for testing
    -- det_x_inner_list = [11234,11159,19279,13767,29580,8001,24844,952,19236,53367]  -- for example

    let password = pw_generator nChar det_x_inner_list pattern ""  -- with let back in "deterministic land"!
    putStrLn ("\nYour password of " ++ show nChar ++ " characters is: " ++ password)
    -- show password will make something like: "..."

    -- putStrLn ("\n")  -- last IO dummy for testing

-- end of random_bitstring_and_flexible_password_generator.curry
