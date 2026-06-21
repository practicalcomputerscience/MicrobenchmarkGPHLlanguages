{-
random_streams_for_perf_stats.curry is actually file ./random_streams_for_perf_stats/src/Main.curry

a program for the KiCS2 compiler: https://www.curry-lang.org/kics2/

2026-06-20/21

build on Ubuntu 24 LTS: do this only once:
                        use the CPM (Curry Package Manager) for this project:
                        $ cypm new random_streams_for_perf_stats
                        $ cd random_streams_for_perf_stats
                        $ cypm add time  # add this package

                        edit Main.curry (this file) in subdir ./random_streams_for_perf_stats/src/Main.curry

run on Ubuntu 24 LTS:   $ cypm curry :load Main :eval main :quit


build on Ubuntu 24 LTS an executable:
                        $ cypm curry :load Main :save :quit
run this executable:   $ ./Main
                        $ time ./Main => real	0m1.496s
                        $ sudo perf stat -r 20 ./Main
                        # BE CAREFUL!
                        # this command may run into:
                        # Main: *** UserException: could not write to file: "random_bitstring.bin"
                        => 1.48046 +- 0.00428 seconds time elapsed  ( +-  0.29% )  # 2026-06-21 18:04:33


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

m :: Int
m = 65521  -- = 2^16 - 15 < two16 = 65536
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
   in return ((y+mo+d+h+(m+1)*(s+1)*(msecs+1)) `mod` m)


--   this is an adapted version of the original function from
--   file Format.curry from the printf package, and which is not exported!
convertToBase :: Int -> Int -> String
convertToBase b n =
    if (n == 0) then "0"
      else cTB "" b n
  where
    cTB :: String -> Int -> Int -> String
    cTB acc base m = if (m == 0) then acc else
      let dr = ((div m base),(mod m base))
          d  = (fst dr)
          r  = (snd dr)
          st = if (r < 10) then (show r) else
            case r of
              10 -> "a"
              11 -> "b"
              12 -> "c"
              13 -> "d"
              14 -> "e"
              15 -> "f"
      in cTB (st ++ acc) b d

-- New function from Google AI: takes target length, base, and number
convertToBasePad :: Int -> Int -> Int -> String
convertToBasePad targetLen base n =
    let baseStr = convertToBase base n
        strLen  = length baseStr
        padLen  = targetLen - strLen
    -- Replicate '0' padLen times and prepend to the string
    in if padLen > 0
       then replicate padLen '0' ++ baseStr
       else baseStr


--- The deterministic loop calculating the PRNG states and strings.
--- Curry can return tuple values directly instead of using output variables.
--- keep this a pure function to make life easier:
masterloop :: Int -> Int -> ([Int], [String], [String])
masterloop 0 _ = ([], [], [])
masterloop length seed
    | length > 0 = (newSeed : restX, bitsX : restBitsX, bitsHex : restBitsHex)
    where
      -- newSeed = (17364 * seed + 0) `mod` 65521
      newSeed = (a * seed + c) `mod` m

      -- bitsX = "0000000000000000"  -- just a test dummy
      bitsX   = convertToBasePad 16 2 newSeed

      -- bitsHex = "0000"  -- just a test dummy
      bitsHex = convertToBasePad 4 16 newSeed

      (restX, restBitsX, restBitsHex) = masterloop (length - 1) newSeed


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

    let (_, bitsXList, bitsHexList) = masterloop end x0  -- for final solution
    -- let (x, bitsXList, bitsHexList) = masterloop end x0  -- for testing

    -- let nums = [1..5]  -- just a demo dummy
    -- mapM_ print nums  -- this works because of its determinism!
    -- deterministicList <- getAllValues x  -- for testing
    -- putStrLn ("x = " ++ show deterministicList)  -- for testing

    let bitsXStr   = concat bitsXList
    let bitsHexStr = concat bitsHexList
    det_bitsXStr   <- getOneValue bitsXStr
    det_bitsHexStr <- getOneValue bitsHexStr

    -- putStrLn ("det_bitsXStr = " ++ show det_bitsXStr)  -- for testing
    -- putStrLn ("det_bitsHexStr = " ++ show det_bitsHexStr)  -- for testing

    -- write bit stream to disk:
    case det_bitsXStr of
      Just s  -> catch (do writeFile fileBitsX s
                           putStrLn ("Bit stream has been written to disk under name:  " ++ show fileBitsX))
                 (\err -> putStrLn ("could not write to file: " ++ show fileBitsX ++ " ! -- " ++ show err))
      Nothing -> error ("could not write to file: " ++ show fileBitsX)

    -- write byte stream to disk:
    case det_bitsHexStr of
      Just s  -> catch (do writeFile fileBitsHex s
                           putStrLn ("Byte stream has been written to disk under name: " ++ show fileBitsHex))
                 (\err -> putStrLn ("could not write to file: " ++ show fileBitsHex ++ " ! -- " ++ show err))
      Nothing -> error ("could not write to file: " ++ show fileBitsHex)

-- end of random_streams_for_perf_stats.curry
