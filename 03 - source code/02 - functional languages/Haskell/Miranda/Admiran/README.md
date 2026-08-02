2026-08-02: work in progress: tbd

<br/>

# Admiran

https://github.com/taolson/Admiran

Admiran: Miran + da -> ad + miran -> Admiran

<br/>

Admiran for **compilation** is an "extended subset" successor of interpreted Miranda: [Miranda language features removed from Admiran](https://github.com/taolson/Admiran#miranda-language-features-removed-from-admiran)

<br/>

Since this language is still small and young, it's practically forcing the human developer to fall back on his own capabilities without the help of "Big AI".

The "speed part" of the microbenchmark program, [random_streams_for_perf_stats.am](./random_streams_for_perf_stats.am) is in some parts, that is user defined functions,
almost a 1:1 copy based on the Miranda implementation [random_streams_for_perf_stats.m](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell/Miranda/random_streams_for_perf_stats.m).

<br/>

However, there are some surprising and profound _structural_ differences between both programs. Only have a look at both _main_ functions,
independently of their sourcing of an initial random seed:

Miranda:

```
main :: [sys_message]
main =
  [Stdout "\ngenerating a random bit stream..."]
  ++ write_to_file file_bits_x   bits_x_str_total   "bit"
  ++ write_to_file file_bits_hex bits_hex_str_total "byte"
  ++ [Stdout "\n"]
  where
    os_seed = numval (hd (lines $-)) + 1
    || + 1 to not have a 0 seed!
    || $- = standard symbol for the list of characters typed at the keyboard
    || lines breaks $- into lines, hd takes first line, numval converts it into a number

    (x, bits_x, bits_hex) = masterloop upper_limit os_seed

    bits_x_str_total   = concat bits_x
    bits_hex_str_total = concat bits_hex

```

Admiran:

```
main :: io ()
main
  = random_seed >>= \os_seed ->
    || putStrLn ("os_seed = " ++ showint os_seed) >>  || for testing
    putStrLn "\ngenerating a random bit stream..." >>
    (masterloop ([], [], []) upper_limit os_seed |> write_strings_to_files)
```

You can rather easily put much more tasks into the _main_ function of a Miranda script, compared to a meticulously crafted and often very short _main_ function of an Admiran program.

Also the provided [example programs](https://github.com/taolson/Admiran/tree/main/examples) show this phenomenon.

In Admiran it's _even_ harder to get all types along its _function composition_ right, according to my observations.

<br/>

## No exception handling

What's also missing yet in Admiran are means of exception handling the coder can (conveniently) use in his own programs.
Thus, writing the (big) strings to files lacks exception handling in my Admiran program:

```
write_strings_to_files :: ([int], [string], [string]) -> io ()
write_strings_to_files (ints, strings1, strings2)
  = writeFile file_bits_x (concat strings1) >>
    putStrLn ("Bit stream has been written to disk under name:  " ++ file_bits_x) >>
    writeFile file_bits_hex (concat strings2) >>
    putStrLn ("Byte stream has been written to disk under name: " ++ file_bits_hex)
```

If there's a problem when writing to a file, the program exits immediatly, but with a detailed error message:

```
tbd
```

So, the Admiran compiler has means of exception handling implemented, as seen in source file [exception.am](https://github.com/taolson/Admiran/blob/main/compiler/exception.am),
but nothing I can (easily) import into my own program.

<br/>

Missing exception handling is the reason why [also](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#miranda) [Admiran](tbd) isn't included in my "official" listing of programming languages.

<br/>

## Installation tips

It's rather easy and straightforward to compile and install Admiran from sources: tbd

tbd


<br/>

##_end
