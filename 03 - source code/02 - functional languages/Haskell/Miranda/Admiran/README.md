# Admiran

https://github.com/taolson/Admiran

Admiran: Miran + da -> ad + miran -> Admiran

---

Table of contents:

- [Idea of Admiran]()
- [No exception handling]()
- [Execution speed]()
- [Installation tips]()

<br/>

---

## Idea of Admiran

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
$ ./random_streams_for_perf_stats 

generating a random bit stream...
random_bitstring.bin: Permission denied
$
```

So, the Admiran compiler has means of exception handling implemented, as it can seen in source code file [exception.am](https://github.com/taolson/Admiran/blob/main/compiler/exception.am),
but nothing I can (easily) use in my own program.

<br/>

Missing exception handling, as core feature in this language benchmarking, is the reason why [also](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#miranda) [Admiran](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#admiran) isn't included in my "official" listing of programming languages.

<br/>

#### Execution speed

Compiled Admiran program [random_streams_for_perf_stats.am](./random_streams_for_perf_stats.am) takes about 340 milliseconds to run. This is blasting the 2 seconds of the Miranda script,
but still far away from the 43 milliseconds of the Haskell executable.

<br/>

## Installation tips

It's rather easy and straightforward to compile and install Admiran from sources. I took source from here: https://github.com/taolson/Admiran/releases/tag/v2.6,
unpacked them.

In unpacked top-level directory _./Admiran-2.6/compiler_ edit configuration file _config.am_ like this:

```
|| admiranLibPath        = "../lib"        || set to absolute path of Admiran lib directory: comment this original line
admiranLibPath        = "<absolute path>/Admiran-2.6/lib"  || this is the new line with the ABSOLUTE PATH to lib
```

Then do this in top-level directory _./Admiran_:

```
$ make  # this takes its time!
...
building amc
generating STG code 66934 STG insns
generating asm code
linking with runtime
diff compiler/amc.s compiler/amc.s.REF

=== amc compiler built successfully ===
mv compiler/amc bin/amc
$ 
```

Finally, edit your _~/.bashrc_ file like this for example:

```
export PATH="$HOME/scripts/Miranda/Admiran/Admiran-2.6/bin:$PATH"
```

..and activate it with command: _$ source ~/.bashrc_

There's no command to ask the Admiran compiler for its version.

<br/>

##_end
