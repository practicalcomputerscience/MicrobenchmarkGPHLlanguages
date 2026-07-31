2026-07-31: work in progress

<br/>

# Integer to string conversions

Not all general purpose, high-level programming languages, including their officially maintained libraries, provide in-built functions to convert an unsigned 32-bit integer number into its 
representations as a:

- binary string, including padding to 16 characters with leading "0"'s if needed, and
- hexadecimal string, including padding to 4 characters with leading "0"'s if needed, and only using lowercase letters "a" to "f".

In no corner of my microbenchmark program I employed more user defined functions than for these two functionalities (which are already multi-functionalities).

Some languages, including young ones, provide inbuilt functions to do all jobs on a integer input number, some languages, including very old ones, don't provide any official solutions.

Some languages only partly provide the required functionalities, often with missing the padding with leading zeros.

<br/>

In order to provide some overview of this messy situation, I created another language list, see below.

By the way: the clever implementation of above functionalities often has a surprisingly high impact on the execution speed of an implementation.
See for example at [String padding](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/README.md#string-padding):

> Integrated string padding on the left hand side of mandatory string variables bits_x_str to 16 characters of '0' and '1' and bits_hex_str (or similarly named) to 4 characters of '0' to 'f' can also have a major effect on execution speed at some implementations.
>
> For example, in the Haskell implementation this measure alone brought down program execution time by over 40%!

<br/>

tbd -- the list







<br/>

##_end
