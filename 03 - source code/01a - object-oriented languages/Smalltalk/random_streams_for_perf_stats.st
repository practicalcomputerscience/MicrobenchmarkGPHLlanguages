"
random_streams_for_perf_stats.st

2026-05-20

run on Ubuntu 24 LTS:   $ gst --quiet random_streams_for_perf_stats.st
                        # switch --quiet to avoid displaying: Global garbage collection... done
                        Bit stream has been written to disk under name:  random_bitstring.bin
                        Byte stream has been written to disk under name: random_bitstring.byte
                        $
                        $ time gst --quiet random_streams_for_perf_stats.st => 0m0.256s


$ gst --version
GNU Smalltalk version 3.2.91
Copyright 2009 Free Software Foundation, Inc.
Written by Steve Byrne (sbb@gnu.org) and Paolo Bonzini (bonzini@gnu.org)
...
$

"

" ----------------------------- "
"                               "
" user defined functions        "

"Extend Integer with a method to produce a 16-bit binary string (partly proposed by MS Copilot)"
Integer extend [
    integer_to_bin_string [
        | binStr padLength |
        binStr    := self printStringRadix: 2.
        binStr    := binStr copyFrom: 3 to: binStr size.
        padLength := (16 - binStr size) max: 0.
        ^ (String new: padLength withAll: $0), binStr.  "^ for returning a value"
    ]
]

"Extend Integer with a method to produce a 4-digit hexadecimal string"
Integer extend [
    integer_to_hex_string [
        | hexStr padLength |
        hexStr    := (self printStringRadix: 16) asLowercase.
        hexStr    := hexStr copyFrom: 4 to: hexStr size.
        padLength := (4 - hexStr size) max: 0.
        ^ (String new: padLength withAll: $0), hexStr.
    ]
]

" end of user defined functions "
"                               "
" ----------------------------- "


"Define a new class for the performance stats generation"
Object subclass: random_streams_for_perf_stats [
    "[..] is a block for grouping statements, especially when statements are not expressions"

    random_streams_for_perf_stats class >> main [
        "random_streams_for_perf_stats class >> main defines an instance method named main in class random_streams_for_perf_stats"

        | END m a c file_bits_x file_bits_hex x rnd bits_x bits_hex bits_x_str bits_hex_str fileOut |
        "| v1 v2 | are variable declarations"

        END := 62501.  "62501 for exactly 1M binary digits"
        "END := 10."   "for testing"
        m := 65521.    "m := 5. for testing"
        a := 17364.
        c := 0.

        file_bits_x   := 'random_bitstring.bin'.
        file_bits_hex := 'random_bitstring.byte'.

        "Initialize an Array of size 'END' filled with 0s"
        x := Array new: END.

        rnd := Random new.  "GNU Smalltalk shared Random number generator"
        x at: 1 put: ((rnd next * (m - 2)) asInteger + 1).  "(...) is for grouping expressions; m - 2 is really needed here"
        "'x[1] = ' display. (x at: 1) printNl."  "for testing"

        "Use WriteStreams as the high-performance equivalent to StringBuilder"
        bits_x   := WriteStream on: (String new).
        bits_hex := WriteStream on: (String new).

        '' displayNl. 'generating a random bit stream...' displayNl.

        "Loop from 2 to END (Smalltalk arrays are 1-indexed)"
        2 to: END do: [ :i |
            | prevVal currentVal |
            prevVal    := x at: (i - 1).
            currentVal := ((a * prevVal) + c) rem: m.
            x at: i put: currentVal.
            "'' displayNl. 'x[i] = ' display. currentVal printNl."  "for testing"

            bits_x_str   := currentVal integer_to_bin_string.  "call user-defined function integer_to_bin_string"
            "bits_x_str  displayNl."  "for testing"
            bits_x nextPutAll: bits_x_str.

            bits_hex_str := currentVal integer_to_hex_string.
            "bits_hex_str  displayNl."  "for testing"
            bits_hex nextPutAll: bits_hex_str.
        ].


        "write bit stream to disk"
        [
            fileOut := FileStream open: file_bits_x mode: FileStream write.
            fileOut nextPutAll: bits_x contents.
            fileOut close.
            'Bit stream has been written to disk under name:  ' display. file_bits_x displayNl.
        ] on: Error do: [ :ex |
            Transcript nextPutAll: 'could not write to file: '; nextPutAll: file_bits_x;
                       nextPutAll: ' -- '; nextPutAll: ex messageText; cr.
        ].

        "write byte stream to disk"
        [
            fileOut := FileStream open: file_bits_hex mode: FileStream write.
            fileOut nextPutAll: bits_hex contents.
            fileOut close.
            'Byte stream has been written to disk under name: ' display. file_bits_hex displayNl.
        ] on: Error do: [ :ex |
            Transcript nextPutAll: 'could not write to file: '; nextPutAll: file_bits_hex;
                       nextPutAll: ' -- '; nextPutAll: ex messageText; cr.
        ].
    ]
]

"Execute the code automatically when executing the file"
random_streams_for_perf_stats main!

"end of random_streams_for_perf_stats.st"
