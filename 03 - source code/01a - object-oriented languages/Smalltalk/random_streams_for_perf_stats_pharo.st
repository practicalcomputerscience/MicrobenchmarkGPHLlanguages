"
random_streams_for_perf_stats_pharo.st

2026-05-24

download latest prebuilt from: https://pharo.org/download
fix in ~/.bashrc potentially: alias pharo='~/scripts/Smalltalk/Pharo/pharo-launcher-linux-3.4.3-x64/pharo-launcher/pharo-vm/pharo'

run on Ubuntu 24 LTS: $ pharo --headless ./pharo-launcher-linux-3.4.3-x64/pharo-launcher/shared/images/pharo-stable/Pharo13.0-SNAPSHOT-64bit-374678e2d5.image --script ./random_streams_for_perf_stats_pharo.st
                      
                      $ time pharo --headless ./pharo-launcher-linux-3.4.3-x64/pharo-launcher/shared/images/pharo-stable/Pharo13.0-SNAPSHOT-64bit-374678e2d5.image --script ./random_streams_for_perf_stats_pharo.st
                        => real	0m0.221s


$ pharo --version
Pharo v10.3.8 built on Aug 13 2025 11:48:47 Compiler: 5.4.0 20160609
Built from: v10.3.8+0.a7c8a0b - Commit: a7c8a0b - Date: 2025-08-12 19:03:31 +0200
$ 

"

| END m a c file_bits_x file_bits_hex x rnd bits_x bits_hex bits_x_str bits_hex_str rnd_seed bits_x_str_total bits_hex_str_total fileOut |

END := 62501.  "62501 for exactly 1M binary digits"
"END := 10."   "for testing"
m := 65521.
a := 17364.
c := 0.

file_bits_x   := 'random_bitstring.bin'.
file_bits_hex := 'random_bitstring.byte'.

x := Array new: END.

rnd := Random new.
"random seeding: https://github.com/pharo-project/pharo/blob/Pharo14/src/Random-Core/Random.class.st"
rnd_seed := rnd useUnixRandomGeneratorSeed.
"Transcript show: 'rnd_seed = '; show: rnd_seed; cr."  "for testing: true result is to be expected"

x at: 1 put: ((rnd next * (m - 2)) asInteger + 1).
"Transcript show: 'x[1] = '; show: (x at: 1); cr."  "for testing"

bits_x   := WriteStream on: (String new).
bits_hex := WriteStream on: (String new).

Transcript cr; show: 'generating a random bit stream...'; cr.

2 to: END do: [ :i |
    | prevVal currentVal |
    prevVal    := x at: (i - 1).
    currentVal := ((a * prevVal) + c) rem: m.
    x at: i put: currentVal.
    
    bits_x_str   := currentVal printStringBase: 2 nDigits: 16.
    bits_x nextPutAll: bits_x_str.

    bits_hex_str := (currentVal printStringBase: 16 nDigits: 4) asLowercase.
    bits_hex nextPutAll: bits_hex_str.
].


bits_x_str_total   := bits_x contents.
bits_hex_str_total := bits_hex contents.


"write bit stream to disk"
fileOut := file_bits_x asFileReference.
[
    fileOut writeStreamDo: [ :stream |
        stream nextPutAll: bits_x_str_total.
    ].
    Transcript show: 'Bit stream has been written to disk under name:  ', file_bits_x; cr.
] on: FileException do: [ :ex |
    Transcript show: 'could not write to file: ', file_bits_x; cr.  "a real error message is not printed here, so I left it away"
].

"write byte stream to disk"
fileOut := file_bits_hex asFileReference.
[
    fileOut writeStreamDo: [ :stream |
        stream nextPutAll: bits_hex_str_total.
    ].
    Transcript show: 'Byte stream has been written to disk under name: ', file_bits_hex; cr.
] on: FileException do: [ :ex |
    Transcript show: 'could not write to file: ', file_bits_hex; cr.
].


Smalltalk snapshot: false andQuit: true.

"end of random_streams_for_perf_stats_pharo.st"
