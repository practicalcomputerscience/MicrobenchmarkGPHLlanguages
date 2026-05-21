"
random_bitstring_and_flexible_password_generator.st

2026-05-20

run on Ubuntu 24 LTS:   $ gst --quiet random_bitstring_and_flexible_password_generator.st


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
Object subclass: random_bitstring_and_flexible_password_generator [
    "[..] is a block for grouping statements, especially when statements are not expressions"

    random_bitstring_and_flexible_password_generator class >> main [
        "random_bitstring_and_flexible_password_generator class >> main defines an instance method named main in class random_bitstring_and_flexible_password_generator"

        | END m a c file_bits_x file_bits_hex x rnd bits_x bits_hex bits_x_str bits_hex_str fileOut
          N_CHAR answer answer_str WITH_SPECIAL_CHARS pattern i j pw_chars bin0_0 bin0_1 char0 char1 current_x |
        "| v1 v2 | are variable declarations"

        END := 62501.  "62501 for exactly 1M binary digits"
        "END := 100."   "for testing"
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


        "make a password of N_CHAR printable chars: user input requested here"
        N_CHAR := 12.
        answer := false.
        [ answer ] whileFalse: [
            '' displayNl.  "a new line here"
            Transcript show: 'Password of ', N_CHAR printString, ' printable chars OK? ''y'' or another integer number >= 8: '.
            Transcript flush.
            answer_str := stdin nextLine.
            "answer_str displayNl."  "for testing"

            (answer_str = 'y')
                ifTrue:  [ answer := true ]
                ifFalse: [
                    "Strictly check that every character is a digit and the string isn't empty (Google AI)"
                    (answer_str notEmpty and: [ answer_str allSatisfy: [ :ch | ch isDigit ] ])
                    ifTrue: [
                        N_CHAR := answer_str asInteger.
                        (N_CHAR < 8)
                            ifTrue:  [ Transcript show: 'enter an integer number >= 8 or ''y'''; cr ]
                            ifFalse: [ answer := true ]
                    ]
                    ifFalse: [
                        Transcript show: 'enter an integer number >= 8 or ''y'''; cr
                    ]
                ]
        ].
        "'N_CHAR = ' display. N_CHAR printNl."  "for testing"

        WITH_SPECIAL_CHARS := true.
        answer := false.
        [ answer ] whileFalse: [
            '' displayNl.  "a new line here"
            Transcript show: 'Do you want me to use special characters like .;,+*... ? ''y'' or ''n'': '.
            Transcript flush.
            answer_str := stdin nextLine.

            (answer_str = 'y')
                ifTrue:  [ answer := true ]
                ifFalse: [
                    WITH_SPECIAL_CHARS := false.
                    answer := true
                ]
        ].
        "'WITH_SPECIAL_CHARS = ' display. WITH_SPECIAL_CHARS printNl."  "for testing"


        "Define the matching pattern using POSIX bracket groups (Google AI)"
        pattern := WITH_SPECIAL_CHARS
            ifTrue:  [ '^[[:print:]]$' ]   "Strictly matches exactly one printable ASCII character"
            ifFalse: [ '^[[:alnum:]]$' ].  "Strictly matches exactly one alphanumeric character"


        i := 0.   "char counter for the password"
        j := 1.   "counter for x (Note: Smalltalk arrays are 1-indexed)"
        pw_chars := ''.

        [ i < N_CHAR ] whileTrue: [  "Google AI"
            current_x := x at: j.  "Get the current 16-bit integer value from array x"

            bin0_0 := current_x bitShift: -8.   "Extract high 8 bits by shifting right 8 bits: elegant solution"
            bin0_1 := current_x bitAnd: 16rFF.  "Extract low 8 bits using a bitwise AND mask with 255 (0xFF)"

            char0 := (Character value: bin0_0) asString.  "Convert the raw integer values into actual String objects"
            char1 := (Character value: bin0_1) asString.

            (char0 =~ pattern) ifMatched: [
                pw_chars := pw_chars, char0.
                i := i + 1
            ].

            (char1 =~ pattern) ifMatched: [ :match |
                "Google AI: adding the :match variable fulfills GNU Smalltalk's regex design.
                 If the string matches the pattern, it safely evaluates the block.
                 If it fails, it cleanly returns nil and skips the block."
                i < N_CHAR ifTrue: [
                    pw_chars := pw_chars, char1.
                    i := i + 1
                ]
            ].

            j := j + 1
        ].

        '' displayNl.  "a new line here"
        Transcript show: 'Your password of ', N_CHAR printString, ' characters is: ', pw_chars; cr.
    ]
]

"Execute the code automatically when executing the file"
random_bitstring_and_flexible_password_generator main!

"end of random_bitstring_and_flexible_password_generator.st"
