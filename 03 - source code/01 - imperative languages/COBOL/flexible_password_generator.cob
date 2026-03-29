*> flexible_password_generator.cob
*>
*> this file name is too long: random_bitstring_and_flexible_password_generator.cob
*>
*> 2026-03-14/15/16
*>
*> build on Ubuntu 24 LTS: $ cobc -F -W -x flexible_password_generator.cob -o flexible_password_generator
*>                                -F for free format to not use first 7 source line positions for sequence numbers used on punched cards !!
*>
*> run on Ubuntu 24 LTS:   $ ./flexible_password_generator
*>
*>
*> still these warnings:
*>   ...in paragraph 'MAIN-LOGIC':
*>   ...warning: some digits may be truncated [-Wpossible-truncate]
*>   ...note: 'N-CHAR-OUT' defined here as PIC 9(38) [-Wpossible-truncate]
*>   ...note: 'N-CHAR' defined here as PIC 9(2) [-Wpossible-truncate]
*>
*> still these warnings when building (I didn't build GnuCOBOL from its sources):
*>   <command-line>: warning: "_FORTIFY_SOURCE" redefined
*>   <command-line>: note: this is the location of the previous definition
*>
*>
*> $ cobc -V  # cobc (GnuCOBOL) 3.1.2.0
*>


IDENTIFICATION DIVISION.
PROGRAM-ID. FLEXIBLE-PASSWORD-GENERATOR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT FILE-BITS-X ASSIGN TO FILE-BITS-X-NAME
        ORGANIZATION IS LINE SEQUENTIAL  *> adds a final line feed char, but works!
        FILE STATUS IS WS-FS.  *> Link to status variable
    SELECT FILE-BITS-HEX ASSIGN TO FILE-BITS-HEX-NAME
        ORGANIZATION IS LINE SEQUENTIAL  *> adds a final line feed char, but works!
        FILE STATUS IS WS-FS.  *> Link to status variable

DATA DIVISION.
FILE SECTION.
FD  FILE-BITS-X.
01  BIT-RECORD        PIC X(1000000).  *> PIC X() for holding alphanumeric characters
                                       *> PIC is a picture clause
FD  FILE-BITS-HEX.
01  HEX-RECORD        PIC X(250000).


WORKING-STORAGE SECTION.
01  FILE-BITS-X-NAME    PIC X(20) VALUE "random_bitstring.bin".
01  FILE-BITS-HEX-NAME  PIC X(21) VALUE "random_bitstring.byte".

*> Constants
77  C-END             PIC 9(9)  VALUE 62501.  *> 62501 for exactly 1M binary digits
                                              *> PIC 9() is for numeric data items made up only of digits 0..9
*> 77  C-END             PIC 9(9)  VALUE 10.  *> for testing
77  C-M               PIC 9(9)  VALUE 65521.  *> = 2^16 - 15
77  C-A               PIC 9(9)  VALUE 17364.
77  C-C               PIC 9(9)  VALUE 0.

*> Variables
01  WS-FS             PIC XX.  *> 00 = Success, others = Error

77  I                 PIC 9(9).  *> used for more than one counting procedure
77  X-PREV            PIC 9(9).
77  X-CURR            PIC 9(9).
77  TEMP-VAL          PIC 9(9).
77  REM-BIN-VAL       PIC 9 VALUE 0.
77  REM-HEX-VAL       PIC 9(9).

*> Seed generation
77  CURRENT-TIME      PIC 9(9).


*> Define an array for C-END random integer numbers:
01  X-ARRAY.
    05 X-ELEMENT      PIC 9(9) OCCURS 62501 TIMES.


*> String Building Buffers
01  BITS-X            PIC X(1000000) VALUE ALL ".".
01  BITS-HEX          PIC X(250000) VALUE ALL ".".

01  BITS-X-STR        PIC X(16) VALUE SPACES.
01  BITS-HEX-STR      PIC X(4) VALUE SPACES.

77  BIT-PTR           PIC 9(9) VALUE 1.
77  HEX-PTR           PIC 9(9) VALUE 1.

*> Conversion Helpers
77  HEX-DIGITS        PIC X(16) VALUE "0123456789abcdef".
77  STR-INDEX         PIC 9(2).

*> for the user dialog
01  ANSWER            PIC X VALUE 'N'.
01  ANSWER-STR        PIC X(38).          *> numeric fields cannot be larger than 38 digits in COBOL
01  N-CHAR-IN         PIC 9(2) VALUE 12.  *> this is a constant fixed at value 12
01  N-CHAR-OUT        PIC 9(38).          *> this holds the answer of the user, if answer is not 'y'
01  N-CHAR            PIC 9(2).  *> holds final value: similar solution like in Ada: 99 is the upper limit

01  WITH-SPECIAL-CHARS   PIC X VALUE 'y'.

01  ASCII-CHAR        PIC X.
01  CHAR-SET          PIC X(94).  *> 94 is the maximum string length for characters !...~ <==> 33...126 (ASCII codepoint)

77  J                 PIC 9(9) VALUE 1.  *> counter for X-ELEMENT; start with index 1
01  PW-CHARS          PIC X(99).  *> 99 characters is the maximum password length
77  BIN0              PIC X(16).
77  BIN0-0            PIC X(8).
77  BIN0-1            PIC X(8).
77  INP-STR           PIC X(8).
77  K                 PIC 9(2).   *> counter for INP-STR
77  OUT-NBR           PIC 9(3).   *> ASCII codepoint 0..255
77  CHAR0             PIC X(1).
77  CHAR1             PIC X(1).


PROCEDURE DIVISION.
MAIN-LOGIC.
    DISPLAY " "  *> have an extra new line
    END-DISPLAY
    DISPLAY "generating a random bit stream..."
    END-DISPLAY

    *> Initialize Seed using System Time (Equivalent to rnd.nextInt)
    ACCEPT CURRENT-TIME FROM TIME
    END-ACCEPT
    COMPUTE X-PREV = FUNCTION MOD(CURRENT-TIME, C-M - 1) + 1
    END-COMPUTE
    *> DISPLAY "X-PREV = " X-PREV  *> for testing
    *> END-DISPLAY  *> for testing

    *> Main Loop (Starts from 1 like the Groovy for-loop)
    PERFORM VARYING I FROM 1 BY 1 UNTIL I >= C-END
        *> x[i] = (a * x[i-1] + c) % m
        COMPUTE X-CURR = FUNCTION MOD(C-A * X-PREV + C-C, C-M)  *> essential: only have one line of computations here!
        END-COMPUTE
        *> DISPLAY X-CURR  *> for testing
        *> END-DISPLAY  *> for testing

        MOVE X-CURR TO X-ELEMENT(I)

        *> Convert to Binary String (Manual 16-bit padding)
        MOVE X-CURR TO TEMP-VAL
        PERFORM CONVERT-TO-BINARY

        *> Convert to Hex String (Manual 4-char padding)
        MOVE X-CURR TO TEMP-VAL
        PERFORM CONVERT-TO-HEX

        MOVE X-CURR TO X-PREV
    END-PERFORM.

    *> write bit stream to disk:
    OPEN OUTPUT FILE-BITS-X
    WRITE BIT-RECORD FROM BITS-X
    END-WRITE
    IF WS-FS NOT = "00"
        DISPLAY "could not write to file: " FILE-BITS-X-NAME WITH NO ADVANCING  *> avoid final line feed
        END-DISPLAY
        DISPLAY " ! -- GnuCOBOL-specific file status code: " WS-FS
        END-DISPLAY
    ELSE
        DISPLAY "Bit stream has been written to disk under name:  " FILE-BITS-X-NAME
        END-DISPLAY
    END-IF.
    CLOSE FILE-BITS-X.

    *> write byte stream to disk:
    OPEN OUTPUT FILE-BITS-HEX
    WRITE HEX-RECORD FROM BITS-HEX
    END-WRITE
    IF WS-FS NOT = "00"
        DISPLAY "could not write to file: " FILE-BITS-HEX-NAME WITH NO ADVANCING  *> avoid final line feed
        END-DISPLAY
        DISPLAY " ! -- GnuCOBOL-specific file status code: " WS-FS
        END-DISPLAY
    ELSE
        DISPLAY "Byte stream has been written to disk under name: " FILE-BITS-HEX-NAME
        END-DISPLAY
    END-IF.
    CLOSE FILE-BITS-HEX.

    *> for testing:
    *> DISPLAY "X-ELEMENT(1): " X-ELEMENT(1)
    *> END-DISPLAY
    *> COMPUTE TEMP-VAL = C-END - 1
    *> END-COMPUTE
    *> DISPLAY "X-ELEMENT(" TEMP-VAL "): " X-ELEMENT(TEMP-VAL)
    *> END-DISPLAY
    *> DISPLAY "X-ELEMENT(" C-END "): " X-ELEMENT(C-END)
    *> END-DISPLAY


    *> make a password of N_CHAR printable chars: user input requested here
    PERFORM UNTIL ANSWER = 'y'
        DISPLAY " "  *> have an extra new line
        END-DISPLAY
        DISPLAY "Password of " N-CHAR-IN " chars OK? 'y' or integer >= 8: " WITH NO ADVANCING
        END-DISPLAY

        ACCEPT ANSWER-STR
        END-ACCEPT

        IF ANSWER-STR(1:38) = 'y'  *> (1:38) to prevent 'yy' as a valid answer
            MOVE N-CHAR-IN TO N-CHAR
            MOVE 'y' TO ANSWER
        ELSE
            MOVE FUNCTION NUMVAL(ANSWER-STR) TO N-CHAR-OUT
            IF FUNCTION TRIM(ANSWER-STR) NUMERIC AND N-CHAR-OUT >= 8 AND N-CHAR-OUT <= 99
            *> FUNCTION TRIM(ANSWER-STR) NUMERIC is the secret to success here!!
            *> ANSWER-STR NUMERIC alone is not working, because of the 38 char field of ANSWER-STR filled with spaces!
            *> The difficulty here is that the "real" length of ANSWER-STR is not known before.
            *>
                MOVE N-CHAR-OUT TO N-CHAR
                MOVE 'y' TO ANSWER
            ELSE
                DISPLAY "enter an integer number 8 <= i <= 99 or 'y'"
                END-DISPLAY
            END-IF
        END-IF
    END-PERFORM.
    *> DISPLAY "N-CHAR = " N-CHAR  *> for testing
    *> END-DISPLAY  *> for testing


    MOVE 'N' TO ANSWER
    PERFORM UNTIL ANSWER = 'y'
        DISPLAY " "  *> have an extra new line
        END-DISPLAY
        DISPLAY "Do you want me to use special characters like .;,+*... ? 'y' or 'n': " WITH NO ADVANCING
        END-DISPLAY

        ACCEPT ANSWER-STR
        END-ACCEPT

        IF ANSWER-STR(1:38) = 'y'  *> (1:38) to prevent 'yy' as a valid answer
            MOVE 'y' TO ANSWER
        ELSE
            MOVE 'N' TO WITH-SPECIAL-CHARS
            MOVE 'y' TO ANSWER
        END-IF
    END-PERFORM.
    *> DISPLAY "WITH-SPECIAL-CHARS = " WITH-SPECIAL-CHARS  *> for testing
    *> END-DISPLAY  *> for testing


    IF WITH-SPECIAL-CHARS = 'y'
        PERFORM VARYING I FROM 34 BY 1 UNTIL I > 127  *> I is an ASCII code
            COMPUTE STR-INDEX = I - 33
            END-COMPUTE
            MOVE FUNCTION CHAR(I) TO CHAR-SET(STR-INDEX:1)
        END-PERFORM
    ELSE
        PERFORM VARYING I FROM 49 BY 1 UNTIL I > 59   *> 0..9
            COMPUTE STR-INDEX = I - 48
            END-COMPUTE
            MOVE FUNCTION CHAR(I) TO CHAR-SET(STR-INDEX:1)
        END-PERFORM
        PERFORM VARYING I FROM 66 BY 1 UNTIL I > 92   *> A..Z
            COMPUTE STR-INDEX = I - 65 + 10
            END-COMPUTE
            MOVE FUNCTION CHAR(I) TO CHAR-SET(STR-INDEX:1)
        END-PERFORM
        PERFORM VARYING I FROM 98 BY 1 UNTIL I > 123  *> a..z
            COMPUTE STR-INDEX = I - 97 + 10 + 26
            END-COMPUTE
            MOVE FUNCTION CHAR(I) TO CHAR-SET(STR-INDEX:1)
        END-PERFORM

        *> fill the remaining positions with 'A' chars to not have space chars ' ' here,
        *> which may be selected in case of no special chars!
        PERFORM VARYING STR-INDEX FROM 63 BY 1 UNTIL STR-INDEX > 94
            MOVE 'A' TO CHAR-SET(STR-INDEX:1)
        END-PERFORM
    END-IF
    *> DISPLAY "CHAR-SET =" CHAR-SET  *> for testing
    *> END-DISPLAY  *> for testing


    MOVE 1 TO I  *> char counter for the password, starting at index 1

    PERFORM UNTIL I > N-CHAR
        MOVE X-ELEMENT(J) TO TEMP-VAL
        *> DISPLAY " "  *> for testing
        *> END-DISPLAY  *> for testing
        *> DISPLAY "TEMP-VAL = " TEMP-VAL  *> for testing
        *> END-DISPLAY  *> for testing

        PERFORM CONVERT-TO-BINARY  *> result in BITS-X-STR
        *> DISPLAY "BITS-X-STR = " BITS-X-STR  *> for testing
        *> END-DISPLAY  *> for testing

        MOVE BITS-X-STR(1:8) TO BIN0-0
        MOVE BITS-X-STR(9:8) TO BIN0-1
        *> DISPLAY "BIN0-0 = " BIN0-0  *> for testing
        *> END-DISPLAY  *> for testing
        *> DISPLAY "BIN0-1 = " BIN0-1  *> for testing
        *> END-DISPLAY  *> for testing

        MOVE BIN0-0 TO INP-STR
        PERFORM BINARY-STR-TO-UNSIGNED-INT  *> result in OUT-NBR
        *> DISPLAY "OUT-NBR = " OUT-NBR  *> for testing
        *> END-DISPLAY  *> for testing
        MOVE FUNCTION CHAR(OUT-NBR + 1) TO CHAR0
        *> DISPLAY "CHAR0 =" CHAR0  *> for testing
        *> END-DISPLAY  *> for testing

        MOVE BIN0-1 TO INP-STR
        PERFORM BINARY-STR-TO-UNSIGNED-INT  *> result in OUT-NBR
        *> DISPLAY "OUT-NBR = " OUT-NBR  *> for testing
        *> END-DISPLAY  *> for testing
        MOVE FUNCTION CHAR(OUT-NBR + 1) TO CHAR1
        *> DISPLAY "CHAR1 =" CHAR1  *> for testing
        *> END-DISPLAY  *> for testing


        MOVE 0 TO K  *> always reset the counter to zero before the INSPECT statement,
                     *> as it increments the existing value
        INSPECT CHAR-SET TALLYING K FOR ALL CHAR0
        IF K > 0
            MOVE CHAR0 TO PW-CHARS(I:1)
            COMPUTE I = I + 1
            END-COMPUTE
            *> DISPLAY "CHAR0 is part of CHAR-SET"  *> for testing
            *> END-DISPLAY  *> for testing
        END-IF

        MOVE 0 TO K
        INSPECT CHAR-SET TALLYING K FOR ALL CHAR1
        IF K > 0 AND I < N-CHAR
            MOVE CHAR1 TO PW-CHARS(I:1)
            COMPUTE I = I + 1
            END-COMPUTE
            *> DISPLAY "CHAR1 is part of CHAR-SET"  *> for testing
            *> END-DISPLAY  *> for testing
        END-IF

        COMPUTE J = J + 1
        END-COMPUTE
    END-PERFORM

    DISPLAY " "
    END-DISPLAY
    IF N-CHAR < 10  *> skip the leading zero (or space char!)
        DISPLAY "Your password of " N-CHAR(2:1) " characters is: " FUNCTION TRIM(PW-CHARS)
        *> FUNCTION TRIM() for not showing probably trailing space chars,
        *> which may appear as a new line in the terminal! (which it isn't)
        END-DISPLAY
    ELSE
        DISPLAY "Your password of " N-CHAR " characters is: " FUNCTION TRIM(PW-CHARS)
        END-DISPLAY
    END-IF

    STOP RUN.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*>
*> user defined procedures:

CONVERT-TO-BINARY.
    *> Logic to extract 16 bits
    *> Initialize string with all '0's
    MOVE ALL "0" TO BITS-X-STR

    *> start from the last character (LSB) and work backwards
    PERFORM VARYING STR-INDEX FROM 16 BY -1
      UNTIL TEMP-VAL = 0 OR STR-INDEX = 0
        DIVIDE TEMP-VAL BY 2
            GIVING TEMP-VAL
            REMAINDER REM-BIN-VAL
        END-DIVIDE

        *> Move the digit ('0' or '1') to the specific position
        MOVE REM-BIN-VAL TO BITS-X-STR(STR-INDEX:1)
    END-PERFORM

    *> DISPLAY BITS-X-STR  *> for testing
    *> END-DISPLAY  *> for testing

    MOVE BITS-X-STR TO BITS-X(BIT-PTR:16)

    ADD 16 TO BIT-PTR
    END-ADD.  *> final . is essential here!


CONVERT-TO-HEX.
    *> Logic to extract 4 hex digits
    *> Initialize string with all '0's
    MOVE ALL "0" TO BITS-HEX-STR

    PERFORM VARYING STR-INDEX FROM 4 BY -1
      UNTIL TEMP-VAL = 0 OR STR-INDEX = 0
        DIVIDE TEMP-VAL BY 16
            GIVING TEMP-VAL
            REMAINDER REM-HEX-VAL
        END-DIVIDE

        MOVE HEX-DIGITS(REM-HEX-VAL + 1:1)  *> source: "0123456789abcdef". This is a very elegant solution!!
          TO BITS-HEX-STR(STR-INDEX:1)
    END-PERFORM

    *> DISPLAY BITS-HEX-STR  *> for testing
    *> END-DISPLAY  *> for testing
    *> DISPLAY " "  *> for testing
    *> END-DISPLAY  *> for testing

    MOVE BITS-HEX-STR TO BITS-HEX(HEX-PTR:4)

    ADD 4 TO HEX-PTR
    END-ADD.  *> final . is essential here!


BINARY-STR-TO-UNSIGNED-INT.
    MOVE 0 TO OUT-NBR
    PERFORM VARYING K FROM 1 BY 1 UNTIL K > LENGTH OF INP-STR
        IF INP-STR(K:1) = "1"
            COMPUTE OUT-NBR = OUT-NBR * 2 + 1
            END-COMPUTE
        ELSE
            COMPUTE OUT-NBR = OUT-NBR * 2
            END-COMPUTE
        END-IF
    END-PERFORM.

*> end of user defined procedures
*>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

END PROGRAM FLEXIBLE-PASSWORD-GENERATOR.
