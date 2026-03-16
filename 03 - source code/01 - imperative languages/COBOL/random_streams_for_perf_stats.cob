*> random_streams_for_perf_stats.cob
*>
*> 2026-03-09/10/12/14/16
*>
*> build on Ubuntu 24 LTS: $ cobc -F -W -x random_streams_for_perf_stats.cob -o random_streams_for_perf_stats_cobc
*>                                -F for free format to not use first 7 source line positions for sequence numbers used on punched cards !!
*>
*> run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_cobc
*>                         $ time ./random_streams_for_perf_stats_cobc  =>  real	0m0.431s <<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>
*> still these warnings when building (I didn't build GnuCOBOL from its sources):
*>   <command-line>: warning: "_FORTIFY_SOURCE" redefined
*>   <command-line>: note: this is the location of the previous definition
*>
*>
*> mostly transpiled from random_streams_for_perf_stats.groovy with Google AI
*>
*> $ cobc -V  # cobc (GnuCOBOL) 3.1.2.0
*>
*>
*> alternative (experimental) compiler:
*>   download pre-compiled ~.deb package from here: https://gitlab.cobolworx.com/COBOLworx/gcc-cobol/-/packages/6
*>   $ sudo dpkg -i gcobol-16_16.0.1.20260311-10a0db-ubu20_x86_64.deb
*>   $ gcobol --version
*>   gcobol (GCOBOL-16.0.1.20260311-10a0db-ubu20) 16.0.1 20260311 (experimental)
*>   ...
*>   $ gcobol -W -O3 random_streams_for_perf_stats.cob -o random_streams_for_perf_stats_gcobol
*>   $ time ./random_streams_for_perf_stats_gcobol  => real	0m0.737s <<<<<<<<<<<<<<<<<<<<<<<<<<<<


IDENTIFICATION DIVISION.
PROGRAM-ID. RANDOM-STREAMS-FOR-PERF-STATS.

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
01  BIT-RECORD        PIC X(1000000).
FD  FILE-BITS-HEX.
01  HEX-RECORD        PIC X(250000).


WORKING-STORAGE SECTION.
01  FILE-BITS-X-NAME    PIC X(20) VALUE "random_bitstring.bin".
01  FILE-BITS-HEX-NAME  PIC X(21) VALUE "random_bitstring.byte".

*> Constants
77  C-END             PIC 9(9)  VALUE 62501.  *> 62501 for exactly 1M binary digits
*> 77  C-END             PIC 9(9)  VALUE 10.  *> for testing
77  C-M               PIC 9(9)  VALUE 65521.  *> = 2^16 - 15
77  C-A               PIC 9(9)  VALUE 17364.
77  C-C               PIC 9(9)  VALUE 0.

*> Variables
01  WS-FS             PIC XX.  *> 00 = Success, others = Error

77  I                 PIC 9(9).
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

*> end of user defined procedures
*>
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

END PROGRAM RANDOM-STREAMS-FOR-PERF-STATS.
