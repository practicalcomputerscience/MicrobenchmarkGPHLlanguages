(* random_bitstring_and_flexible_password_generator.wls

A Wolfram Language script for interpretation on the Wolfram Engine.

2025-10-31, 2025-11-01

run on Ubuntu 24 LTS: > wolframscript -f ./random_bitstring_and_flexible_password_generator.wls


> wolframscript -version
WolframScript 1.13.0 for Linux x86 (64-bit)
>
*)


(***********************************************************************)
(*                                                                     *)
(* user defined functions                                              *)

(* Function to export data with exception handling: Duck.ai: works! *)
exportData[data_, filePath_, type_] := Module[
  {result},
  (* Try to export the data *)
  Quiet[
    (* Handling errors using Check *)
    Check[
      result = Export[filePath, data, "Text"],
      (* Error handling code *)
      Print["could not write to file: ", filePath, " ! -- ",
            ToString@First@$MessageList];
      Return[False]; (* Return False if error occurs *)
    ]
  ];

  (* Return True if successful *)
  If[result === $Failed,
    _;
    False,
    Print[type <> " stream has been written to disk under name: ", filePath];
    True
  ]
];

(* end of user defined functions                                       *)
(*                                                                     *)
(***********************************************************************)


end = 62501;  (* 62501 for exactly 1M binary digits *)
(*end = 15;  (* for testing *)*)
              (* Putting a semicolon at the end of the line tells the
                 Wolfram Language to show no output *)
(*m1   = END*16 - 16;
k250 = END*4 - 4;*)

m = 65521;  (* = 2^16 - 15 *)
a = 17364;
c = 0;

filebitsx   = "random_bitstring.bin";  (* no _ char in normal variable names => not file_bits_x here! *)
                                       (* x_: any expression, to be referred to as x *)
filebitshex = "random_bitstring.byte";

x = ConstantArray[0, end];
(* Print["x = ", x];  (* for testing: {0, 0, 0, 0, 0, 0, 0, 0, 0, 0} *) *)

x[[1]] = RandomInteger[{1, m}];  (* m is inclusive*)
(* Print["x = ", x]; (* for testing: {3, 0, 0, 0, 0, 0, 0, 0, 0, 0} *) *)
(* Print["x[[1]] = ", x[[1]]];  (* for testing array reading access: 3*) *)

(*bitsx = "";
bitshex = "";*)
bitsx   = ConstantArray["0000000000000000", end-1];
bitshex = ConstantArray["0000", end-1];


Print["\ngenerating a random bit stream..."];

For[i = 2, i <= end,
  {
    (* Print["i = ", i];  (* for testing *) *)
    x[[i]] = Mod[(a*x[[i-1]] + c),m];
    (* Print["x[[i]] = ", x[[i]]];  (* for testing *) *)

    (*bitsxstr = StringPadLeft[IntegerString[x[[i]], 2], 16, "0"];  (* MS Bing AI *)*)
    (* printing of bitsxstr does not work here! *)
    (* Print[StringPadLeft[IntegerString[x[[i]], 2], 16, "0"]];  (* for testing *) *)

    (* Print[StringPadLeft[IntegerString[x[[i]], 16], 4, "0"]]; (* for testing *) *)

    bitsx[[i-1]]   = StringPadLeft[IntegerString[x[[i]], 2], 16, "0"];
    bitshex[[i-1]] = StringPadLeft[IntegerString[x[[i]], 16], 4, "0"];

    i++;
  }
]

bitsxtotal   = StringJoin[bitsx]
bitshextotal = StringJoin[bitshex]

(*Print["bitsxtotal = ", bitsxtotal];  (* for testing *)*)
(*Print["bitshextotal = ", bitshextotal];  (* for testing *)*)


exportData[bitsxtotal, filebitsx, "Bit"];
exportData[bitshextotal, filebitshex, "Byte"];


(* # make a password of n_char printable chars *)
nchar = 12;  (* base case *)
answer = False;
While[answer === False,
  Pause[0.65];  (*0.65 seconds*)
  (* this pause, and nothing else, is the solution to prevent output races between the prompt and the error message! *)

  reply = InputString["\nPassword of " <> IntegerString[nchar] <> " printable chars OK? 'y' or another integer number >= 8: "];
  (* InputString[] is needed for "y" *)

  If[reply === "y",
    answer = True,
    {
      (* Try to convert to integer number *)
      Quiet[
        Check[
          result = Interpreter["Integer"][reply],  (* MS Bing AI; "9.0 is a valid answer here: OK with me *)
          (* Error handling code *)
          Print["enter an integer number >= 8 or 'y'"]  (* this line is essential *)
        ]
      ]

      (* Return new nchar if successful *)
      If[FailureQ[result],
        Print["enter an integer number >= 8 or 'y'"],
        If[result < 8,
          Print["enter an integer number >= 8 or 'y'"],
          nchar = result;
          answer = True;
        ]
      ]
    }
  ]
]
(* Print["nchar = ", nchar];  (* for testing *) *)


withspecialchars = True
answer = False
While[answer === False,
    Pause[0.65];
    reply = InputString["\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "];
    If[reply === "y",
        answer = True,
        withspecialchars = False;
        answer = True
    ]
]
(* Print["withspecialchars = ", withspecialchars];  (* for testing *) *)

If[withspecialchars === True,
    charset = "[!-~]",
    (* https://reference.wolfram.com/language/tutorial/WorkingWithStringPatterns.html#182364929 *)
    (* charset is later used as a regular expression: *)
    (* https://reference.wolfram.com/language/ref/RegularExpression.html *)
    charset = "[A-Z]|[a-z]|[0-9]"
]
(* Print["charset = ", charset];  (* for testing *) *)


i = 0;  (* char counter for the password *)
j = 1;  (* counter for x *)
pwchars = "";

While[i < nchar,
  bin0 = StringPadLeft[IntegerString[x[[j]], 2], 16, "0"];
  (* Print["bin0 = ", bin0]; *)

  bin00 = StringTake[bin0,8];
  bin01 = StringTake[bin0,{8,16}];
  (* Print["bin00 = ", bin00]; *)
  (* Print["bin01 = ", bin01]; *)

  char0 = FromCharacterCode[FromDigits[bin00, 2]];
  char1 = FromCharacterCode[FromDigits[bin01, 2]];
  (* Print["char0 = ", char0]; *)
  (* Print["char1 = ", char1]; *)

  If[StringMatchQ[char0, RegularExpression[charset]],
    {
      pwchars = pwchars <> char0;
      i++;
      If[i === nchar, Break[]]
    }
  ]

  If[StringMatchQ[char1, RegularExpression[charset]],
    {
      pwchars = pwchars <> char1;
      i++;
    }
  ]

  j++;
]

Print["\nYour password of " <> IntegerString[nchar] <> " characters is: " <> pwchars];

(* end of random_bitstring_and_flexible_password_generator.wls *)

