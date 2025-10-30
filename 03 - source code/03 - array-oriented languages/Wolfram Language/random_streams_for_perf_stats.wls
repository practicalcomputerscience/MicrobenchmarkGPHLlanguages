(* random_streams_for_perf_stats.wls

A Wolfram Language script for interpretation on the Wolfram Engine.

2025-10-30

run on Ubuntu 24 LTS: > wolframscript -f ./random_streams_for_perf_stats.wls
                      > ./exe_times_statistics_for_one_test_case_in_cwd2 "wolframscript -f" ./random_streams_for_perf_stats.wls
                      => mean = 1546 [milliseconds]


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
(*end = 11;  (* for testing *)*)
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

x[[1]] = RandomInteger[{1, end}];  (* end is inclusive*)
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

(* end of random_streams_for_perf_stats.wls *)
