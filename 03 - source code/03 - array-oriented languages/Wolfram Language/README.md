2025-10-30: work in progress

# Wolfram Language

https://www.wolfram.com/language/

---

Table of contents:

- [Installation tips for the Wolfram Engine](#installation-tips-for-the-wolfram-engine)
- [Execution speed](#execution-speed)
- [Pause[] function to prevent output races](#pause-function-to-prevent-output-races)
- [Error Handling](#error-handling)
<br/>

---

### Installation tips for the Wolfram Engine

The free for private use and locale Wolfram Engine is also used for executing **WolframScripts** (_~.wls_): https://www.wolfram.com/wolframscript/

> [!WARNING]
> The Wolfram Engine, packed into a Bash installation script (_~.sh_), cannot be installed in latest **Ubuntu** version 25.10 as of 2025-10-30!

This is also true for older versions of the Wolfram Engine than latest version 14.3 (as of 2025-10-30): https://www.wolfram.com/engine/

(Ubuntu version 25.10 with its _Rust Coreutils_ has some major problems apparently: https://www.phoronix.com/news/Ubuntu-25.10-Coreutils-Makeself)

<br/>

However, the installation in **Ubuntu 24 LTS**, as used in this benchmarking environment, worked (with me): 

```
$ chmod 755 ./WolframEngine_14.3.0_LIN.sh
$ sudo bash ./WolframEngine_14.3.0_LIN.sh
-----------------------------------------------------------------------------------------
                                 Wolfram Engine 14.3 Installer 
-----------------------------------------------------------------------------------------
...
Installation complete.

$ wolframscript
The Wolfram Engine requires one-time activation on this computer.
...
In[1]:= Exit[]
$
```

### Execution speed

Since I haven't found any string builder in this language (https://reference.wolfram.com/language/guide/StringOperations.html), this script is a bit on the slow side with a mean of **1546 milliseconds** execution time over 20 runs, so I had to put it on my [Languages that were too slow list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#wolfram-language).

This is already my [optimized version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Wolfram%20Language/random_streams_for_perf_stats.wls), where I don't do plain string concatenation (_bitsx = bitsx <> StringPadLeft[IntegerString[x[[i]], 2], 16, "0"];_), but filling an initialized array of fixed size with the little strings in the master loop, which finally will be joined together into one big string: _bitsxtotal   = StringJoin[bitsx]_

> [!NOTE]
> Be aware that the '_' character has special meaning in Wolfram Language ("blank" for any single character) and so I dropped it from my variable names: https://reference.wolfram.com/language/tutorial/Patterns.html#139

The original version with string concatenation was about five times slower.

### Pause[] function to prevent output races

The _Pause[]_ function of the Wolfram Language is a real hit: https://reference.wolfram.com/language/ref/Pause.html

After elaborate experimentation, I found out that this function is preventing an adverse output race between print commands _InputString["\nPassword ..."];_ and _Print["enter ..."]_:

```
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

```

Without _Pause[0.65];_, the next prompt for user input after some invalid user input would be constantly displayed **before** error message _enter an integer number >= 8 or 'y'_!

### Error Handling

(TBD)

<br/>

##_end

