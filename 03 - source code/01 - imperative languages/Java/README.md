# Java

The source code of the Java version of the microbenchmark program was mostly transpiled with the help of "Big AI" from its [Haxe](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#haxe) source code.

This was not a very efficient transpilation, although the initially resulting Java program was working fully correctly after some (minor) corrections with additional prompts. Partly, because initially the Haxe source code, initially transpiled from the (usual) Groovy program, wasn't the best source itself. For example, it implemented one more user defined function [isStrictPositiveInteger]() than actually necessary. In Java, that piece of code looks concisely like this:

```
            String answer_str = scanner.nextLine().trim();  // nextLine() allows the evaluation of the full string

            if (answer_str.equals("y")) {
                answer = true;
            } else {
                if (answer_str.matches("\\d+")) {  // Google AI: checks if the string contains only digits:
                                                   //            this is a simple and elegant solution!
                    N_CHAR = Integer.parseInt(answer_str);
                    ...
                } ...
            }
```

Only then I realized the same pattern matching idea, again after asking some "Big AI" systems for the "best" solution, in the Haxe program version:

```
            var answer_str = Sys.stdin().readLine();  // answer_str is just the text, no newline (2026-05-18)

            if (answer_str == "y") {
                answer = true;
            } else {
                var digitRegex = ~/^\d+$/;  // using Haxe's native regular expression class, EReg (Google AI); 2026-05-19
                if (digitRegex.match(answer_str)) {  
                    var parsed = Std.parseInt(answer_str);
                    ...
                } ...
            }
```

However, and as this background at ["Java"](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05e%20-%20Haxe#java) tries to explain,
the idea here was not to find the best source language to transpile into the most efficient Java source code.

This "Big AI" based transpilation from one Haxe source code file into only **one** Java source code file shows that Haxe is still a language "without (too) many batteries included" with its now two user defined functions, while [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy), my source language for numerous transpilations, needs none, same like Java:

language | source lines of code of full program (manually improved) | number of user defined functions in full program | program execution time of "speed part" of microbenchmark | comment
--- | --- | --- | --- | ---
Haxe    | 123  | 2 | 152 milliseconds with bytecode for the (modern) HashLink virtual machine | language for "cross-platform development"
Groovy  | 99   | 0 | 341 milliseconds with an uberJAR file for the Java virtual machine | my preferred source language for transpilations
Java    | 107  | 0 | 51 milliseconds with an uberJAR file for the Java virtual machine | development target: having only one source code file in target language Java

<br/>

#### The originally extra user defined function in Haxe

This user defined function has become redundant after noticing Java's simple and elegant solution:

```
    // Function to check if a value is strictly an integer (digits only)
    private static boolean isStrictPositiveInteger(String input) {
        if (input == null) return false;
        String str = input.trim();
        if (str.matches("^\\d+$")) {
            try {
                Integer.parseInt(str);
                return true;
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return false;
    }
```

<br/>

##_end
