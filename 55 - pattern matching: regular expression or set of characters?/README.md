2026-05-23: work in progress

tbd

Background of the page is this big refactoring job: [2026-05-22: another round of refactoring](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/wiki/Further-advances#2026-05-22-another-round-of-refactoring)

<br/>

# Pattern matching: (POSIX compliant) regular expressions or a set or hash of characters or single character strings, respectively?

TL;DR: get to me the [The Pattern matching list](#the-pattern-matching-list) below!

<br/>

See at [POSIX compliant regular expressions](https://pubs.opengroup.org/onlinepubs/9799919799/): basic regular expression (BRE) and extended regular expression (ERE)

[Regular Expressions (Regex) Character Classes Cheat Sheet](https://www.petefreitag.com/cheatsheets/regex/character-classes/)

<br/>

### POSIX character classes

> [!CAUTION]
> The danger with working with POSIX class _[[:print:]]_, that is all "printable" (ASCII) characters, lies in the fact that in one or the other language it may include the space character (decimal number 32), something which is not desired in the microbenchmark program!

For example, in C++:

```
#include <regex>
static const regex print_re("[[:print:]]");
```

..variable _print_re_ also covers the space character, but not in the [GNU Smalltalk solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01a%20-%20object-oriented%20languages/Smalltalk/random_bitstring_and_flexible_password_generator.st)!

<br/>

### Unicode characters

> [!CAUTION]
> Another potential pitfall are Unicode characters!

For example in [Scala](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala#scala), the pattern matching class [scala.util.matching.Regex](https://www.scala-lang.org/api/current/scala/util/matching/Regex.html) delegates to Java package [java.util.regex](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/package-summary.html). Java's handling of regular expressions may implicitly expect Unicode characters (while the microbenchmark program strictly adheres to ASCII characters), for example for this POSIX character class:

```
\p{Alnum} 	An alphanumeric character:[\p{IsAlphabetic}\p{IsDigit}]
```

..from Java page [Class Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html):

> The following Predefined Character classes and POSIX character classes are in conformance with the recommendation of Annex C: Compatibility Properties of Unicode Regular Expression , when UNICODE_CHARACTER_CLASS flag is specified. 

For the [Scala program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/random_bitstring_and_flexible_password_generator.scala), this means that this code as such won't work:

```
      val alnum_re: Regex = "[[:alnum:]]".r
```

..but this one:

```
      val alnum_re: Regex = """[A-Za-z0-9]""".r
```

However again, in [GNU Smalltalk](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01a%20-%20object-oriented%20languages/Smalltalk#gnu-smalltalk) for example, POSIX character class _[[:alnum:]]_ works fine with ASCII characters!

<br/>

Here's a little Java program to set and retrieve the state of Java flag _UNICODE_CHARACTER_CLASS_:

```
// AskStateOfFlagUNICODECHARACTERCLASS.java -- with help from Duck.ai
import java.util.regex.Pattern;
public class AskStateOfFlagUNICODECHARACTERCLASS {
    public static void main() {
        Pattern p1 = Pattern.compile("\\w+", Pattern.UNICODE_CHARACTER_CLASS);
        boolean enabled1 = (p1.flags() & Pattern.UNICODE_CHARACTER_CLASS) != 0;
        System.out.println("state of Java flag UNICODE_CHARACTER_CLASS = " + enabled1);
        
        Pattern p2 = Pattern.compile("\\w+");
        boolean enabled2 = (p2.flags() & Pattern.UNICODE_CHARACTER_CLASS) != 0;
        System.out.println("state of Java flag UNICODE_CHARACTER_CLASS = " + enabled2);
    }
}
```

Run it like this:

```
$ java AskStateOfFlagUNICODECHARACTERCLASS.java
state of Java flag UNICODE_CHARACTER_CLASS = true
state of Java flag UNICODE_CHARACTER_CLASS = false
$
```

<br/>

### Regular expressions

> [!CAUTION]
> In compiled languages with manual memory management, take care of regular expression objects!

If not, these objects may lead to memory leaks after program exits: [Memory leak detection with Valgrind](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/15%20-%20memory%20leak%20detection%20with%20Valgrind/README.md#memory-leak-detection-with-valgrind)

<br/>

## The Pattern matching list

programming language | regular expressions with variable _pattern_ used? | set/hash of characters/single character strings with variable _char_set_ used? | space character not included = OK | comments | POSIX or "near-POSIX" character classes used? | refactoring date
--- | --- | --- | --- | --- | --- | ---
Ada | yes |  | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | | 2026-06-08
AssemblyScript |  | yes | OK | _char_set_ is a set of allowed characters compiled from codepoints | | 2026-05-24
Ballerina | yes |  | OK | non-POSIX patterns: \`[!-~]+\`, \`[A-Za-z0-9]+\` | | 2026-06-12
C | yes |  | OK | POSIX patterns: _"^[[:graph:]]+$"_, _"^[[:alnum:]]+$"_ | yes | 2026-06-17
C++ | yes |  | OK | POSIX patterns: _"[[:graph:]]"_, _"[[:alnum:]]"_ | yes | 2026-06-17
C3 |  |  | OK | _char_set_ is a string of allowed characters in both cases, partly compiled from codepoints | | 2026-06-10
C# | yes |  | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ as strings | | 2026-06-18
Chapel | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-06-15
Clojure | yes | | OK | non-POSIX patterns: _"[!-~]+"_, _"[A-Za-z0-9]+"_ | | 2026-05-29
COBOL (GnuCOBOL) |  |  | OK | _CHAR-SET_ is a string of allowed characters in both cases ("picture clauses" for holding characters), compiled from codepoints | | 2026-06-03
CoffeeScript | yes | | OK | non-POSIX patterns: _/^[!-~]$/_, _/^[A-Za-z0-9]$/_ | | 2026-05-26
Common Lisp | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-05-30
Crystal | yes |  | OK | non-POSIX patterns: _/[!-~]/_, _/[A-Za-z0-9]/_ | | 2026-06-16
D | yes |  | OK | non-POSIX patterns: _r"[!-~]"_, _r"[A-Za-z0-9]"_ | | 2026-06-13
Dart | yes |  | OK | non-POSIX patterns: _r'^[!-~]$'_, _r'^[A-Za-z0-9]$'_ | | 2026-06-17
Eiffel | yes |  | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | | 2026-06-09
Fortran (GNU) |  |  | OK | _CHAR_SET_ is a string of allowed characters in both cases, compiled from codepoints | | 2026-06-10
FreeBASIC |  |  | OK | _char_set_ is a string of allowed characters in both cases, partly compiled from codepoints (regular expressions are natively supported, but need some extra effort) | | 2026-06-14
Gleam | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-05-31
Go | yes |  | OK | POSIX patterns: _"[[:graph:]]+"_, _"[[:alnum:]]+"_ | yes | 2026-06-17
Groovy | yes | | OK | non-POSIX patterns: _'[!-~]'_, _'[A-Za-z0-9]'_ | | 2026-05-27
Haxe | yes | | OK | non-POSIX patterns used for target safety: _"[!-~]"_, _"[A-Za-z0-9]"_ | | 2026-05-27
Inko |  |  | OK | _char_set_ is a _StringBuffer_ of allowed characters, partly compiled from codepoints, partly filled from a string | | 2026-06-06
Java | yes | | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | | 2026-05-26
Julia | yes |  | OK | non-POSIX patterns: _r"[!-~]+"_, _r"[A-Za-z0-9]+"_ | | 2026-05-28
Kotlin | yes | | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | | 2026-05-26
Mercury |  |  | OK | _CharSet_ is a string of allowed characters in both cases, compiled from codepoints; the _lex_ library needs extra installation and is not integral part of the Mercury implementation: [Extra programs in the Mercury implementation](https://github.com/Mercury-Language/mercury/tree/6b031c1ec68260767cff8e334f2aeadc998293ba/extras#extra-programs-in-the-mercury-implementation) | | 2026-06-09
Mojo |  |  | OK | _char_set_ is a string of allowed characters in both cases, partly compiled from codepoints | | 2026-05-28
Nim | yes |  | OK | POSIX patterns: _"[[:graph:]]"_, _"[[:alnum:]]"_ | yes | 2026-06-17
OCaml | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-05-29
Odin | yes |  | OK | non-POSIX patterns: \`^[!-~]$\`, \`^[A-Za-z0-9]$\` | | 2026-06-13
Perl 5 | yes | | OK | non-POSIX patterns: _qr/[!-~]/_, _qr/[A-Za-z0-9]/_ | | 2026-05-25
PHP | yes | | OK | non-POSIX patterns: _'/^[!-~]$/'_, _'/^[A-Za-z0-9]$/'_ | | 2026-06-16
Picat |  |  | OK | _CharSet_ is a string of allowed characters, compiled from codepoints in both cases | | 2026-06-16
Pike | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$")_ | | 2026-06-18
PowerShell | yes |  | OK | non-POSIX patterns: _"^[!-~]+$"_, _"^[A-Za-z0-9]+$"_ | | 2026-05-26
Prolog, SWI | yes |  | OK | POSIX patterns: _"[[:graph:]]"_, _"[[:alnum:]]"_ | yes | 2026-06-18
Python | yes |  | OK | Python's built-in _re_ (Regular expression operations) module doesn't support the POSIX character class syntax | | 2026-05-24
Roc |  |  | OK | working with two user defined functions, _printable_chars_ and _alphanum_chars_, to provide filtering of allowed characters | | 2026-05-31
Ruby | yes |  | OK | POSIX patterns: _/\A[[:graph:]]+\z/_, _/\A[[:alnum:]]+\z/_ | yes | 2026-06-17
Rust | yes |  | OK | non-POSIX patterns: _r"^[!-~]$"_, _r"^[A-Za-z0-9]$"_ | | 2026-06-14
Scala | yes |  | OK | non-POSIX patterns: _"""[!-~]""".r_, _"""[A-Za-z0-9]""".r_ | | 2026-05-25
Scheme, Bigloo | yes |  | OK | POSIX patterns: _"^[[:graph:]]$"_, _"^[[:alnum:]]$"_ | yes | 2026-06-17
Scheme, Racket | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-05-30
Smalltalk (GNU) | yes |  | OK | _pattern_ with POSIX bracket groups _[[:print:]]_ and _[[:alnum:]]_ | yes | 2026-05-24
Standard ML | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | | 2026-05-30
Swift | yes |  | OK | non-POSIX patterns: _/[!-~]/_, _/[A-Za-z0-9]/_ | | 2026-06-10
Tcl | yes |  | OK | non-POSIX patterns: _{([!-~])}_, _{([A-Za-z0-9])}_ | | 2026-06-19
TypeScript | yes |  | OK | non-POSIX patterns: _/^[!-~]$/_, _/^[A-Za-z0-9]$/_ | | 2026-06-17
V | yes |  | OK | non-POSIX patterns: _r'^[!-~]$'_, _r'^[A-Za-z0-9]$'_ | | 2026-06-10
Zig |  |  | OK | _char_set_ is an _ArrayList_ of allowed UTF-8 characters (*), partly compiled from codepoints, partly filled from a string | | 2026-06-12
 
<br/>

(*) [UTF-8](https://en.wikipedia.org/wiki/UTF-8) = Unicode Transformation Format – 8-bit

<br/>

##_end
