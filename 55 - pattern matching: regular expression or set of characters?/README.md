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

When using POSIX character classes:

> [!CAUTION]
> The danger with working with POSIX class _[[:print:]]_, that is all "printable" (ASCII) characters, lies in the fact that in one or the other language it may include the space character (decimal number 32), something which is not desired in the microbenchmark program!

For example, in C++:

```
#include <regex>
static const regex print_re("[[:print:]]");
```

..variable _print_re_ also covers the space character, but not in the [GNU Smalltalk solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01a%20-%20object-oriented%20languages/Smalltalk/random_bitstring_and_flexible_password_generator.st)!

<br/>

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

## The Pattern matching list

programming language | regular expression with variable _pattern_ used? | set/hash of characters/single character strings with variable _char_set_ used? | space character not included = OK | comments | refactoring date
--- | --- | --- | --- | --- | ---
Ada | yes |  | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | 2026-06-08
AssemblyScript |  | yes | OK | _char_set_ is a set of allowed characters compiled from codepoints | 2026-05-24
C | yes |  | OK | mixed patterns: _"^[!-~]+$"_, _"^[[:alnum:]]\\+$"_ (POSIX) | 2026-05-25
C3 |  |  | OK | still no native support of regular expressions, but 3rd party [C3 Regex Library](https://github.com/cheese3660/regex.c3l#c3-regex-library) could be used for example => _char_set_ is a string of allowed characters in both cases, partly compiled from codepoints | 2026-06-10
C++ | yes |  | OK | mixed patterns: _R"([!-~])"_, _"[[:alnum:]]"_ (POSIX) | 2026-05-25
Clojure | yes | | OK | non-POSIX patterns: _"[!-~]+"_, _"[A-Za-z0-9]+"_ | 2026-05-29
COBOL (GnuCOBOL) |  |  | OK | _CHAR-SET_ is a string of allowed characters in both cases ("picture clauses" for holding characters), compiled from codepoints | 2026-06-03
CoffeeScript | yes | | OK | non-POSIX patterns: _/^[!-~]$/_, _/^[A-Za-z0-9]$/_ | 2026-05-26
Common Lisp | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | 2026-05-30
Eiffel | yes |  | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | 2026-06-09
Fortran (GNU) |  |  | OK | GNU Fortan has no native support of regular expressions, but the [regex_module](https://github.com/perazz/fortran-regex/tree/main#fortran-regex) could be used => _CHAR_SET_ is a string of allowed characters in both cases, compiled from codepoints | 2026-06-10
Gleam | yes | | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | 2026-05-31
Groovy | yes | | OK | non-POSIX patterns: _'[!-~]'_, _'[A-Za-z0-9]'_ | 2026-05-27
Haxe | yes | | OK | non-POSIX patterns used for target safety: _"[!-~]"_, _"[A-Za-z0-9]"_ | 2026-05-27
Inko |  |  | OK | _char_set_ is a _StringBuffer_ of allowed characters, partly compiled from codepoints, partly filled from a string | 2026-06-06
Java | yes | | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | 2026-05-26
Julia | yes |  | OK | non-POSIX patterns: _r"[!-~]+"_, _r"[A-Za-z0-9]+"_ | 2026-05-28
Kotlin | yes | | OK | non-POSIX patterns: _"[!-~]"_, _"[A-Za-z0-9]"_ | 2026-05-26
Mercury |  |  | OK | _CharSet_ is a string of allowed characters in both cases, compiled from codepoints; the _lex_ library needs extra installation and is not integral part of the Mercury implementation: [Extra programs in the Mercury implementation](https://github.com/Mercury-Language/mercury/tree/6b031c1ec68260767cff8e334f2aeadc998293ba/extras#extra-programs-in-the-mercury-implementation) | 2026-06-09
Mojo |  |  | OK | still no native support of regular expressions => _char_set_ is a string of allowed characters in both cases, partly compiled from codepoints | 2026-05-28
Nim | yes |  | OK | mixed patterns: _re"([!-~])"_, _re"[[:alnum:]]"_ (POSIX) | 2026-05-28
OCaml | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | 2026-05-29
Perl 5 | yes | | OK | non-POSIX patterns: _qr/[!-~]/_, _qr/[A-Za-z0-9]/_ | 2026-05-25
PowerShell | yes |  | OK | non-POSIX patterns: _"^[!-~]+$"_, _"^[A-Za-z0-9]+$"_ | 2026-05-26
Python | yes |  | OK | Python's built-in _re_ (Regular expression operations) module doesn't support the POSIX character class syntax | 2026-05-24
Roc |  |  | OK | working with two user defined functions, _printable_chars_ and _alphanum_chars_, to provide filtering of allowed characters | 2026-05-31
Ruby | yes |  | OK | mixed patterns: _/\A[!-~]+\z/_, _/\A[[:alnum:]]+\z/_ (POSIX) | 2026-05-26
Scala | yes |  | OK | non-POSIX patterns: _"""[!-~]""".r_, _"""[A-Za-z0-9]""".r_ | 2026-05-25
Scheme, Bigloo | yes |  | OK | mixed patterns: _"^[!-~]$"_, _"^[[:alnum:]]$"_ (POSIX) | 2026-05-31
Scheme, Racket | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | 2026-05-30
Smalltalk (GNU) | yes |  | OK | _pattern_ with POSIX bracket groups _[[:print:]]_ and _[[:alnum:]]_ | 2026-05-24
Standard ML | yes |  | OK | non-POSIX patterns: _"^[!-~]$"_, _"^[A-Za-z0-9]$"_ | 2026-05-30
V | yes |  | OK | non-POSIX patterns: _r'^[!-~]$'_, _r'^[A-Za-z0-9]$'_ | 2026-06-10

<br/>

##_end
