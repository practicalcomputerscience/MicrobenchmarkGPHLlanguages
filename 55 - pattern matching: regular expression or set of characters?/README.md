2026-05-23: work in progress

tbd

Background of the page is this big refactoring job: [2026-05-22: another round of refactoring](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/wiki/Further-advances#2026-05-22-another-round-of-refactoring)

<br/>

# Pattern matching: (POSIX compliant) regular expressions or a set or hash of characters or single character strings, respectively?

See at [POSIX compliant regular expressions](https://pubs.opengroup.org/onlinepubs/9799919799/): basic regular expression (BRE) and extended regular expression (ERE)

[Regular Expressions (Regex) Character Classes Cheat Sheet](https://www.petefreitag.com/cheatsheets/regex/character-classes/)

<br/>

When using POSIX character classes:

> [!CAUTION]
> The danger with working with POSIX class _[[:print:]]_, that is all "printable" (ASCII) characters, lies in the fact that in one or the other language it may include the space character (decimal number 32), something which is not desired in this program!

For example, in C++:

```
#include <regex>
static const regex print_re("[[:print:]]");
```

..variable _print_re_ contains the space character!

<br/>

programming language | regular expression with variable _pattern_ used? | set/hash of characters/single character strings with variable _char_set_ used? | space character not included = OK | comments | refactoring date
--- | --- | --- | --- | --- | ---
AssemblyScript |  | yes | OK | _char_set_ is a set of characters compiled from codepoints | 2026-05-24
C | yes |  | OK | mixed patterns: _"^[!-~]+$"_, _"^[[:alnum:]]\\+$"_ (POSIX) | 2026-05-25
C++ | yes |  | OK | mixed patterns: _R"([!-~])"_, _"[[:alnum:]]"_ (POSIX) | 2026-05-25
Perl 5 | yes | | OK | non-POSIX patterns: _qr/[!-~]/_, _qr/[A-Za-z0-9]/_ | 2026-05-25
Python | yes |  | OK | Python's built-in _re_ (Regular expression operations) module doesn't support the POSIX character class syntax | 2026-05-24
Scala | yes |  | OK | non-POSIX patterns: _"""[!-~]""".r_, _"""[A-Za-z0-9]""".r_ | 2026-05-25
Smalltalk (GNU) | yes |  | OK | _pattern_ with POSIX bracket groups _[[:print:]]_ and _[[:alnum:]]_ | 2026-05-24

<br/>

##_end
