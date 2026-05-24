2026-05-23: work in progress

tbd

Background of the page is this big refactoring job: [2026-05-22: another round of refactoring](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/wiki/Further-advances#2026-05-22-another-round-of-refactoring)

<br/>

# Pattern matching: (POSIX compliant) regular expressions or a set or hash of characters or single character strings, respectively?

See at [POSIX compliant regular expressions](https://pubs.opengroup.org/onlinepubs/9799919799/): basic regular expression (BRE) and extended regular expression (ERE)

[Regular Expressions (Regex) Character Classes Cheat Sheet](https://www.petefreitag.com/cheatsheets/regex/character-classes/)

<br/>

programming language | regular expression with variable _pattern_ used? | set/hash of characters/single character strings with variable _char_set_ used? | comments
--- | --- | --- | ---
AssemblyScript |  | yes | _char_set_ is a set of characters compiled from codepoints
Smalltalk (GNU) | yes |  | _pattern_ with POSIX bracket groups _[[:print:]]_ and _[[:alnum:]]_

<br/>

##_end
