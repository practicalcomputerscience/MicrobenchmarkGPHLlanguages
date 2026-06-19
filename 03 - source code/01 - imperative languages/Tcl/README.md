# Tcl

https://www.tcl-lang.org/

TCL (originally "Tool Command Language") influenced the design of
[PHP](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05c%20-%20PHP%20on%20the%20Zend%20Engine%20virtual%20machine#php)
and [PowerShell](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/PowerShell#powershell),
and thus should be included in my list of tested languages.

I also tested latest version 9.0.3 (as of 2026-06-19), along with long term version 8.6, which ships with many Linux distributions out of the box.

I compiled this new version from sources (https://www.tcl-lang.org/software/tcltk/download.html) (on a different platform), only to notice that it's about 8% slower than established version 8.6
at the speed part of my microbenchmark program: [random_streams_for_perf_stats.tcl](tbd) (still with _set_ instead of _list_ for the random integer numbers)

Tcl version 9 looks like a significant step in language modernization: https://www.tcl-lang.org/software/tcltk/9.0.html

I think that Tcl is an interesting alternative to [Perl 5](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Perl%205#perl-5) as a scripting language. 

<br/>

##_end
