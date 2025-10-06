# Perl 5

https://www.perl.org/

---

### Error handling when writing to files

As a last minute change I gave up on the experimental _try/catch_ construct of Perl 5 (https://perldoc.perl.org/perlsyn#Try-Catch-Exception-Handling) and switched to a conventional _if-then-else_ construct with warning messaging and quality flags.

Now the exception behavior, though more verbose in code and user experience, is more convincing to me.

Though, I have not tested _try/catch_ with latest version Perl 5.42 as of September 2025: https://perldoc.perl.org/perllinux 

### On Perl 5 environments

Here, I refer to chaper [On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#on-configuring-building-and-execution-environments) of the main page:

> Some versions were already a bit outdated when I started this project; for example with Perl in version 5.38.2, which just came with Ubuntu 24 LTS. This is an example where I don't want to break things potentially.

<br/>

##_end
