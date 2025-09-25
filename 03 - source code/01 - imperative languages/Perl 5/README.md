# Perl 5

https://www.perl.org/

---

### Error handling when writing to files

As a last minute change I gave up on the experimental _try/catch_ construct of Perl 5 (https://perldoc.perl.org/perlsyn#Try-Catch-Exception-Handling) and switched to use a conventional _if-then-else_ construct with warning messaging and quality flags.

Now the exception behavior, though more verbose in code and user experience, is more convincing to me.

Though, I have not tested _try/catch_ with latest version 5.42. I'm still using the inbuilt version 5.38.2 as shipped with Ubuntu 24 LTS.

### On Perl 5 environments

Here, I refer to the main page: https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#on-configuring-building-and-execution-environments

So, for the given reason I'm not using the latest Perl 5 version, which is now 5.42.0: https://perldoc.perl.org/perllinux

##_end
