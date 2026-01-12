# PowerShell

https://learn.microsoft.com/en-us/powershell/?view=powershell-7.5

<br/>

PowerShell is an underrated programming language in my opinion, even though it's not the fastest, though it's not super-slow either: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

> [!TIP]
> Thanks to the vast .NET framework, this was the easiest and fastest language to implement my microbenchmark program.

For scripting tasks I don't mind if a language is a bit slower than others. I wonder if Microsoft did this by design, at least for Linux.

<br/>

For the question: "Is a Powershell script being executed on a **virtual machine**?", I would refer to this mini-chapter in [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#virtual-machine) and say "yes" again.

Microsoft says [here](https://learn.microsoft.com/en-us/powershell/scripting/lang-spec/chapter-01?view=powershell-7.5):

> Unlike most shells, which accept and return text, Windows PowerShell is built on top of the .NET Framework common language runtime (CLR) and the .NET Framework, and accepts and returns .NET Framework objects.

<br/>

### Installation tips

For installation in Ubuntu, I followed this procedure: https://learn.microsoft.com/en-us/powershell/scripting/install/install-ubuntu?view=powershell-7.5

<br/>

2026-01-11/12:

### Performance tips

See from [PowerShell scripting performance considerations](https://learn.microsoft.com/en-us/powershell/scripting/dev-cross-plat/performance/script-authoring-considerations?view=powershell-7.5):

> Many of the techniques described here aren't idiomatic PowerShell and may reduce the readability of a PowerShell script. Script authors are advised to use idiomatic PowerShell unless performance dictates otherwise.

I had a look at the listed techniques, and one other (that is _Generic.List_), and made some more speed tests:

construct | comment | real execution time (1 program run only)
--- | --- | ---
$bits_x = @() | start with an empty array | real	0m11.649s
$bits_x = New-Object System.Collections.Generic.List[string] | needs final conversion into an array and then string | real	0m0.729s
$bits_x = [System.Text.StringBuilder]::new() | my final solution | real  0m0.573s
$bits_x = @("----------------") * $END | pre-allocation of an array of strings; needs final conversion into a string | real	0m0.706s

Result: with this microbenchmark program, using the Power Shell string builder clearly remains the fastest solution, which is not the case in this article, where building an array of joined strings is much faster (on a Windows 11 machine in PowerShell 7.4.2; while I'm on a Ubuntu 24 LTS machine in PowerShell 7.5.4).

Then, I run the program listed under chapter [String addition](https://learn.microsoft.com/en-us/powershell/scripting/dev-cross-plat/performance/script-authoring-considerations?view=powershell-7.5#string-addition) on my system -- and the Power Shell string builder is always the fastest solution:

```
$ pwsh ./string_addition_speed_test.ps

Iterations Test                   TotalMilliseconds RelativeSpeed
---------- ----                   ----------------- -------------
     10240 StringBuilder                     11.000 1x
     10240 Join operator                     11.250 1.02x
     10240 Addition Assignment +=           639.630 58.15x
     51200 StringBuilder                      7.850 1x
     51200 Join operator                     53.640 6.83x
     51200 Addition Assignment +=          5387.730 686.34x
    102400 StringBuilder                     13.740 1x
    102400 Join operator                     43.450 3.16x
    102400 Addition Assignment +=         25044.350 1822.73x

$ 
```

<br/>

By the way: I just cleaned up the "masterloop" a bit to have a more logical and efficient program. This may have helped to have a little bit faster program with the string builder solution now, but nothing which has "moved the needle".

<br/>

##_end
