# C#

https://learn.microsoft.com/en-us/dotnet/csharp/

<br/>

See the related language description slide [Part 3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/01%20-%20presentation%20slides/README.md#part-3) for what (good) things I say about C#.

---

#### Installation tips

Both source code files, that is _random_bitstring_and_flexible_password_generator.cs_ and _random_streams_for_perf_stats.cs_, must have name _Program.cs_ in their related project directories. (Here they are just named differently to store them in the same GitHub directory.)

Otherwise, I installed .NET just with command: _$ sudo apt install dotnet8_

After that, check versions with commands:

```
$ dotnet --list-runtimes
$ dotnet --list-sdks
```

#### Virtual machine?

Is a C# program being executed on a virtual machine?

I would say yes. [Wikipedia](https://en.wikipedia.org/wiki/Common_Language_Runtime) says:

> The Common Language Runtime (CLR), the virtual machine component of Microsoft .NET Framework, manages the execution of .NET programs. Just-in-time compilation converts the managed code (compiled intermediate language code) into machine instructions which are then executed on the CPU of the computer.

Though, Microsoft apparently avoids the term "virtual machine", at least here: https://learn.microsoft.com/en-us/dotnet/standard/clr?redirectedfrom=MSDN, where the term "virtual execution system" is used.

##_end
