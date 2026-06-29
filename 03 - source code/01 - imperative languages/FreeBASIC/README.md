# FreeBASIC

https://www.freebasic.net/

---

Table of contents:

- [On BASIC](#on-basic)
- [Installation tips](#installation-tips)
- [Other functionalities of FreeBASIC](#other-functionalities-of-freebasic)
- [Other BASIC dialects](other-basic-dialects)
- [Regular Expressions in FreeBASIC](#regular-expressions-in-freebasic)
- [](#)

<br/>

---

## On BASIC

BASIC (for _Beginner's All-purpose Symbolic Instruction Code_) was historically a direct response to the state of FORTRAN in the 60ies:

> ...“almost impossible-to-memorize convention for specifying a loop: ‘DO 100, I = 1, 10, 2’. Is it ‘1, 10, 2’ or ‘1, 2, 10’, and is the comma after the line number required or not?”

from: https://time.com/69316/basic/

After a little survey beyond Microsoft's almighty **Visual Basic (.NET)** I discovered FreeBASIC, which by default and without an IDE (Integrated Development Environment), compiles source code directly into standalone executables (in Linux):

```
$ fbc <my_program.bas>
```

Into very fast executables! At least with this microbenchmark program: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

And doing so with [simple string concatenation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/FreeBASIC/random_streams_for_perf_stats.bas):

```
...
dim bits_x as string = ""
...
for i = 1 to upper_limit
...
  bits_x += bits_x_str
...
next
...
```

I'm happy with the fast execution speed of this program and thus didn't start to experiment with potentially other possibilities of a big string concatenation, like not testing the ZSTRING type and thus leaving it with the STRING type.

## Installation tips

Make sure that _**libtinfo5**_ is installed in your Linux system to make FreeBASIC working: https://askubuntu.com/questions/1531760/how-to-install-libtinfo5-on-ubuntu24-04

This means that the dynamically linked program depends also on that **shared library**, also being installed on the target machine. Install it like this for example if it's missing yet:

```
sudo dpkg -i ./libtinfo5_6.3-2ubuntu0.1_amd64.deb
```

FreeBASIC depends on _gcc_; see here for more information about this GNU compiler if there may be problems with it: [C and Checked C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#c-and-checked-c)

<br/>

## Other functionalities of FreeBASIC

You may have a look into the _./FreeBASIC-1.10.1-linux-x86_64/examples_ directory after installation to see FreeBASIC's scope of functionalities, but also its capabilities to interact with other programming languages and potentially its GUI capabilities. A look into directory _./FreeBASIC-1.10.1-linux-x86_64/./include/freebasic_ shows bindings for many different libraries.

<br/>

## Other BASIC dialects

I also tapped into other implementations of BASIC, like:

- **CrossBasic** (**Xojo** clone, Ex-REALbasic): https://github.com/simulanics/CrossBasic
- **GNU Gambas 3**: https://gambaswiki.org/website/en/main.html#
- **QB64** (**OuickBASIC** derivative): https://github.com/DualBrain/QB64

However, after installations and a little playing I can claim: there's an almost 100% chance that these implementations cannot take even a little FreeBASIC program unchanged. Therefore I skipped all other BASIC dialects.

<br/>

#### Regular Expressions in FreeBASIC

There are a couple of internal libraries for regular expressions for FreeBASIC, like for example:

```
$ ls ./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre* -1
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre16.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre2.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre2posix.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre32.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcre-common.bi
./FreeBASIC-1.10.1-linux-x86_64/include/freebasic/pcreposix.bi
$ 
```

See also the **external** "Lightweight, robust, and efficient POSIX compliant regexp matching library" [TRE](https://www.freebasic.net/wiki/ExtLibtre).

I successfully tested the _pcre.bi_ library, see from example source code file _pcredemo.bas_ in subdirectory _./FreeBASIC-1.10.1-linux-x86_64/examples/regex/PCRE_:

```
...
#include once "pcre.bi"  ' 2026-06-14, see from example source code file: pcredemo.bas
...

dim as string pattern = ""
if with_special_chars then
    ' Matches any character within ASCII 33 to 127
    pattern = "^[!-~]$"
else
    ' Matches any alphanumeric character (0-9, A-Z, a-z)
    pattern = "^[A-Za-z0-9]$"
end if

dim OVECCOUNT as const uinteger = 30  '' should be a multiple of 3
dim as pcre ptr re
dim as zstring ptr error_
dim as integer erroffset, ovector(OVECCOUNT-1), rc

'' compile the regular expression
re = pcre_compile(pattern, 0, @error_,  @erroffset, NULL)
/'
                  pattern    = the pattern
                  0          = default options
                  @error_    = for error message
                  @erroffset = for error offset
                  NULL       = use default character tables
'/
...

  rc = pcre_exec(re, NULL, char0, len( char0 ), 0, 0, @ovector(0), OVECCOUNT )
  /'                
                 re           = the compiled pattern
                 NULL         = no extra data - we didn't study the pattern
                 char0        = the subject string
                 len( char0 ) = the length of the subject
                 0            = start at offset 0 in the subject
                 0            = default options
                 @ovector(0)  = output vector for substring information
                 OVECCOUNT    = number of elements in the output vector
  '/
  if rc >= 0 then
    pw_chars += char0
    i += 1
  end if
...
```

As you can see, at least this solution really takes some extra effort compared to many other programming languages.

Here's the full program: [random_bitstring_and_flexible_password_generator_regex.bas](./random_bitstring_and_flexible_password_generator_regex.bas)

So, I decided to keep the original and slim solution based on a simple _instr(char_set, char0)_ function as my official solution.

The solution with regular expressions needs 7 more lines of source code.

<br/>

#### Static linking in FreeBASIC

Static linking in FreeBASIC:

```
$ fbc -static ./random_streams_for_perf_stats.bas
$ mv ./random_streams_for_perf_stats ./random_streams_for_perf_stats_stat
$ ldd ./random_streams_for_perf_stats_stat
	not a dynamic executable
$
```

..leads to no measurable execution speed advantage over normal dynamic linking according to my measurements:

```
$ fbc ./random_streams_for_perf_stats.bas
$ mv ./random_streams_for_perf_stats ./random_streams_for_perf_stats_dyn
$ ldd ./random_streams_for_perf_stats_dyn
	linux-vdso.so.1 (0x00007ba9302c4000)
	libtinfo.so.6 => /lib/x86_64-linux-gnu/libtinfo.so.6 (0x00007ba930271000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007ba930188000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007ba92fe00000)
	/lib64/ld-linux-x86-64.so.2 (0x00007ba9302c6000)
$
```

At least not at the "speed part" of the microbenchmark program.

FreeBASIC anyway features a relative high variance in its executions times of a program.

The size of the statically linked program however is over 21 times bigger than the size of the dynamically linked program: 1,244,456 bytes versus 57,768 bytes!

<br/>

##_end
