# FreeBASIC

https://www.freebasic.net/

---

### On BASIC

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

### Installation tips

At least in Ubuntu, make sure that also _**libtinfo5**_ is installed in your system to make FreeBASIC working: https://askubuntu.com/questions/1531760/how-to-install-libtinfo5-on-ubuntu24-04

### Other functionalities of FreeBASIC

You may have a look into the _./FreeBASIC-1.10.1-linux-x86_64/examples_ directory after installation to see FreeBASIC's scope of functionalities, but also its capabilities to interact with other programming languages and potentially its GUI capabilities. A look into directory _./FreeBASIC-1.10.1-linux-x86_64/./include/freebasic_ shows bindings for many different libraries.

<br/>

### Other BASIC dialects

I also tapped into other implementations of BASIC, like:

- **CrossBasic** (**Xojo** clone, Ex-REALbasic): https://github.com/simulanics/CrossBasic
- **GNU Gambas 3**: https://gambaswiki.org/website/en/main.html#
- **QB64** (**OuickBASIC** derivative): https://github.com/DualBrain/QB64

However, after installations and a little playing I can claim: there's an almost 100% chance that these implementations cannot take even a little source code for FreeBASIC unchanged. Therefore I skipped all other BASIC dialects.

<br/>

##_end
