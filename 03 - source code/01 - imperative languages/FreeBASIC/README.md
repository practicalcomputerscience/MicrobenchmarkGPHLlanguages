# FreeBASIC

https://www.freebasic.net/

---

### On BASIC

BASIC (for _Beginner's All-purpose Symbolic Instruction Code_) was historically a direct response to the state of FORTRAN in the 60ies:

> ...“almost impossible-to-memorize convention for specifying a loop: ‘DO 100, I = 1, 10, 2’. Is it ‘1, 10, 2’ or ‘1, 2, 10’, and is the comma after the line number required or not?”

from: https://time.com/69316/basic/

After a little survey beyond Microsoft's almighty **Visual Basic (.NET)** I discovered **FreeBASIC**: https://www.freebasic.net/, which by default, and without an IDE (Integrated development environment), compiles source code directly into standalone executables (in Linux): _$ fbc <my_program.bas>_

Into very fast executables, at least with this little accidental microbenchmark program, which puts FreeBASIC into the same speed league like Mojo for example. And this, same like Mojo, with simple string concatenation (even the _+_ operator is the same like in Mojo):

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

I'm happy with the fast execution speed of this program and thus didn't start to experiment with potentially other possibilities of big string concatenation; like not testing the ZSTRING type and leave it with the STRING type.

#### Installation tips

At least in Ubuntu make sure that also _**libtinfo5**_ is installed in your system to make FreeBASIC working: https://askubuntu.com/questions/1531760/how-to-install-libtinfo5-on-ubuntu24-04

#### Other functionalities of FreeBASIC

You may have a look into the _./FreeBASIC-1.10.1-linux-x86_64/examples_ directory after installation to see FreeBASIC's scope of functionalities, but also its capabilities to interact with other programming languages and potentially its GUI capabilities. A look into directory _./FreeBASIC-1.10.1-linux-x86_64/./include/freebasic_ shows bindings for many different libraries.

<br/>

### Other BASIC dialects

Then I also tapped into other implementations of BASIC, like:

- **CrossBasic** (**Xojo** clone, Ex-REALbasic): https://github.com/simulanics/CrossBasic
- **GNU Gambas 3**: https://gambaswiki.org/website/en/main.html#
- **QB64** (**OuickBASIC** derivative): https://github.com/DualBrain/QB64

However, after installations and a little playing I can say: there's an almost 100% chance that these implementations cannot take even a simple program written for FreeBASIC unchanged. Therefore I skip all other dialects of BASIC.

<br/>

##_end
