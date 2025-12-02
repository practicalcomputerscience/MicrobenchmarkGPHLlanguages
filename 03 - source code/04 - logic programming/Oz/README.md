2025-12-02: work in progress

# Oz

Development and execution environment **Mozart** (version 2): http://mozart2.org/

Oz cheat sheet from 2019: https://github.com/alhassy/OzCheatSheet/blob/master/CheatSheet.pdf

<br/>

At the moment, Oz, now in Version 3, and Mozart feel like a big, almost abandoned construction site with numerous dead links in the web,
here for example for Oz version 2: https://www.ps.uni-saarland.de/oz2/documentation/, or links to just outdated but still useful documentation (https://www.mozart-oz.org/documentation/);
useful since also current Mozart version 2.0.1 is still using many old parts.

Therefore, I often look into the GitHub repository to see what functions are currently available and how to use them, like here for [lists](https://github.com/mozart/mozart2/blob/83c83da2f670fbd1d08d4145eca3d88f1687582c/lib/main/base/List.oz) for example.

For Oz's **terminology** I took "A Tutorial of Oz 2.0" from 1996 by Seif Haridi: https://www.researchgate.net/publication/2408237_Tutorial_of_Oz_2

---

Table of contents:

- [Concepts of Oz](#TBD)
- [Installation und usage tips](#TBD)
- [Oz is not a Prolog system](#)
- [](#)
- [](#)
- [](#)

<br/>

---

## Concepts of Oz

Maybe this computer programming book of two of the later developers became more famous:

![plot](./Concepts%2C%20Techniques%2C%20and%20Models%20of%20Computer%20Programming.jpg)

from: https://webperso.info.ucl.ac.be/~pvr/book.html

..than the language itself.

Oz feels to me like that its supported programming paradigms are existing in parallel (nowadays), which stands in contrast to layered [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury) for example, where logic programming is the clear bottom layer.

Though the Mozart-Oz pair has been marketed as a "Multi-paradigm Programming System" (https://archive.fosdem.org/2007/slides/lightningtalks/MozartOz.pdf), it has its clear roots in logic programming:

> We give the history of Oz as it developed from its origins in logic programming, starting with Prolog, followed by concurrent logic programming and constraint logic programming, and leading to its two direct precursors, the concurrent constraint model and the Andorra Kernel Language (AKL). 

from: "A History of the Oz Multiparadigm Language", 2020, by its initial and main developers: https://www.ps.uni-saarland.de/Publications/documents/vanRoyHaridiSchulteSmolka2020.pdf, a very good source

Also see "Logic Programming in Oz with Mozart" by Peter Van Roy from 1999: http://mozart2.org/publications/abstracts/lpinoz99.html, which still can be searched as a ready PDF file in the web:

> This short tutorial explains how to do Prolog-style logic programming in Oz. ... The Oz computation model subsumes both search-based logic programming and committed-choice (concurrent) logic programming with deep guards.

KÖRNER P, LEUSCHEL M, BARBOSA J, et al. ([Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD), 2022) sort Oz into the group of **Committed-choice Languages**, which are:

> ...logic languages supporting “committed choice” where only the first clause whose guard succeeds is executed, instead of the multiple execution paths supported by Prolog.

<br/>

## Installation und usage tips

Oz has a strong focus on teaching computer programming, and doing this with means of a graphical user interface (GUI), or more specifically with commands in menu tree "Oz" of the **GNU Emacs** text editor:

![plot](./Oz%20programming%20interface%2C%20GNU%20Emacs.png)

(Initially, "Buffers" is set to "Oz")

This approach makes it not so easy to do (Linux) shell based software development like in a typical Prolog system. It took me a while, but finally I found a solution with the help of Big AI.

<br/>

Installation in a Linux system is easy with just downloading the suitable (Linux) operating system package and installing it with the related packager manager (by double clicking on the package), here file _mozart2-2.0.1-x86_64-linux.deb_ from: https://github.com/mozart/mozart2/releases/tag/v2.0.1 for a Debian based Linux system like Ubuntu.

Also shell commands to start Mozart tools are now available without extra configurations, though the question is: how to start an Oz compiler or interpreter from a Linux shell, like in a Prolog system for example?

When you enter shell command: _$ oz_, the Emacs editor pops up like shown above.

Instead do this:

1/ write a usual source code file, like here for example [prolog_system_test.oz](./prolog_system_test.oz) (to skip the usual "Hello, World!" example):

```
functor
import
  System
  Application
define
    L1 = [1 2]
    L2 = [3 4]
    {System.showInfo "L1 = "#{Value.toVirtualString L1 0 0}}
    {System.showInfo "L2 = "#{Value.toVirtualString L2 0 0}}
    L3 = {Append L1 L2}
    {System.showInfo "L3 = "#{Value.toVirtualString L3 0 0}}
    L4 = {Append nil L1}
    {System.showInfo "L4 = "#{Value.toVirtualString L4 0 0}}
    L5 = [L1|L2]
    {System.showInfo "L5 = "#{Value.toVirtualString L5 0 0}}
    {Application.exit 0}
end
```

2/ then compile this file to a distributable object file (.ozf):

```
$ ozc -c prolog_system_test.oz
```

3/ finally, run this object file:

```
$ ozengine prolog_system_test.ozf
L1 = [1 2]
L2 = [3 4]
L3 = [1 2 3 4]
L4 = [1 2]
L5 = [[[1 2] 3 4]]
$ 
```

By the way: the following approach is not working at the moment (at least in my system), though it should work from my point of view:

```
$ ozc -x prolog_system_test.oz
bash: ./prolog_system_test: cannot execute binary file: Exec format error
$
```

### Oz is not a Prolog system

Here's another way as the common way via the GUI to demonstrate that the Mozart-Oz pair is not a Prolog system.

Just enter or copy&paste, and then execute the predicates from this [Picat example](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Picat#some-concepts-of-picat):

![plot](./Oz%20programming%20interface%2C%20GNU%20Emacs%20b.png)

<br/>

But as a first exercise, I copied the first example of a **procedure call**: _{Browse 9999*9999}_ from [Declarative programming with Oz](https://staff.fmi.uvt.ro/~mircea.marin/lectures/ALFP/Oz-introduction.pdf), 2017, and pasted it into the upper text buffer of the "Oz Programming interface". Then this **application** must be executed with mouse clicks or Emacs keyboard commands:

- mouse: for example, click on menu bar item "Oz" and then "Feed Buffer"
- keyboard: for example, press key [F10] to activate the menu bar. Then use the arrow right key to open the "Oz" menu tree. There scroll down to "Feed Buffer" and press [ENTER]

Now the "Oz Browser" window should pop up as shown above. 

<br/>

However, when these Prolog predicates:

```
append([], L, L).
append([X | [1,2]], [3,4], [X | N]).
append([1,2], [3,4], N).
```

..are being pasted into the upper text buffer and executed, nothing is happening, not even an error message is thrown out.

<br/>

TBD

ideas:

- GeOz: Integration with Gecode for the map coloring problem of Germany??
- xxx
- xxx
- xxx

<br/>

##_end
