2025-12-02: work in progress

# Oz

Development and execution environment **Mozart** (version 2): http://mozart2.org/

Oz cheat sheet from 2019: https://github.com/alhassy/OzCheatSheet/blob/master/CheatSheet.pdf

<br/>

At the moment, Oz, now in Version 3, and Mozart feel like a big, almost abandoned construction site with numerous dead links in the web,
here for example for Oz version 2: https://www.ps.uni-saarland.de/oz2/documentation/, or links to just outdated but still useful documentation (https://www.mozart-oz.org/documentation/),
since also current Mozart version 2.0.1 is still using many old parts.

Therefore, I often look into its GitHub repository to see what functions are currently available and how to use them, like here for [lists](https://github.com/mozart/mozart2/blob/83c83da2f670fbd1d08d4145eca3d88f1687582c/lib/main/base/List.oz) for example.

<br/>

## Concepts of Oz

Maybe this computer programming book of two of the later developers became more famous:

![plot](./Concepts%2C%20Techniques%2C%20and%20Models%20of%20Computer%20Programming.jpg)

from: https://webperso.info.ucl.ac.be/~pvr/book.html

..than the language itself.

Oz feels to me like that its supported programming paradigms are existing in parallel, which stands in contrast to layered [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury) for example, where logic programming is the clear bottom layer.

Though the Mozart-Oz pair has been marketed as a "Multi-paradigm Programming System" (https://archive.fosdem.org/2007/slides/lightningtalks/MozartOz.pdf), it has its clear roots in logic programming:

> We give the history of Oz as it developed from its origins in logic programming, starting with Prolog, followed by concurrent logic programming and constraint logic programming, and leading to its two direct precursors, the concurrent constraint model and the Andorra Kernel Language (AKL). 

from: "A History of the Oz Multiparadigm Language", 2020, by its initial and main developers: https://www.ps.uni-saarland.de/Publications/documents/vanRoyHaridiSchulteSmolka2020.pdf, a very good source

Also see "Logic Programming in Oz with Mozart" by Peter Van Roy from 1999: http://mozart2.org/publications/abstracts/lpinoz99.html, which still can be searched as a ready PDF document in the web:

> This short tutorial explains how to do Prolog-style logic programming in Oz. ... The Oz computation model subsumes both search-based logic programming and committed-choice (concurrent) logic programming with deep guards.

KÖRNER P, LEUSCHEL M, BARBOSA J, et al. ([Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD), 2022) sort Oz into the group of **Committed-choice Languages**, which are:

> ...logic languages supporting “committed choice” where only the first clause whose guard succeeds is executed, instead of the multiple execution paths supported by Prolog.

<br/>

## Installation und usage tips

Oz has a strong focus on teaching computer programming, and doing this with means of a graphical user interface (GUI), or more specifically as menu tree "Oz" in the Emacs text editor:

![plot](./Oz%20programming%20interface%2C%20GNU%20Emacs.png)

This approach makes it not easy to do (Linux) shell based software development, be it interpreted or compiled, like in Prolog system. It took me a while, but finally I found a solution.

<br/>

Installation in a Linux system is easy with just downloading the suitable (Linux) operating system package and installing it with the related packager manager (by double clicking on the package), here file _mozart2-2.0.1-x86_64-linux.deb_ from: https://github.com/mozart/mozart2/releases/tag/v2.0.1 for a Debian based Linux system like Ubuntu.

Also shell commands to start the Mozart tools are now avaiable without extra configurations.

But the bigger question now is: how to start an Oz compiler or interpreter from a Linux shell?

After some tinkerung, I found this solution:

TBD






##_end
