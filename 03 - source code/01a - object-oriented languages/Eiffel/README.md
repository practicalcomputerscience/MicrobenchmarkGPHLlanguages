2026-01-22: work in progress

# Eiffel

ISE (Interactive Software Engineering or Eiffel Software) EiffelStudio: https://www.eiffel.org/

Liberty Eiffel: https://www.liberty-eiffel.org/

[Eiffel in a Nutshell](https://archive.eiffel.com/eiffel/nutshell.html)

ECMA-367: Eiffel: Analysis, design and programming language, 2nd ed., June 2006: https://ecma-international.org/publications-and-standards/standards/ecma-367/

"curated list of awesome Eiffel and Eiffel libraries, resources and tools": https://github.com/seamus-brady/awesome-eiffel

---

Table of contents:

- [Idea of Eiffel](#idea-of-eiffel)
- [Installation tips](#installation-tips)
- [On how to do demanding string building in Eiffel](#on-how-to-do-demanding-string-building-in-eiffel)

<br/>

---

## Idea of Eiffel

I came to Eiffel when I noticed that it had an influence on the design of 
[Ruby](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#ruby) 
and [D](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/D#d) (and others), 
clearly two languages where the programming paradim of **object-oriention** is sitting above the basic, **imperative** paradigm.

But then I noticed that (also) Eiffel is different from your "usual" object-oriented language. I guess it's clear that historically Eiffel has been designed 
around the basic construct of a **class** (+). So, I'd say that this is truly an object-oriented language; see its version of "Hello, World!" from the [Liberty Eiffel examples](https://github.com/LibertyEiffel/Liberty/blob/master/tutorial/hello_world.e):

```
class HELLO_WORLD
   -- ...
   --

create {ANY}
   main

feature {ANY}
   main
      do
         io.put_string("Hello World.%N")
      end

end -- class HELLO_WORLD
```

I say _also_, because this primary paradigm of object-orientation reminds me, without having any experience, of language Self:

> Why did Self became somehow famous? It allegedly was the first (major) language to introduce Traits into the world of object-oriented programming, and now (almost) everybody has them...

from: [Old computer programming languages learning new tricks](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#old-computer-programming-languages-learning-new-tricks)

_hello_world.self_:

```
(|
    helloWorld = ( 'Hello World!' printLine. ).
|) helloWorld.
```

Run this program like: _$ self -f hello_world.self_

Fun facts:

- Eiffel was first published in October 1986: https://archive.eiffel.com/eiffel/nutshell.html
- in the same year, the first version of Self was designed: [Self](https://selflanguage.org/)

Although both languages are still being updated from time to time, they are not listed in the TIOBE index of popularity as of January 2026: https://www.tiobe.com/tiobe-index/,
where Eiffel was peaking there at #33 in 2012: https://www.eiffel.com/2012/eiffel_tiobe/, while Self was apparently never listed there.

<br/>

Otherwise, it seems to me that Eiffel has been heavily influenced by [Ada](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada#ada).

<br/>

My two programs _random_streams_for_perf_stats.e_, and specifically _random_bitstring_and_flexible_password_generator.e_, are not following an **object-oriented design**, something Eiffel strongly aims for. There could be easily more classes than just _RANDOM_STREAMS_FOR_PERF_STATS_ and _RANDOM_BITSTRING_AND_FLEXIBLE_PASSWORD_GENERATOR_, respectively:

- a class for the random number generator
- a class for the user defined (helper) functions
- a class for the chosen character set from which the password characters are selected
- a class for writing strings to files
- a class for the user dialog, so something for a potential graphical user interface (GUI)
- a class for the password generator

..for example.

<br/>

## Installation tips

Since also the ISE Eiffel Studio Community Edition, an IDE, is asking for an account registration by default, I skipped it and tried performance-oriented [Liberty Eiffel](https://github.com/LibertyEiffel/Liberty). I decided to build it from sources: https://wiki.liberty-eiffel.org/index.php/Getting_Started (*)

(With me to be additionally installed: _$ sudo apt-get install castxml libgc-dev libcurl4-openssl-dev libssl-dev_ as **missing pre-requisites**.)

Then, I download latest zip file from: https://github.com/LibertyEiffel/Liberty, and unzipped it to a working directory. There I did as described in (*):

```
$ cd ./Liberty-master
$ ./install.sh -bootstrap  # this takes some time!
...
$
```

Among other things, a directory _./target/bin_ should have been created by now, and which holds a number of tools. 

But first, I added this directory to my _PATH_ enviroment variable (activate the _~/.bashrc_ config file!): _export PATH="$PATH:~/scripts/Eiffel/Liberty-master/target/bin"_

Finally, I did a first compilation test as described in the page above (*):

```
$ se compile ./Liberty-master/tutorial/hello_world.e -o hello_world
$ ./hello_world 
Hello World.
$ 
```

_se_ stands for the old SmartEiffel compiler, and has later became a wrapper around the Liberty Eiffel tools.

<br/>

### On how to do demanding string building in Eiffel

TBD: see from D for example

<br/>

(+) https://courses.cs.vt.edu/~cs3304/Spring02/lectures/lect12.pdf -- Programming Languages, Eiffel, Benjamin J. Keller, Department of Computer Science, Virginia Tech

<br/>

### Boosting the execution speed at the Liberty configuration

To further boost the execution speed of the compiled program, change the default optimization switch _-O2_ for the gcc (and g++) compiler in configuration file: _.../Eiffel/Liberty-master/target/liberty-eiffel/liberty.se_ to _-O3_ under the [boost] section:

```
...
[boost]
...
c_compiler_options: -O3 -pipe
...
cpp_compiler_options: -O3 -pipe
...
```

"Upgrading" this switch to _-Ofast -faggressive-loop-optimizations_ (see from here at C: [Program building tips](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#program-building-tips)) made the executable a little bit slower according the my experiments!

<br/>

##_end
