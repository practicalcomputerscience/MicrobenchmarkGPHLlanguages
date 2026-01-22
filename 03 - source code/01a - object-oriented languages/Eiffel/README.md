2026-01-22: work in progress

# Eiffel

https://www.eiffel.org/

https://archive.eiffel.com/eiffel/nutshell.html

ECMA-367: Eiffel: Analysis, design and programming language, 2nd ed., June 2006: https://ecma-international.org/publications-and-standards/standards/ecma-367/

https://github.com/seamus-brady/awesome-eiffel

## Idea of Eiffel

I came to Eiffel when I noticed that it had an influence on the design of 
[Ruby](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#ruby) 
and [D](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/D#d) (and others), 
clearly two languages where the programming paradim of **object-oriention** is sitting above the basic, **imperative** paradigm.

But then I noticed that (also) Eiffel is different from your "usual", object-oriented language. I guess it's clear that historically Eiffel has been designed 
around the basic construct of a **class**. So, I'd say that this is truly an object-oriented language; see its version of "Hello, World!":

TBD

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

Although both languages are still updated from time to time, they are not listed in the TIOBE index of popularity as of January 2026: https://www.tiobe.com/tiobe-index/,
where Eiffel peaked there at #33 in 2012: https://www.eiffel.com/2012/eiffel_tiobe/, while Self was apparently never listed there.

TBD

<br/>

##_end
