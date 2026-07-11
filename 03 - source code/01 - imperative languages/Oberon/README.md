2026-07-10: work in progress

<br/>

# Oberon

> Oberon is a general-purpose programming language that evolved from Modula-2.

from: The Programming Language Oberon, (Revision 1. 10. 90), N.Wirth (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon.Report.pdf

And not only that, but Oberon is also an example of how to damage an ecosystem, together with the Modula ecosystem, by **fragmenting** it. I found these _original_ flavors of Oberon:

- Oberon since 1988: https://onlinelibrary.wiley.com/doi/10.1002/spe.4380180706
- Oberon-2, "essentially Oberon with a few extensions", since 1990 (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon.Report.pdf
- Active Oberon ("Do the Fish Really Need Remote Control? A Proposal for Self-Active Objects in Oberon") since 1997: https://link.springer.com/chapter/10.1007/3-540-62599-2_41
- Persistent Oberon from 2000: https://link.springer.com/chapter/10.1007/978-3-540-76637-7_6
- Oberon-07/16, also known as "Revised Oberon" since 2007 (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon07.Report.pdf

See from here about some differences between (some) Oberon dialects: [Motivation for a new Oberon version](https://oberon-lang.github.io/2021/07/15/motivation-for-a-new-oberon-version.html)

<br/>

To me it looks a bit that Oberon was Wirth's attempt to not miss the already rolling object-oriented programming (OOP) train,
since [Modula-2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3)
was (still) not explicitely designed for OOP as published in 1980: [MODULA-2, Wirth, Niklaus](https://doi.org/https://doi.org/10.3929/ethz-a-000189918),
and its fully OOP-capable successor [Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3), first published in 1988, no longer under his control.

I guess that this paper from 1989 highlights his scepticism and dilemma:

> It is a sad fact that our field of Computer Science is overly dominated by fads.
> 
> ...
> 
> The most recent slogan is object-oriented programming.
> 
> ...
> 
> It is by no means accidental that the paradigm of object-oriented programming - we bow to convention and adopt the misnomer - originated
> in the application area of simulation of system with discrete events.

from: "Modula-2 and object-oriented programming ...", ..., Author(s): Wirth, Niklaus: https://doi.org/https://doi.org/10.3929/ethz-a-000523424

<br/>

Another aim of Oberon was:

> ..to reduce the complexity of programming languages, of Modula in particular.

from: "Programming, A Tutorial, A derivative of Programming in Modula-2 (1982)", Niklaus Wirth (rev. 5.10.2015), PDF: https://people.inf.ethz.ch/wirth/Oberon/PIO.pdf

<br/>

Ironically, Modula-2 and Modula-3 are still around to some extent in 2025, under their original names, but what happened to Oberon? And in what _implementation_ (on Linux) still usable in year 2026?

This page gave me advice: https://fruttenboel.nl/obc/Main.html

> OBC is by far the best Oberon compiler. ETHZ versions of Oberon are dead, OBC is the way to go 

See at [Oxford_Oberon-2_compiler](https://spivey.oriel.ox.ac.uk/corner/Oxford_Oberon-2_compiler).

tbd

<br/>

> [!WARNING]
> The History of Oberon also shows that too much minimalism in the design of a programming language is probably (way) more detrimental to its success than overly complexity (like in C++ or Rust).

Otherwise, what else should be the motivation to make another derivative of the original Oberons, here:

[The Oberon+ Programming Language](https://oberon-lang.github.io/), also known as Oberon with extensions or OBX

(bytecode for [CLI/ECMA-335](https://ecma-international.org/publications-and-standards/standards/ecma-335/) is actually bytecode for Microsofts's .NET runtime environment)

> From these considerations a new language emerged, which I call Oberon+ (i.e. “Oberon with extensions”, abbreviated OBX); it is based on Oberon-07, Oberon-2 and Oberon 90, with all the elements of these languages, plus the - from my point of view - most essential missing features and a lot of simplifications and increased flexibility. Oberon+ is - so to say - the Oberon I personally would have hoped for; from my point of view it represents modern simplicity in programming.

from: https://oberon-lang.github.io/2021/07/15/motivation-for-a-new-oberon-version.html

However, I think that also this language designer already fell into the same trap like Wirth himself (which he knew: https://rochus-keller.ch/?page_id=17), and that is having three names for the same language:

- Oberon+
- Oberon with extensions
- OBX

..even though there's only _one compiler_ existing for Oberon+.

<br/>

tbd

<br/>

##_end
