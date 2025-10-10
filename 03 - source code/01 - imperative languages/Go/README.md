# Go

https://go.dev/

---

### On concurrency in Go

On the main page (https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#other-aspects-of-a-computer-programming-language) I said this:

> Though, in one instance I've made a derivative program of the "speed part" to see how concurrency works in Go. This was rather easy and as easy as advertised.

<br/>

This is from a recent experience report on Go:

> Go’s concurrency model was a major draw but putting it into production wasn’t without its headaches.
>
> While it is easy to create goroutines, cleanup and coordination across goroutines was an area that initially presented some challenges.
>
> These challenges pushed us to adopt stronger patterns for managing concurrent processes. We’ve embraced the sync & context packages along with ensuring the use of defer to avoid goroutine leaks.
>
> These patterns now help us write more robust and maintainable Go code. And to ensure we catch concurrency bugs early, we enable Data Race Detection in our tests by default.

from: _Go at American Express Today: Seven Key Learnings_, Published July 16, 2025: https://www.americanexpress.io/go-at-american-express-today/

<br/>

## A sobering experience

Back in 2023 I read good things about Go and so it landed then on my long list (with no practical tests until now). It was the first language into which I translated (manually) my
unintentional benchmark program from the original [Python version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_bitstring_and_flexible_password_generator.py).

I also thought that the official [language documentation of Go](https://go.dev/doc/) was by far the best I've seen in years, and I've seen a lot of official language documentation in the last couple of months. For my taste, the role of good documentation is still not taken serious enough in too many cases. Documentation, with non-trivial examples, creates trust in the long run and not shiny presentations.

Though in the meantime I had to walk back a little bit from this impression, though I still think that Go's documentation is pretty good.

> [!WARNING]
> However, when I jumped from language to language, I re-checked if I've really implemented the same behavior in my Go program (and other language versions), only to notice that this was not
the case!

After initial tests the Go program worked Okish too, but not with the exact same behavior like my original Python program. And it was not a minor, cosmetic issue, but a potentially
dangerous behavior for a computer program.

Weeks later I tested my Go program with another corner case, only to discover that there's another problem. Only many weeks later I think I understood that with Go at least one group of functions behave substantially different from similar functions in all the other programming languages I tested so far: 

TBD

<br/>

Now I ask myself: are there other corners in Go where it's advisable to be on higher alert for unconventional behavior and effects? Because my microbenchmark program can only scratch the surface of a general purpose, high-level programming language - naturally with only 87 lines of source code in Python: [LOC ranking list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/10%20-%20Lines%20Of%20source%20Code%20(LOC)%3A%20verbosity#loc-ranking-list)

For a reason there's a book (from 2022) with troubles in Go you can run into: https://www.thecoder.cafe/p/100-go-mistakes

<br/>

##_end
