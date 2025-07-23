https://go.dev/

---

#### On concurrency in Go

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

##_end
