# Go

https://go.dev/

---

### On concurrency in Go

On the [main page](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages#other-aspects-of-a-computer-programming-language) I said this:

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

## A sobering experience: formatted I/O

Back in 2023 I read good things about Go and so it landed on my long list (with no practical tests until now). It was the first language into which I translated (manually) my
unintentional benchmark program from the original [Python version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Python/random_bitstring_and_flexible_password_generator.py).

I also thought that the official [language documentation of Go](https://go.dev/doc/) was by far the best I've seen in years, and I've seen a lot of official language documentation in the last couple of months. For my taste, the role of good documentation is still not taken serious enough in too many cases. Documentation, with non-trivial examples, creates trust in the long run and not shiny presentations.

Though in the meantime I had to walk back a little bit from this impression, though I still think that Go's documentation is pretty good.

> [!WARNING]
> However, when I jumped from language to language, I re-checked if I've really implemented the same behavior in my Go program (and other language versions), only to notice that this was not
the case!

After initial tests the Go program worked Okish too, but not with the exact same behavior like my original Python program. And it was not a minor, cosmetic issue, but a potentially
dangerous behavior for a computer program.

Weeks later I tested my Go program with another corner case, only to discover that there's another problem. Only many weeks later I think I understood that with Go at least one group of functions behave substantially different from similar functions in all the other programming languages I tested so far: 

> [!CAUTION]
> The problem is with functions in Go package _**fmt**_ for formatted I/O: https://pkg.go.dev/fmt

<br/>

**Originally**, I used function _fmt.Scanln()_ to read user input from the keyboard "into a string" on the console: 

```
...
  var N_CHAR int

  for {
    N_CHAR = 12
    N_CHAR_STR := strconv.Itoa(N_CHAR)
    fmt.Printf("\n\nPassword of %s printable chars OK? 'y' or another integer number >= 8: ", N_CHAR_STR)

    fmt.Scanln(&N_CHAR_STR)
    if N_CHAR_STR == "y" {
      break
    }

    i, err := strconv.Atoi(N_CHAR_STR)
    if err != nil {
        fmt.Print("enter an integer number >= 8 or 'y'")
    } else {
      if i >= 8 {
        N_CHAR = i
        break
      } else {
        fmt.Print("enter an integer number >= 8 or 'y'")
      }
    }
  }

  WITH_SPECIAL_CHARS := true
  var reply string

  for WITH_SPECIAL_CHARS {
    fmt.Printf("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
    fmt.Scanln(&reply)
    if reply == "y" {
      break
    } else {
      WITH_SPECIAL_CHARS = false
    }
  }
...
```

And with "into a string" I mean the "complete" user input **into one string**!

Because with every **white space character** between two non-white space characters at the first user input of this program, which is all input until the user presses the ENTER key for the first time, internally another substring will be automatically created and stored for potentially later use!!

Which means that the content of "substring #2" after user input of "10 10" for example, that is "10", will be automatically used as content of following string variable _reply_ in the source code as shown above: _fmt.Scanln(&reply)_

However, "10 10" or "66 ggg" as the first user input of this program shall not be accepted as a valid answer to question: 

```
Password of 12 printable chars OK? "y" or another integer number >= 8:
```

"Substring #2" with content "10" or "ggg" respectively will then be used as answer to question #2 for the user and above code will evaluate them as a "no" answer (which is the desired behavior of this program) - something which could represent potentially dangerous behavior in a more serious computer program than this hobby microbenchmark!

<br/>

So, after a longer while I came to the conclusion that using Go's _fmt_ functions would not lead to an acceptably well behaving program.

So, for my [final version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go/random_bitstring_and_flexible_password_generator.go) so far I switched to function _bufio.NewReader(os.Stdin)_ from the _**bufio**_ package: https://pkg.go.dev/bufio

```
...
  for {
    N_CHAR = 12
    N_CHAR_STR := strconv.Itoa(N_CHAR)
    fmt.Printf("\n\nPassword of %s printable chars OK? 'y' or another integer number >= 8: ", N_CHAR_STR)

    // very important: make these definitions only inside this loop, not outside to prevent a "spill-over"
    // of an answer like "10 10" with whites spaces in between:
    reader := bufio.NewReader(os.Stdin)
    reply, err := reader.ReadString('\n')
    // this solution, incl. a terminal "\n", was found from here: https://golangr.com/keyboard-input

    reply_ := strings.TrimSpace(reply)  // MS Bing AI answer

    // fmt.Printf("reply_ = %s---\n", reply_)  // for testing
    // fmt.Printf("err = %s\n", err)  // for testing --> err = %!s(<nil>) as a good answer
    ////  err.Error() != "expected newline" --> panic: runtime error: invalid memory address or nil pointer dereference

    if reply_ == "y" && err == nil {
      break
    }

    i, err2 := strconv.Atoi(reply_)
    if err2 != nil {
        fmt.Print("enter an integer number >= 8 or 'y'")
    } else {
      if i >= 8 {
        N_CHAR = i
        break
      } else {
        fmt.Print("enter an integer number >= 8 or 'y'")
      }
    }
  }
...
```

<br/>

Now I ask myself: are there other corners in Go where it's advisable to be on higher alert for unconventional behavior and effects?

Because my microbenchmark program can only scratch the surface of a general purpose, high-level programming language:

> [!TIP]
> For a reason there's a book (from 2022) with troubles in Go you can run into: https://www.thecoder.cafe/p/100-go-mistakes

<br/>

##_end
