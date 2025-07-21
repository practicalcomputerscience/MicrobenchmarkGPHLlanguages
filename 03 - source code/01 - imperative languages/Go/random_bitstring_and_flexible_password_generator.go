/*
random_bitstring_and_flexible_password_generator.go

2025-03-25/26/27/28/29/31, 2025-05-05/17/31, 2025-06-03/06/18, 2025-07-17

build on Ubuntu 24 LTS: $ go build random_bitstring_and_flexible_password_generator.go

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator
  Here with no symbol and debug info which makes a significantly smaller binary file:
    $ go build -ldflags "-s -w" random_bitstring_and_flexible_password_generator.go
    see: https://stackoverflow.com/questions/29599209/how-to-build-a-release-version-binary-in-go
    => no speed improvement!


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
2025-03-27: old solution with adding characters to strings in the modulo loop (starting with an empty string):
  this took 3.752s to run
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

package main

import (
  "fmt"
  "strconv"
  "math/rand"
  "regexp"
  "os"
  "strings"
  "bufio"
)


func main() {
  const END int  = 62501  // 62501 for exactly 1M binary digits
  // const M1 int   = END*16
  // const K250 int = END*4

  const m = 65521  // = 2^16 - 15
  const a = 17364
  const c = 0

  const file_bits_x string = "random_bitstring.bin"
  const file_bits_hex string = "random_bitstring.byte"


  var x = make([]int, END)  // allocate memory --> automatic garbage collection with Go
  // also needed for the password
  x[0] = rand.Intn(int(m))

  var bits_x strings.Builder  // new solution --> 0,014174 +- 0,000118 seconds time elapsed  ( +-  0,83% )
  // https://golangdocs.com/concatenate-strings-in-golang
  var bits_x_str string

  var bits_hex strings.Builder


  print("\ngenerating a random bit stream...")

  for i := 1; i < END; i++  {
    x[i] = (a*x[i-1] + c) % m

    bits_x_str = fmt.Sprintf("%016b", x[i])  // needed for bit stream
    bits_x.WriteString(bits_x_str)

    bits_x_str = fmt.Sprintf("%04x", x[i])   // needed for program ENT
    bits_hex.WriteString(bits_x_str)
  }

  // write bit stream to disk:
  f, err := os.Create(file_bits_x)
  if err != nil {
    fmt.Printf("\ncould not open file for writing: %s -- %s", file_bits_x, err)
  }

  _, err = fmt.Fprint(f, bits_x.String())
  if err != nil {
    fmt.Printf("\ncould not write to file: %s -- %s", file_bits_x, err)
  } else {
    fmt.Printf("\nBit stream has been written to disk under name: %s", file_bits_x)
  }
  f.Close()

  // write byte stream to disk:
  f, err = os.Create(file_bits_hex)
  if err != nil {
    fmt.Printf("\ncould not open file for writing: %s -- %s", file_bits_hex, err)
  }
  _, err = fmt.Fprint(f, bits_hex.String())
  if err != nil {
    fmt.Printf("\ncould not write to file: %s -- %s", file_bits_hex, err)
  } else {
    fmt.Printf("\nByte stream has been written to disk under name: %s\n", file_bits_hex)
  }
  f.Close()


  // make a password of N_CHAR printable chars:
  var N_CHAR int

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

  WITH_SPECIAL_CHARS := true
  for WITH_SPECIAL_CHARS {

    // very important: make these definitions only inside this loop:
    stdin := bufio.NewReader(os.Stdin)
    var reply string

    fmt.Printf("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")

    h, err := fmt.Fscanln(stdin, &reply)

    if reply == "y" && h == 1 && err == nil {
      break
    } else {
      WITH_SPECIAL_CHARS = false
    }
  }

  var pattern *regexp.Regexp  // using pointer logic allows to generate a regexp object
                              // at pattern without immediately using it
  var reg_err error

  if WITH_SPECIAL_CHARS {
    pattern, reg_err = regexp.Compile("[[:graph:]]+")  // true case
    _ = reg_err  // get rid of the "declared and not used" error message
  } else {
    pattern, reg_err = regexp.Compile("[[:alnum:]]+")
    _ = reg_err
  }


  i := 0  // char counter for the password
  j := 0  // char counter for x
  pw_chars := ""

  for {
    // convert an integer number into a string of '0' and '1' characters:
    bin0 := fmt.Sprintf("%016b", x[j])
    // bin0 could be for example ' 111001001100101'
    // --> padding needed with leading zeros: %016b

    bin0_0 := bin0[0:8]   // position 8 is exclusive in Go
    bin0_1 := bin0[8:16]

    // convert a string of '0' and '1' characters into an unsigned int number:
    char0, err := strconv.ParseUint(bin0_0, 2, 16)  // char0 is of type uint64
    _ = err  // get rid of the "declared and not used" error message
    char0b := fmt.Sprintf("%c", int(uint8(char0)))

    char1, err := strconv.ParseUint(bin0_1, 2, 16)
    _ = err
    char1b := fmt.Sprintf("%c", int(uint8(char1)))

    if pattern.MatchString(char0b) {
      pw_chars = pw_chars + char0b
      i += 1
      if i == N_CHAR {
        break
      }
    }

    if pattern.MatchString(char1b) {
      pw_chars = pw_chars + char1b
      i += 1
      if i == N_CHAR {
        break
      }
    }

    j += 1
  }

  fmt.Printf("\nYour password of %d characters is: %s\n", N_CHAR, pw_chars)
}

// end of random_bitstring_and_flexible_password_generator.go
