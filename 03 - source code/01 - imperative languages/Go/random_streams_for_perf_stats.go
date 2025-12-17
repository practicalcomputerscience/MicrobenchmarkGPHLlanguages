/*
random_streams_for_perf_stats.go

2025-05-31, 2025-06-02/03/18, 2025-07-17; 2025-12-17

build on Ubuntu 24 LTS: $ go build random_streams_for_perf_stats.go

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

*/

package main

import (
  "fmt"
  "math/rand"
  "os"
  "strings"
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
  x[0] = rand.Intn(int(m-1)) + 1  // returns a non-negative pseudo-random number in the half-open interval [0,n); 2025-12-17

  // var bits_x [M1]string    // needed for bit stream; old solution --> 0,038946 +- 0,000305 seconds time elapsed  ( +-  0,78% )
  var bits_x strings.Builder  //                        new solution --> 0,014174 +- 0,000118 seconds time elapsed  ( +-  0,83% )
  // https://golangdocs.com/concatenate-strings-in-golang

  var bits_x_str string
  // var byte_nbr int
  // var bits_hex [K250]string  // needed for program ENT - A Pseudorandom Number Sequence Test Program
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

}

// end of random_streams_for_perf_stats.go
