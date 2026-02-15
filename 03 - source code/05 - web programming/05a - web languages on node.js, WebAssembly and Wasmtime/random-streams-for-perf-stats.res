/* random-streams-for-perf-stats.res

2026-02-03
2026-02-15: changed name RandomStreamsForPerfStats to Random_streams_for_perf_stats
            cannot be random_streams_for_perf_stats like in the other languages,
            because module names must start with a capital letter


build on Ubuntu 24 LTS: $ npm create rescript-app@latest
                        set these three project parameters for example:
                          name of ReScript project: random-streams-for-perf-stats
                          template: Basic
                          ReScript version: 12.1.0
                        $ cd random-streams-for-perf-stats
                        copy random-streams-for-perf-stats.res to dir ./src
                        delete ./src/Demo.res
                        $ npm run res:build

run on Ubuntu 24 LTS:   $ node ./src/random-streams-for-perf.res.mjs
                        $ time node ./src/random-streams-for-perf.res.mjs => real	0m0.044s
                        $ multitime -n 20 node ./src/random-streams-for-perf.res.mjs
                        =>
                                      Mean        Std.Dev.
                          real        0.045       0.002


$ npm ls
random-streams-for-perf-stats@0.0.0 ~/scripts/ReScript/random-streams-for-perf-stats
└── rescript@12.1.0

$ npm --version
11.6.2
$

*/


// ReScript does not have official bindings for Node.js
// and here, I don't use rescript-nodejs: https://github.com/TheSpyder/rescript-nodejs
//
// this is a MS Bing AI solution
//
// Writing files with Node.js
// The easiest way to write to files in Node.js is to use the fs.writeFile() API.
// https://nodejs.org/en/learn/manipulating-files/writing-files-with-nodejs
//
// it's essential to place these statements outside of module Random_streams_for_perf_stats:
@module("fs")  // this is a resource of node.js!
external writeFileSync: (string, string) => unit = "writeFileSync"


// module names must start with a capital letter:
module Random_streams_for_perf_stats = {  // 2025-02-15
  let main = () => {
    // Define constants
    let upper_limit = 62501  // 62501 for exactly 1M binary digits
    // let upper_limit = 10  // for testing

    let m = 65521  // = 2^16 - 15
    let a = 17364
    let c = 0

    let file_bits_x   = "random_bitstring.bin"
    let file_bits_hex = "random_bitstring.byte"

    // Initialize array
    let x = Belt.Array.make(upper_limit, 0)

    // Initialize the random number generator
    x[0] = Js.Math.random_int(1, m - 1)  // Random number between 1 and m-1, both inclusive

    let bits_x   = []  // create a buffer
    let bits_hex = []  // create a buffer


    Console.log("\ngenerating a random bit stream...")
    for i in 1 to (upper_limit - 1) {
      // x[0] is of type option<'a> and thus must be handled accordingly:
      let x_old = Belt.Option.getWithDefault(x[i - 1], 0)  // 0 is a safe fallback value
      // Console.log("\nx_old = " + Int.toString(x_old))  // for testing

      let x_new = (a * x_old + c) % m
      // Console.log("x_new = " + Int.toString(x_new))  // for testing
      x[i]  = x_new

      let bits_x_str   = String.padStart(Int.toString(x_new, ~radix=2), 16, "0")
      // Console.log("bits_x_str = " + bits_x_str)  // for testing
      let _ = bits_x->Array.push(bits_x_str)

      let bits_hex_str = String.padStart(Int.toString(x_new, ~radix=16), 4, "0")
      // Console.log("bits_hex_str = " + bits_hex_str)  // for testing
      let _ = bits_hex->Array.push(bits_hex_str)
    }


    let bits_x_str_total   = bits_x->Array.joinUnsafe("")
    let bits_hex_str_total = bits_hex->Array.joinUnsafe("")
    // Console.log("\nbits_x_str_total = " + bits_x_str_total)  // for testing
    // Console.log("bits_hex_str_total = " + bits_hex_str_total)  // for testing


    // Google AI solution
    //
    // write bit stream to disk:
    try {
      writeFileSync(file_bits_x, bits_x_str_total)
      Console.log("Bit stream has been written to disk under name:  " + file_bits_x)
    } catch {
      | JsExn(obj) =>
        // Use JsExn.message(obj) to safely handle the 'unknown' type
        switch JsExn.message(obj) {
          | Some(msg) => Console.log("could not write to file: " + file_bits_x + " -- " + msg)
          | None      => Console.log("Caught a JS error with no message")
        }
      | _ => Console.log("Caught a non-JS error")
    }


    // write byte stream to disk:
    try {
      writeFileSync(file_bits_hex, bits_hex_str_total)
      Console.log("Byte stream has been written to disk under name: " + file_bits_hex)
    } catch {
      | JsExn(obj) =>
        switch JsExn.message(obj) {
          | Some(msg) => Console.log("could not write to file: " + file_bits_hex + " -- " + msg)
          | None      => Console.log("Caught a JS error with no message")
        }
      | _ => Console.log("Caught a non-JS error")
    }
  }
}

// Running the main function
Random_streams_for_perf_stats.main()

/* end of random-streams-for-perf-stats.res */


