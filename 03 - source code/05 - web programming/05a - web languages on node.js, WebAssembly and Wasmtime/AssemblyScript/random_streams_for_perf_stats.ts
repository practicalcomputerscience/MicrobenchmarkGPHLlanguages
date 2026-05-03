/*
random_streams_for_perf_stats.ts -- this is an AssemblyScript (for WASI), not TypeScript, file to be compiled to a WebAssembly file

2026-04-30, 2026-05-02

built on Ubuntu 24 LTS: do this only once:
                        $ npm install -g npm@11.13.0          # update npm if needed
                        $ sudo npm install -g assemblyscript  # install the compiler globally for CLI access
                        $ asc --version  # verify the installation
                        # install the WASI (WebAssembly System Interface) Shim to utilize WASI imports instead of Web API's:
                        #   this is a global solution for every project in the same directory!!! (so, be careful...)
                        $ npm install --save-dev @assemblyscript/wasi-shim
                        # tip: remove any prior files, like directory node_modules, if this command doesn't create new files and dir's!!

                        do this after every source code change:
                        # compile the AssemblyScript file to a WebAssembly file:
                        $ asc random_streams_for_perf_stats.ts --outFile random_streams_for_perf_stats.wasm --optimize --config node_modules/@assemblyscript/wasi-shim/asconfig.json


run on Ubuntu 24 LTS:   $ wasmtime --dir=. ./random_streams_for_perf_stats.wasm  # --dir=. to grant permissions to the current dir
                        $ time wasmtime --dir=. ./random_streams_for_perf_stats.wasm => real	0m0.028s <<<<<<<<<<<<<<<<<
                        $ multitime -n 20 wasmtime --dir=. ./random_streams_for_perf_stats.wasm


transpiled from random_streams_for_perf_stats.ts (TypeScript) with Google AI


$ asc --version
Version 0.28.17
$ wasmtime -V
wasmtime 44.0.1 (f302ebd6b 2026-04-30)
$

*/

import { Console, FileSystem, Descriptor } from "as-wasi/assembly";

/////////////////////////////////////////////////////////////////////////////////////////////
//
// user defined functions:

// helper to replace missing .padStart() function available in TypeScript:
function padLeft(str: string, len: i32, char: string): string {
  while (str.length < len) {
    str = char + str;
  }
  return str;
}

// helper to write a string to a file. Returns true on success, false on failure
// In AssemblyScript, a try-catch construct is not currently supported for error recovery.
// While the throw keyword exists, using it will immediately abort/terminate
// the entire WebAssembly program rather than jumping to a catch block.
function saveToFile(path: string, content: string): bool {
  let file: Descriptor | null = FileSystem.open(path, "w");

  if (file == null) {
    // If open fails (usually due to missing --dir permissions), return false
    return false;
  }

  // 1. Encode string to ArrayBuffer
  let buffer = String.UTF8.encode(content);

  // 2. Convert ArrayBuffer to Array<u8> (required by as-wasi Descriptor)
  let bytes = new Array<u8>(buffer.byteLength);
  for (let i = 0; i < buffer.byteLength; i++) {
      bytes[i] = load<u8>(changetype<usize>(buffer) + i);
  }

  // 3. Perform the write (Method returns void, so no assignment here)
  file.write(bytes);

  // 4. Cleanup
  file.close();

  return true;
}

// helper to directly write the 16-bit binary representation into the byte buffer:
function writeBinaryToBuffer(val: u32, buf: Uint8Array, offset: i32): void {
  for (let i: i32 = 0; i < 16; i++) {
    // Check bits from left to right (MSB first)
    let bit = (val >> (15 - i)) & 1;
    buf[offset + i] = bit == 1 ? 0x31 : 0x30; // '1' or '0' in UTF-8
  }
}

// helper to directly write the 4-digit hex representation into the byte buffer:
function writeHexToBuffer(val: u32, buf: Uint8Array, offset: i32): void {
  const chars = "0123456789abcdef";
  for (let i: i32 = 0; i < 4; i++) {
    let nibble = (val >> ((3 - i) * 4)) & 0xF;
    buf[offset + i] = <u8>chars.charCodeAt(nibble);
  }
}

// end of user defined functions
//
/////////////////////////////////////////////////////////////////////////////////////////////


class random_streams_for_perf_stats {
  static main(): void {
    const END:  i32 = 62501;  // 62501 for exactly 1M binary digits; i32: AssemblyScript uses strict typing
    // const END:  i32 = 10;     // for testing
    const M1:   i32 = END * 16 - 16;
    const K250: i32 = END * 4 - 4;


    const m: u32 = 65521; // = 2^16 - 15
    const a: u32 = 17364;
    const c: u32 = 0;

    const file_bits_x   = "random_bitstring.bin";
    const file_bits_hex = "random_bitstring.byte";

    const x = new Array<u32>(END);  // strict typing

    // AssemblyScript's Math.random returns f64, similar to TS:
    const rnd = <u32>(Math.floor(Math.random() * <f64>(m - 1)) + 1);  // <u32> for explicit casting
    x[0] = rnd;

    // let bits_x:   string = "";  # with: bits_x   += bits_x_str; => slow solution with 3.7 sec exe etime
    // let bits_hex: string = "";
    // => much faster solution:
    //   Pre-allocate byte arrays to avoid "copy-on-concatenate" overhead
    let bits_x   = new Uint8Array(M1);
    let bits_hex = new Uint8Array(K250);


    Console.log("\ngenerating a random bit stream...");
    for (let i: i32 = 1; i < END; i++) {
      x[i] = (a * x[i - 1] + c) % m;

      const bits_x_str   = padLeft(x[i].toString(2), 16, "0");
      // Console.log("\nbits_x_str: " + bits_x_str);      // for testing; Console.log from the as-wasi module
      writeBinaryToBuffer(x[i], bits_x, (i - 1) * 16);

      const bits_hex_str = padLeft(x[i].toString(16), 4, "0");
      // Console.log("\nbits_hex_str: " + bits_hex_str);  // for testing
      writeHexToBuffer(x[i], bits_hex, (i - 1) * 4);
    }


    // Convert the full buffers to strings only once:
    let bits_x_str_total   = String.UTF8.decode(bits_x.buffer);
    let bits_hex_str_total = String.UTF8.decode(bits_hex.buffer);
    // Console.log("\n\nbits_x: " + bits_x_str_total);           // for testing
    // Console.log("\nbits_hex: " + bits_hex_str_total + "\n");  // for testing


    // writing to files only takes about 0.1 sec!
    // write bit stream to disk:
    if (saveToFile(file_bits_x, bits_x_str_total)) {
      Console.log("\nBit stream has been written to disk under name:  " + file_bits_x);
    } else {
      Console.error("\ncould not write to file: " + file_bits_x);
    }

    // write byte stream to disk:
    if (saveToFile(file_bits_hex, bits_hex_str_total)) {
      Console.log("\nByte stream has been written to disk under name: " + file_bits_hex + "\n");
    } else {
      Console.error("\ncould not write to file: " + file_bits_hex + "\n");
    }
  }  // end of main()
}  // end of class

// Entry point for WASI:
random_streams_for_perf_stats.main();

// end of random_streams_for_perf_stats.ts (AssemblyScript)
