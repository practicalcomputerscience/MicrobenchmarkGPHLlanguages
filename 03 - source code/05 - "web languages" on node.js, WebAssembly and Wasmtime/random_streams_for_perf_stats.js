/*
random_streams_for_perf_stats.js

2026-01-31

run on Ubuntu 24 LTS: $ node ./random_streams_for_perf_stats.js
                      $ time node ./random_streams_for_perf_stats.js => real	0m0.042s
                      $ multitime -n 20 node ./random_streams_for_perf_stats.js
                      =>
                                    Mean        Std.Dev.
                        real        0.042       0.001


$ node -v
v22.21.0
$


transpiled from random_streams_for_perf_stats.ts with Duck.ai

*/

const fs = require('fs');

class random_streams_for_perf_stats {
    static main() {
        const END = 62501;  // 62501 for exactly 1M binary digits

        const m = 65521;  // = 2^16 - 15
        const a = 17364;
        const c = 0;

        const file_bits_x   = "random_bitstring.bin";
        const file_bits_hex = "random_bitstring.byte";

        const x = new Array(END);
        const rnd = Math.floor(Math.random() * (m - 1)) + 1;
        x[0] = rnd;

        let bits_x   = "";
        let bits_hex = "";

        console.log("\ngenerating a random bit stream...");
        for (let i = 1; i < END; i++) {
            x[i] = (a * x[i - 1] + c) % m;

            const bits_xStr = x[i].toString(2).padStart(16, '0');
            bits_x += bits_xStr;

            const bits_hexStr = x[i].toString(16).padStart(4, '0');
            bits_hex += bits_hexStr;
        }

        try {
            fs.writeFileSync(file_bits_x, bits_x);
            console.log(`Bit stream has been written to disk under name:  ${file_bits_x}`);
        } catch (ex) {
            console.error(`could not write to file: ${file_bits_x} ! -- ${ex.message}`);
        }

        try {
            fs.writeFileSync(file_bits_hex, bits_hex);
            console.log(`Byte stream has been written to disk under name: ${file_bits_hex}`);
        } catch (ex) {
            console.error(`could not write to file: ${file_bits_hex} ! -- ${ex.message}`);
        }
    }
}

// Run the main function
random_streams_for_perf_stats.main();

// end of random_streams_for_perf_stats.js
