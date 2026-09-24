"use strict";
/*
random_streams_for_perf_stats.cjs, transpiled from random_streams_for_perf_stats.ts
with TypeScript compiler tsc

2026-01-31, 2026-02-12, 2026-09-24


build on Ubuntu 24 LTS: do this only once:
                        $ npm i --save-dev @types/node

                        do this with every source code change:
                        $ tsc ./random_streams_for_perf_stats.ts
                        $ mv random_streams_for_perf_stats.js random_streams_for_perf_stats.cjs

run on Ubuntu 24 LTS:   $ node ./random_streams_for_perf_stats.cjs


$ node -v
v22.21.0
$ tsc -v
Version 5.9.3
$

*/
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("node:fs"); // node:fs is for deno, but OK for node.js + bun
var random_streams_for_perf_stats = /** @class */ (function () {
    function random_streams_for_perf_stats() {
    }
    random_streams_for_perf_stats.main = function () {
        var END = 62501; // 62501 for exactly 1M binary digits
        var m = 65521; // = 2^16 - 15
        var a = 17364;
        var c = 0;
        var file_bits_x = "random_bitstring.bin";
        var file_bits_hex = "random_bitstring.byte";
        var x = new Array(END);
        var rnd = Math.floor(Math.random() * (m - 1)) + 1;
        x[0] = rnd;
        var bits_x = "";
        var bits_hex = "";
        console.log("\ngenerating a random bit stream...");
        for (var i = 1; i < END; i++) {
            x[i] = (a * x[i - 1] + c) % m;
            var bits_x_str = x[i].toString(2).padStart(16, '0');
            bits_x += bits_x_str;
            var bits_hex_str = x[i].toString(16).padStart(4, '0');
            bits_hex += bits_hex_str;
        }
        try {
            fs.writeFileSync(file_bits_x, bits_x);
            console.log("Bit stream has been written to disk under name:  ".concat(file_bits_x));
        }
        catch (ex) {
            console.error("could not write to file: ".concat(file_bits_x, " ! -- ").concat(ex.message));
        }
        try {
            fs.writeFileSync(file_bits_hex, bits_hex);
            console.log("Byte stream has been written to disk under name: ".concat(file_bits_hex));
        }
        catch (ex) {
            console.error("could not write to file: ".concat(file_bits_hex, " ! -- ").concat(ex.message));
        }
    };
    return random_streams_for_perf_stats;
}());
// Run the main function
random_streams_for_perf_stats.main();
// end of random_streams_for_perf_stats.ts
