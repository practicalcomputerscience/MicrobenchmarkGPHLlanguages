/*
random_bitstring_and_flexible_password_generator.ts

2026-02-12
2026-02-15: see below

run on Ubuntu 24 LTS: $ node ./random_bitstring_and_flexible_password_generator.ts                 # running on node.js
                      $ deno --allow-write ./random_bitstring_and_flexible_password_generator.ts   # running on Deno web runtime
                      $ bun ./random_bitstring_and_flexible_password_generator.ts                  # running on Bun web runtime


$ node -v
v24.13.0
$ deno -v
deno 2.6.9
$ bun -v
1.3.9
$

*/

import * as fs from 'node:fs';  // node:fs is for deno, but OK for node.js + bun
import * as readline from "node:readline";  // node:readline is for deno, but OK for node.js + bun

class random_bitstring_and_flexible_password_generator {
    static main(): void {
        const END = 62501;  // 62501 for exactly 1M binary digits

        const m = 65521;  // = 2^16 - 15
        const a = 17364;
        const c = 0;

        const file_bits_x   = "random_bitstring.bin";
        const file_bits_hex = "random_bitstring.byte";

        const x: number[] = new Array(END);

        const rnd = Math.floor(Math.random() * (m - 1)) + 1;
        x[0] = rnd;

        let bits_x   = "";
        let bits_hex = "";

        console.log("\ngenerating a random bit stream...");
        for (let i = 1; i < END; i++) {
            x[i] = (a * x[i - 1] + c) % m;

            const bits_x_str = x[i].toString(2).padStart(16, '0');
            bits_x += bits_x_str;

            const bits_hex_str = x[i].toString(16).padStart(4, '0');
            bits_hex += bits_hex_str;
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


        // Make a password of N_CHAR printable chars: user input requested here
        let N_CHAR = 12;
        let answer = false;

        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });

        const askPasswordLength = () => {
            rl.question(`\nPassword of ${N_CHAR} printable chars OK? 'y' or another integer number >= 8: `, (answerStr) => {
                if (answerStr === 'y') {
                    answer = true;
                    askSpecialCharsUsage();
                } else {
                    const numberValue = Number(answerStr);

                    if (Number.isInteger(numberValue) && numberValue >= 8) {
                        N_CHAR = numberValue;
                        askSpecialCharsUsage();

                    } else {
                        console.log("enter an integer number >= 8 or 'y'");
                        askPasswordLength();
                    }
                }
            });
        };


        let WITH_SPECIAL_CHARS = false;
        const askSpecialCharsUsage = () => {
            rl.question("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ", (answerStr) => {
                if (answerStr === 'y') {
                    WITH_SPECIAL_CHARS = true;
                }

                // 2026-02-15:
                // I moved this needed function call to here from being the very last function call,
                // just after console.log(`\nYour...`), originally placed by Duck.ai:
                rl.close();

                charSetBuilding();
            });
        };


        // Work with a set of chars
        let char_set: Set<string> = new Set();
        const charSetBuilding = () => {
            if (WITH_SPECIAL_CHARS) {
                for (let i = 33; i <= 126; i++) {  // ASCII printable characters
                    char_set.add(String.fromCharCode(i));
                }
            } else {
                for (let i = 97; i <= 122; i++) {  // 'a' to 'z'
                    char_set.add(String.fromCharCode(i));
                }
                for (let i = 65; i <= 90; i++) {   // 'A' to 'Z'
                    char_set.add(String.fromCharCode(i));
                }
                for (let i = 48; i <= 57; i++) {   // '0' to '9'
                    char_set.add(String.fromCharCode(i));
                }
            }
            generatePassword();
        };


        const generatePassword = () => {
            let i = 0;  // char counter for the password
            let j = 0;  // counter for x
            let pw_chars = "";

            while (i < N_CHAR) {
                let bin0 = x[j].toString(2).padStart(16, '0');

                let bin0_0 = bin0.substring(0, 8);
                let bin0_1 = bin0.substring(8, 16);

                let char0 = String.fromCharCode(parseInt(bin0_0, 2));
                let char1 = String.fromCharCode(parseInt(bin0_1, 2));

                if (char_set.has(char0)) {
                    pw_chars += char0;
                    i++;
                }

                if (char_set.has(char1) && i < N_CHAR) {
                    pw_chars += char1;
                    i++;
                }

                j++;
            }

            console.log(`\nYour password of ${N_CHAR} characters is: ${pw_chars}`);
        };

        askPasswordLength();
    }
}

// Run the main function
random_bitstring_and_flexible_password_generator.main();

// end of random_bitstring_and_flexible_password_generator.ts
