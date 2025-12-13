// random_streams_for_perf_stats_rust.rs
//
// 2025-05-31, 2025-12-13: see below
//
// make on Ubuntu 24 LTS: ../Rust$ cargo new password_encryption_perf_stats
//                        ../Rust$ cd password_encryption_perf_stats
//                        ../Rust$ cargo build
// run on Ubuntu 24 LTS:  ../Rust$ ./target/debug/password_encryption
//
// build for optimized release:  ../Rust/password_encryption_perf_stats$ cargo build -r -v
//
// run on Ubuntu 24 LTS:  ../Rust/password_encryption_perf_stats$ ./target/release/password_encryption
//                                                              $ sudo perf stat -r 20 ./target/release/password_encryption
//
//
// [dependencies] --> add:
//    rand = "0.9.1"
//    rand_chacha = "0.9.0"
//    radix_fmt = "1"
//
// $ rustc -V -v
// rustc 1.88.0 (6b00bc388 2025-06-23)
// binary: rustc
// commit-hash: 6b00bc3880198600130e1cf62b8f8a93494488cc
// commit-date: 2025-06-23
// host: x86_64-unknown-linux-gnu
// release: 1.88.0
// LLVM version: 20.1.5
// $



use rand::prelude::*;
use rand_chacha::ChaCha20Rng;
use std::io::Write; // <--- bring flush() into scope
                    // https://stackoverflow.com/questions/37531903/how-do-i-print-output-without-a-trailing-newline
use std::fs::File;


fn main() {
    const END: usize = 62501;   // 62501 for exactly 1M binary digits
    // const END: usize = 100;  // for testing

    // const M1: usize = 1_000_000;
    // const K250: usize = 250_000;

    const M: usize = 65521;  // = 2^16 - 15
    const A: usize = 17364;
    const C: usize = 0;

    let file_bits_x   = "random_bitstring.bin";
    let file_bits_hex = "random_bitstring.byte";


    // let mut x: Vec<usize> = vec![0; END];
    let mut x: [usize;END] = [0;END];


    let mut bits_x   = "".to_string();
    let mut bits_hex = "".to_string();  // needed for program ENT - A Pseudorandom Number Sequence Test Program

    let mut rng = ChaCha20Rng::from_os_rng();  // https://rust-random.github.io/book/guide-seeding.html
    x[0] = rng.random_range(1..M);  // exclusive M; https://rust-random.github.io/book/quick-start.html; 2025-12-13: 1..M --> 0..M

    println!("\ngenerating a random bit stream...");
    for i in 1..END {
        x[i] = (A*x[i-1] + C) % M;
        // println!("{}", x[i]);

        // bit stream:
        let bits_x_str_a  = format!("{:#018b}", x[i-1]);
        let bits_x_str_b  = bits_x_str_a.strip_prefix("0b");       // bits_x_str_a is Option<&str> and needs unwrapping
        let bits_x_str_c  = &(bits_x_str_b).unwrap().to_string();  // bits_x_str_c is a padded string: "0000001110010000"
        bits_x           += bits_x_str_c;
        // println!("{}", bits_x);  // for testing

        // byte stream for program ENT:
        let bits_hex_str_a  = format!("{:#06x}", x[i-1]);
        let bits_hex_str_b  = bits_hex_str_a.strip_prefix("0x");
        let bits_hex_str_c  = &(bits_hex_str_b).unwrap().to_string();
        bits_hex           += bits_hex_str_c;
        // println!("{}", bits_hex);  // for testing
    }

    // up to this point: ==18709== All heap blocks were freed -- no leaks are possible


    // writing streams to files:
    match File::create(file_bits_x) {
        Ok(mut f) => {
            if let Err(e) = write!(f, "{}", bits_x) {
                eprintln!("could not write to file: {} ! -- {}", file_bits_x, e);
            } else {
                println!("Bit stream has been written to disk under name:  {}", file_bits_x);
            }
        },
        Err(e) => eprintln!("could not write to file: {} ! -- {}", file_bits_x, e)
    }

    match File::create(file_bits_hex) {
        Ok(mut f) => {
            if let Err(e) = write!(f, "{}", bits_hex) {
                eprintln!("could not write to file: {} ! -- {}", file_bits_hex, e);
            } else {
                println!("Byte stream has been written to disk under name: {}", file_bits_hex);
            }
        },
        Err(e) => eprintln!("could not write to file: {} ! -- {}", file_bits_hex, e)
    }

}

// end of random_streams_for_perf_stats_rust.rs
