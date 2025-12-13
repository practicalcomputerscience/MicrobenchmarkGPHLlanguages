// random_bitstring_and_flexible_password_generator.rs
//
// 2025-05-07/08/17/19/22/31, 2025-07-19, 2025-12-13: see below
//
//
// make on Ubuntu 24 LTS: ../Rust$ cargo new password_encryption
//                        ../Rust$ cd password_encryption
//                        ../Rust$ cargo build
// run on Ubuntu 24 LTS:  ../Rust$ ./target/debug/password_encryption
//
// build for optimized release:  ../Rust/password_encryption$ cargo build -r -v
//                                            in case of doubt: $ cargo clean  # ..and then again $ cargo build -r -v
//
// run on Ubuntu 24 LTS:  ../Rust/password_encryption$ ./target/release/password_encryption
//
//
// [dependencies] --> add:
//    rand = "0.9.1"
//    rand_chacha = "0.9.0"
//    radix_fmt = "1"
//
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
use std::io;
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
    x[0] = rng.random_range(1..M);  // exclusive M; https://rust-random.github.io/book/quick-start.html; 2025-12-13: 0..M --> 1..M

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



    // make a password of n_char printable chars: user input requested here
    let mut n_char = 12;
    let mut answer: bool = false;
    let mut answer_str = String::new();

    while !answer {
      // n_char = 12;
      let q1_str = format!("\nPassword of {} printable chars OK? 'y' or another integer number >= 8: ", n_char.to_string());
      _ = print!("{}", q1_str);  // print no new line here at the end of printing the string
      io::stdout().flush().unwrap();  // flush this output string

      answer_str.clear();  // if not, answer_str would grow with every new line input
      _ = io::stdin().read_line(&mut answer_str);  // reading user input: no "?" here when main function has no result handling
      // println!("answer_str ={}---", answer_str);  // for testing: the "---" is printed in a new line!

      answer_str.pop();  // remove trailing newline char
      if answer_str == "y" {
        answer = true;
      } else {
        // println!("n_char ={}---", n_char);  // for testing
        // n_char = i32::from_str(&answer_str).unwrap();  // here can be panic!

        match answer_str.parse::<i32>() {
          Ok(n)  => {
            if n < 8 {
                 println!("enter an integer number >= 8 or 'y'");
            } else {
                 n_char = n;
                 answer = true;
            }
          },

          Err(_) => {
            println!("enter an integer number >= 8 or 'y'");
          }
        }
      }
    }
    // println!("n_char ={}---", n_char);  // for testing


    let mut with_special_chars: bool = true;
    answer = false;
    while !answer {
      let q1_str = format!("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
      _ = print!("{}", q1_str);  // print no new line here at the end of printing the string
      io::stdout().flush().unwrap();  // flush this output string

      answer_str.clear();  // if not, answer_str would grow with every new line input


      // up to this point: valgrind: All heap blocks were freed -- no leaks are possible
      _ = io::stdin().read_line(&mut answer_str);  // reading user input: no "?" here when main function has no result handling
      // valgrind: ...HEAP SUMMARY:  ==19435==     in use at exit: 8,192 bytes in 1 blocks
      // https://www.reddit.com/r/rust/comments/u9gx5t/stdiostdin_leaking_memory_says_memcheck/
      // not to worry here too much --> wait on a valgrind fix


      answer_str.pop();  // remove trailing newline char
      if answer_str == "y" {
        answer = true;
      } else {
        with_special_chars = false;
        answer = true;
      }
    }

    let mut char_set = "".to_string();      // cast from type &str to type string
    if with_special_chars {
      for i in 33..127 {
        char_set.push_str(&(char::from_u32(i)).unwrap().to_string());
      }
    } else {
      char_set.push_str("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
    }
    // println!("{}",char_set);  // for testing


    let mut i = 0;  // char counter for the password
    let mut j = 0;  // char counter for x
    let mut pw_chars = "".to_string();

    while i < n_char {
      let bin0 = format!("{:#018b}", x[j]);  // bin0 is a padded string: "0b0000001110010000"
      // https://doc.rust-lang.org/std/string/struct.String.html
      let bin0a = bin0.strip_prefix("0b");  // bin0a is Option<&str> and needs unwrapping
      let bin0b = &(bin0a).unwrap().to_string();  // bin0 is a padded string: "0000001110010000"
      // println!("{}", bin0b);

      let bin0_0 = &bin0b[0..8];
      let bin0_1 = &bin0b[8..16];
      // println!("{}  {}", bin0_0, bin0_1);

      let int0_0 = u32::from_str_radix(bin0_0, 2).unwrap();
      let int0_1 = u32::from_str_radix(bin0_1, 2).unwrap();
      // println!("{}  {}", int0_0, int0_1);

      let char0 = &(char::from_u32(int0_0)).unwrap().to_string();
      let char1 = &(char::from_u32(int0_1)).unwrap().to_string();
      // println!("{}  {}", char0, char1);

      if char_set.contains(char0) {  // char_set is of type string
        pw_chars.push_str(&char0);
        i += 1;
      }

      if char_set.contains(char1) && i < n_char {
        pw_chars.push_str(&char1);
        i += 1;
      }

      j += 1;
    }

    println!("\nYour password of {n_char} characters is: {pw_chars}\n")

}

// end of random_bitstring_and_flexible_password_generator.rs
