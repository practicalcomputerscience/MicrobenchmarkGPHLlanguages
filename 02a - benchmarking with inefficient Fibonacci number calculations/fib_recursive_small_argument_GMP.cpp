/* fib_recursive_small_argument_GMP.cpp

2026-02-10

this is a super-slow recursive solution, the naive solution, but for testing purposes


test on Ubuntu 24 LTS: OK

build:              $ g++ fib_recursive_small_argument_GMP.cpp -o fib_recursive_small_argument_gmp_g++ -lgmpxx -lgmp       # for development
                    $ g++ -O3 fib_recursive_small_argument_GMP.cpp -o fib_recursive_small_argument_gmp_g++  -lgmpxx -lgmp  # for production


output:
  $ ./fib_recursive_small_argument_gmp_g++ <n>

  n = 47 => fib = 2971215073          => Time: 115.439s
  

$ g++ --version
g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$

*/


#include <iostream>
#include <chrono>
#include <gmpxx.h> // Include the GMP C++ wrappers


class FibArgument {
public:
    static void main(int argc, char* argv[]) {
        if (argc < 2) {
            std::cout << "Please provide an argument for n." << std::endl;
            return;
        }

        // unsigned long long n = std::atoi(argv[1]); // Convert argument to integer
        int n = std::atoi(argv[1]); // Convert argument to integer
        if (n < 2 || n > 93) {
            std::cout << "Please provide an argument of 2 <= n <= 93" << std::endl;
            return;
        }

        std::cout << "argument n = " << n << std::endl;

        auto start_time = std::chrono::high_resolution_clock::now();

        mpz_class result = fib(n);  // Use mpz_class for large integers
        // unsigned long long result = fib(n);
        std::cout << result << std::endl;

        auto end_time = std::chrono::high_resolution_clock::now();

        std::chrono::duration<double> elapsed = end_time - start_time;
        std::cout << "Time: " << elapsed.count() << "s" << std::endl;
    }

    // static unsigned long long fib(unsigned long long n) {
    //     if (n <= 1) return n;
    //     return fib(n - 1) + fib(n - 2);
    // }
    static mpz_class fib(int n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }
    
};


int main(int argc, char* argv[]) {
    FibArgument::main(argc, argv);
    return 0;
}

// end of fib_recursive_small_argument_GMP.cpp
