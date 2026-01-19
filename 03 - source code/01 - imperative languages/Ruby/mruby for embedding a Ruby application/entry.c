/* entry.c for main.rb, which is actually random_streams_for_perf_stats_mruby.rb

2026-01-19

test on Ubuntu 24 LTS: OK!!!

workflow for mruby to make a standalone, native binary executable for Linux:
  $ git clone https://github.com/mruby/mruby.git
  $ cd mruby  # change into the project dir, that is the working dir
  <edit config file ./build_config/default.rb: add there: conf.gem :mgem => 'mruby-stringio'>
  $ rake  # compile mruby
  <edit your ~/.bashrc config file to include path to the ./mruby working dir for convenience>
  $ mruby ./main.rb  # test the mruby source code before compiling it
  $ time mruby ./main.rb  # => real	0m0.037s
  ...
  $ mrbc -Bapp_bytecode -o main_bytecode.c main.rb  # compile this program to mruby bytecode (-B) in file main_bytecode.c; this is step 2
  <compile everything to a standalone, native binary executable with the gcc compiler:>
  $ gcc -std=c99 -I./include entry.c -o random_streams_for_perf_stats_mruby ./build/host/lib/libmruby.a -lm
  <-O3 etc. doesn't seem to have an effect here; same with: $ clang -O3 ...>
  $ ./random_streams_for_perf_stats_mruby  # test your app! => real	0m0.037s => this is the same speed as with: $ mruby ./main.rb
  <so, it looks like that the execution of the embedded bytecode is not being influenced by the final gcc/clang compilation>
  ...
  $
  <copy this executable file to another, basic Linux system to really test it to be a standalone exe:>
  $ ./random_streams_for_perf_stats_mruby  # test your app again
  ...  # => works! This is really a standalone, native binary executable!
  $


this workflow description started with Google AI prompt:
  "How to use mruby to compile a standalone executable; describe whole process please"

See also: https://mruby.org/docs/articles/executing-ruby-code-with-mruby.html
*/ 

#include <mruby.h>
#include <mruby/irep.h>
#include "main_bytecode.c" // The file generated in step 2

int main() {
    mrb_state *mrb = mrb_open();
    if (!mrb) return 1;

    // Load and run the compiled bytecode using the array name from step 2
    mrb_load_irep(mrb, app_bytecode);

    if (mrb->exc) {
        mrb_print_error(mrb);
    }

    mrb_close(mrb);
    return 0;
}

// end of entry.c
