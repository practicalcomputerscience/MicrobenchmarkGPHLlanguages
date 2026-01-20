# mruby to make a standalone Ruby based app

There are a couple of documents which try to explain how to work with [mruby](https://mruby.org/):

- [mruby in GitHub](https://github.com/mruby/mruby/tree/master#mruby)
- [Executing Ruby code with mruby](https://mruby.org/docs/articles/executing-ruby-code-with-mruby.html)
- [Mruby: Beyond "hello world"](https://katafrakt.me/2024/10/05/mruby-beyond-hello-world/) from 2024 (*)

..but in the end it was Big AI (Google AI) which clearly explained me what to do.

<br/>

## Simple workflow

### Building an app specific mruby

Starting at a working directory of your project, clone mruby's GitHub repository, something which will create subdirectory _./mruby_, where the action will happen:

```
$ git clone https://github.com/mruby/mruby.git
$ cd mruby
```

There are official documents how to organize yourself potentially better, like using mruby's package manager [mrbgems](https://github.com/mruby/mruby/blob/master/doc/guides/mrbgems.md#mrbgems) "to create extensions in C and/or Ruby."

So, by using this simple workflow one gets an individual _./mruby_ subdirectory for each Ruby app. In other words, even with mruby being a "lightweight implementation of the Ruby language", it takes some time to master it in my opinion.

Anyhow, this clone could already be built as such. But before doing this, we have to add library [mruby-stringio](https://github.com/ksss/mruby-stringio) to our configuration. So, in build configuration file _./build_config/default.rb_ add line: _conf.gem :mgem => 'mruby-stringio'_ to add the StringIO class in the build.

See from [Libraries](https://mruby.org/libraries/) for an overview of mruby's libraries (or classes).

Alternatively, one could also have an individual **build configuration file**, where the _MRUBY_CONFIG_ or just _CONFIG_ environment variable is referring to. For this, you may also have a look at [mruby configuration macros](https://github.com/mruby/mruby/blob/master/doc/guides/mrbconf.md#mruby-configuration-macros), and the many possibilities for fine-tuning the building process of an app specific mruby implementation.

This environment variable is then considered at the building command _$ MRUBY_CONFIG=my_config.rb ruby ./minirake_ for example (*), or simply like this in our case:

```
$ rake  # this will take some time
```

Just for convenience, I edit and activate now my _~/.bashrc_ config file to include a path to the project's _./mruby/bin_ subdirectory, a directory where links to the just built mruby programs have been created:

```
$ ls -1 ./mruby/bin
mirb
mrbc
mrdb
mruby
mruby-config
mruby-strip
$
```

<br/>

You may also have a look at the [Compile](https://mruby.org/docs/api/file.compile.html) page, though it seems that the information there is already a bit old:

> mruby uses Rake to compile and cross-compile all libraries and binaries.

<br/>

### Soure code adaptions

The [original Ruby source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby/random_streams_for_perf_stats.rb) doesn't work here. There's apparently a high probability that "normal" Ruby source code for any sophisticated program doesn't work in mruby.

So, here's my slightly adapted version of the [speed part](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby/mruby%20for%20embedding%20a%20Ruby%20application/main.rb) of this microbenchmark program, here just named _main.rb_ (something, like other names, which can be modified to the project's needs).

<br/>

At this point, it makes totally sense to test the adapted source code with the just built mruby interpreter:

```
$ mruby ./main.rb  # location of main.rb is: ./mruby
$ time mruby ./main.rb  # => real	0m0.037s
```

At this point, two things have been tested:

a/ the successful build of an app specific mruby interpreter

b/ the source code of the app itself

<br/>

### Compiling to bytecode for mruby's virtual machine (vm)

mruby obviously has its own implementation of a vm, see at: [The new bytecode](https://github.com/mruby/mruby/blob/master/doc/internal/opcode.md#the-new-bytecode), or: [vm.c](https://github.com/mruby/mruby/blob/master/src/vm.c), and which is also under active development.

So, talking about compiling at this point in the workflow, it's about compiling source code for a specific mruby implementation into bytecode for its (specific) vm, which is then packed into a **C array** just named _app_bytecode_:

```
$ mrbc -Bapp_bytecode -o main_bytecode.c main.rb  # this is step 2
```

This command created file _main_bytecode.c_, which holds this C array and is also located in the _./mruby_ directory:

```
#include <stdint.h>
#ifdef __cplusplus
extern
#endif
const uint8_t app_bytecode[] = {
0x52,0x49,0x54,0x45,0x30,0x33,0x30,0x30,0x00,0x00,0x04,0x59,0x4d,0x41,0x54,0x5a,
...
0xff,0x45,0x4e,0x44,0x00,0x00,0x00,0x00,0x08,
};
```

<br/>

### Have a C program to define the program's entry point

Here just named [entry.c](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby/mruby%20for%20embedding%20a%20Ruby%20application/entry.c), and also located in the _./mruby_ directory:

```
#include <mruby.h>
#include <mruby/irep.h>
#include "main_bytecode.c" // The file generated in step 2

int main() {
    mrb_state *mrb = mrb_open();
    if (!mrb) return 1;

    // Load and run the compiled bytecode using the array name from Step 2
    mrb_load_irep(mrb, app_bytecode);

    if (mrb->exc) {
        mrb_print_error(mrb);
    }

    mrb_close(mrb);
    return 0;
}
```

<br/>

### Compiling the app with gcc

Finally, all parts can be conventionally compiled and linked into one standalone, native binary executable for Linux with the gcc compiler:

```
$ gcc -std=c99 -I./include entry.c -o random_streams_for_perf_stats_mruby ./build/host/lib/libmruby.a -lm
```

(actually, switch _-std=c99_ isn't really needed here)

Let's run it:

```
$ ./random_streams_for_perf_stats_mruby

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
$ time ./random_streams_for_perf_stats_mruby
...
real	0m0.037s
...
$ 
```

I also copied this executable to another, basic Linux system to see if it's really a standalone executable without any dependencies (including testing the error handling when there's a problem with writing strings to files). Yes, it is.

However, this standalone executable has the same program execution time of about 37.5 milliseconds as running it with the mruby interpreter as seen above! So, I played with my usual gcc compiler switches to see if I could get a faster executable, but to no avail. The same with playing with the clang compiler.

It looks like that embedding mruby's bytecode into a C program doesn't change the execution speed (much).

Anyhow, "transpiling" the Ruby source code of the "speed part" of the microbenchmark program into mruby source code, and its virtual machine, reduced the program execution time by around 48%, which means that this program is a little bit faster than the Perl 5 version: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

<br/>

##_end
