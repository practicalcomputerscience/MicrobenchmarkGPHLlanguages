# OCaml

https://ocaml.org/

https://dune.build/

---

Table of contents:

- [Installation tips](#installation-tips)
- [OCaml on the Java Virtual Machine (JVM)](#ocaml-on-the-java-virtual-machine-jvm)

---

### Installation tips

Rename:

- _password_encryption_main.ml_ into _main.ml_ located in the _./bin_ project subdirectory
- _password_encryption_perf_stats_main.ml_ into _main.ml_ located in the _./bin_ project subdirectory

..here with project directories _password_encryption_ and _password_encryption_perf_stats_ respectively, each built with the Dune build tool like this for example:

```
$ dune init proj password_encryption
$ cd password_encryption  # check main.ml file in project subdirectory ./bin
$ dune build
$ ./_build/default/bin/main.exe  # run program in Ubuntu 24 LTS
```
I'm not configuring special things in the _dune_ and _dune-project_ configuration files.

<br/>

Including external libraries, for example for this import in the _main.ml_ file listed below:

```
open Unix;;
```

..is done in the _dune_ configuration file which is located in the _./bin_ project subdirectory:

```
(executable
 (public_name unix_time)
 (name main)
 (libraries unix mtime mtime.clock.os))
```

..for this program for reading the monotonic operating system time:

```
open Unix;;

let main () =
  let t0 = localtime(time ()) in  (* t0: type tm --> https://www.man7.org/linux/man-pages/man3/tm.3type.html *)
  let t0a = t0.tm_sec in  (* t0a: type int *)
  Printf.printf "t0a = %d in sec \n" t0a;;
  
  (* https://github.com/dbuenzli/mtime/blob/master/test/min_clock.ml *)
  (* Format.printf "Timestamp: %a@." Mtime.pp (Mtime_clock.now ()); *)
  (* pp = pretty print *)
  (* Timestamp: 41060878490720ns *)
  
  let t10 = Mtime_clock.now_ns () in  (* () for getting rid of type unit -> int64 error *)
  (* https://github.com/c-cube/playground/blob/4fffb47cc226b624bea8d8a55df06fc453935ad5/mtime_rdtsc_bench/main.ml#L7 *)
  
  Printf.printf "Current monotonic time: %Ld in ns\n" t10;
  (*Current monotonic time: 46057558754772
    t0a = 58 in sec*)
    
  let t11a: int64 = Int64.div t10 1_000_000L in
  (*https://www.typeerror.org/docs/ocaml/libref/int64*)
  let t11b: int = Int64.to_int t11a in
  Printf.printf "Current monotonic time: %d in ms\n" t11b;;
  (* Current monotonic time: 46792836209233 in ns
     Current monotonic time: 46792836 in ms
     t0a = 13 in sec*)

main ();;
```

<br/>

### OCaml on the Java Virtual Machine (JVM)

When I had a look at what functional programming languages are available on the JVM: https://en.wikipedia.org/wiki/List_of_JVM_languages, I found OCaml: http://www.ocamljava.org/index.html

This version is pretty old from 2015 and abandoned, but I gave it a try.

I downloaded the last binary build to avoid compiling it by my own: http://www.ocamljava.org/downloads/ and set the path to the compiler binaries in my
_.bashrc_ config file (temporarily):

```
export PATH="$PATH:$HOME/scripts/OCaml-Java/ocamljava-2.0-alpha3-bin/bin/"
```

I checked my installation with:

```
$ ocamljava -v
... The OCaml Java-bytecode compiler, version 4.01.0 ... OCaml-Java version: 2.0-alpha3 ... Standard library directory: /ocamljava-2.0-alpha3/lib/ocaml
```

Then I compiled my original and working [OCaml source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/password_encryption_perf_stats_main.ml), which is normally meant to be compiled into a binary executable:

```
$ ocamljava -c password_encryption_perf_stats_main.ml
```

..and linked it into an "uberJAR" file (same like with Scala and Kotlin for example):

```
$ ocamljava -o password_encryption_perf_stats_main.jar password_encryption_perf_stats_main.cmj
```

..with Java version:

```
$ java -version
... OpenJDK Runtime Environment (build 21.0.7...) ...
$
```

..and ran it on the JVM:

```
$ time java -jar password_encryption_perf_stats_main.jar
```

Result: 724 [milliseconds] 

Not bad.

<br/>

##_end
