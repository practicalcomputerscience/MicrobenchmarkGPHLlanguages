https://ocaml.org/

https://dune.build/

---

#### Installation tips

Rename:

- _password_encryption_main.ml_ into _main.ml_ located in the _./bin_ project sub directory
- _password_encryption_perf_stats_main.ml_ into _main.ml_ located in the _./bin_ project sub directory

..here with project directories _password_encryption_ and _password_encryption_perf_stats_ respectively, each built with the Dune build tool like this for example:

```
$ dune init proj password_encryption
$ cd password_encryption
< check _main.ml_ file in project sub directory ./bin >
$ dune build
$ ./_build/default/bin/main.exe  # run program in Ubuntu 24 LTS
```
I'm not configuring special things in the _dune_ and _dune-project_ configuration files.

<br/>

Including external libraries, for example for this import in the _main.ml_ file listed below:

```
open Unix;;
```

..is done in the _dune_ configuration file which is located in the _./bin_ project sub directory:

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

##_end
