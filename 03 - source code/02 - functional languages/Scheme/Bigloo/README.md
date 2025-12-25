2025-12-25: TBD: something to say about the full program _random_bitstring_and_flexible_password_generator.scm_ <=> Racket solution

# Bigloo Scheme

https://www-sop.inria.fr/indes/fp/Bigloo/

---

Bigloo Scheme compiles to very small binary executables, for example like this:

```
$ bigloo -call/cc -O6 random_streams_for_perf_stats.scm -o random_streams_for_perf_stats
```

..with switch _-call/cc_ used for procedure [call-with-current-continuation](https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter5.html#Control%20Features):

> Note: Since call/cc is difficult to compile efficiently, one might consider using bind-exit instead. For this reason, we decided to enable call/cc only with a compiler option. 

..and switch _-O6_ used for best opimization: [The Bigloo command line](https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter32.html#G49542)

<br/>

Enter the REPL (Read-Eval-Print Loop) like this: _$ rlwrap bigloo_ and give command _(exit)_ to exit it.

##_end
