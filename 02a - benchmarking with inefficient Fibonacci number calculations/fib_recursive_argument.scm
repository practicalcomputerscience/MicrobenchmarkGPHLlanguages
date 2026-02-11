#| fib_recursive_argument.scm -- this program is for Bigloo Scheme

2026-02-11

build on Ubuntu 24 LTS: $ bigloo -call/cc fib_recursive_argument.scm -o fib_recursive_argument      # for development
                        $ bigloo -call/cc -O6 fib_recursive_argument.scm -o fib_recursive_argument  # for production

run on Ubuntu 24 LTS:   $ fib_recursive_argument <n>
                        $ time ./fib_recursive_argument <n>
                          n = 44 => fib = 701408733           => Time: 36.871s (without -O6)
                          n = 44 => fib = 701408733           => Time:  2.413s (with -O6)
                          ...
                          n = 47 => fib = 2971215073          => Time: 10.196s (with -O6)
                        

$ bigloo -version
Bigloo (4.6a)
$

|#


(module fib_recursive_argument
   (main main))


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


(define (main argv)
  ;; argv will be something like ("./my-app" "arg1" "arg2")
  (define first-arg (cadr argv))

  (define n (string->number first-arg))

  (when (>= n 2)
    (display (string-append "argument n = " (number->string n)) )
    (newline)

    (let ((start (current-milliseconds)))  ; Capture start time
      (display (fib n))
        (let ((end (current-milliseconds)))  ; Capture end time
          (let ((elapsed (/ (- end start) 1000)))  ; milliseconds -> seconds
            (printf "\nTime: ~as" elapsed))))

    (newline)))

; end of random_streams_for_perf_stats.scm
