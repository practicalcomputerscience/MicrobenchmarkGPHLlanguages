#| random_streams_for_perf_stats.rkt

2025-06-27
2025-07-22: change from "#:exists 'replace" to "#:exists 'can-update": now exception behavior is same like with other Scheme dialects

run on Ubuntu 24 LTS:   $ racket random_streams_for_perf_stats.rkt

build on Ubuntu 24 LTS: $ raco exe random_streams_for_perf_stats.rkt
                        $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

                        compile source to bytecode:
                        $ raco make random_streams_for_perf_stats.rkt

$ racket --version
Welcome to Racket v8.17 [cs].
$

|#


#lang racket/base        ; this is supposed to make a little faster solution
(require racket/string)  ; for string-join function


(define END 62500)  ; for production
; (define END 20)  ; for testing

(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)

(define file_bits_x   "random_bitstring.bin")
(define file_bits_hex "random_bitstring.byte")

(define old-seed 0)  ; initialization is needed
(define new-seed 0)

; (define x '())  ; empty list: old solution with real	0m9,854s
(define x (make-vector END 0))


; define multiple ini strings in required numbers:
;
; (define v (make-vector 3 2))  ; OK!?! here numbers are used directly
;
(define ini-string-bits_x "0000000000000000")
(define (vector-of-n-strings n str)
  (make-vector n str))  ; mutable, initialization
(define bits_x-vector (vector-of-n-strings END ini-string-bits_x))

(define ini-string-bits_hex "0000")
(define bits_hex-vector (vector-of-n-strings END ini-string-bits_hex))

(set! old-seed (random 1 m))



;----------------------  user defined functions  ----------------------------
;
(define (Integer_to_bin_string n)
  (let* ([binary-str (number->string n 2)]  ; Convert number to binary string
                                            ; let* form allows later clauses to use earlier bindings
         [padding (make-string (max 0 (- 16 (string-length binary-str))) #\0)]) ; Create padding
    (string-append padding binary-str)))


(define (Integer_to_hex_string n)
  (let* ([hex-str (number->string n 16)]    ; Convert number to hexadecimal string
         [padding (make-string (max 0 (- 4 (string-length hex-str))) #\0)]) ; Create padding
    (string-append padding hex-str)))


(define (write_to_file filename content)
  (with-handlers ([exn:fail? (lambda _ #f)]) ; Return #f to indicate failure
    (call-with-output-file filename
      (lambda (out)
        (display content out)) ; Write the string to the file
      #:exists 'can-update) ; Replace the file if it already exists, but not when permission is "Read-Only"
    #t))  ; Return #t to indicate success


;
;---------------------  end of user defined functions  ----------------------



(define (main)

  (printf "\ngenerating a random bit stream...")

  ;----------------------  recursive master loop  -----------------------------
  (let masterloop ([i 0])

    (when (< i END)
      ; (printf "\ni = ~a" i)  ; for testing
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      ; (printf "\nnew-seed = ~a" new-seed)  ; for testing
      (set! old-seed new-seed)
      ; (set! x (append x (list new-seed)))  ; append to global list x: old solution
      (vector-set! x i new-seed)  ; set in global vector x of integer numbers

      (let ([bits_x_str (Integer_to_bin_string new-seed)]
            [bits_hex_str (Integer_to_hex_string new-seed)])
            ; (printf "\nbits_x_str = ~a"   bits_x_str)  ; for testing

            ; fill vectors:
            (vector-set! bits_x-vector i bits_x_str)
            (vector-set! bits_hex-vector i bits_hex_str)
      )

      (masterloop (+ i 1))))  ; recursion

  ; convert vector of strings into one big string:
  (define bits_x (string-join (vector->list bits_x-vector) ""))
  ; (printf "\nbits_x = ~a" bits_x)  ; for testing
  (define bits_hex (string-join (vector->list bits_hex-vector) ""))

  (define success1? (write_to_file file_bits_x bits_x))
  (if success1?
    (printf "\nBit stream has been written to disk under name:  ~a" file_bits_x)
    (printf "\ncould not write to file: ~a !" file_bits_x))

  ; write byte stream to disk:
  (define success2? (write_to_file file_bits_hex bits_hex))
  (if success2?
    (printf "\nByte stream has been written to disk under name: ~a" file_bits_hex)
    (printf "\ncould not write to file: ~a !" file_bits_hex))

  (printf "\n")
)


; Entry point for the program:
;   this part is needed to execute the main function:
(module+ main
  (main))

; end of random_streams_for_perf_stats.rkt
