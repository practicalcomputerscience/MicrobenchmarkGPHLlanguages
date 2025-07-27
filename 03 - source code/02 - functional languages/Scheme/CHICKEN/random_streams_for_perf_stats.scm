#| random_streams_for_perf_stats.scm -- this is the CHICKEN Scheme version

2025-06-27/28

build on Ubuntu 24 LTS: $ cd ./scripts/CHICKEN_Scheme/chicken-5.4.0
                        $ sudo ./chicken-install srfi-152  # for example --> this takes time
                        $ csc -O5 ./random_streams_for_perf_stats.scm
                        => -unsafe: $ csc -O5 -unsafe ./random_streams_for_perf_stats.scm
                          => no speed improvement

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats

                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

                 REPL:  $ csi
                        > (exit)

$ csc -version
CHICKEN
(c) 2008-2022, The CHICKEN Team
(c) 2000-2007, Felix L. Winkelmann
Version 5.4.0 (rev 1a1d1495)
linux-unix-gnu-x86-64 [ 64bit dload ptables ]
$

|#


(import (chicken random)   ; pseudo-random-integer
        (srfi-152)         ; for function string-concatenate from: http://api.call-cc.org/5/doc/srfi-152
)


(define END 62500)  ; for production

(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)

(define file_bits_x   "random_bitstring.bin")
(define file_bits_hex "random_bitstring.byte")

(define old-seed 0)  ; initialization is needed
(define new-seed 0)

(define x (make-vector END 0))

; define multiple ini strings in required number:
;
(define ini-string-bits_x "0000000000000000")
(define (vector-of-n-strings n str)
  (make-vector n str))  ; mutable, initialization
(define bits_x-vector (vector-of-n-strings END ini-string-bits_x))

(define ini-string-bits_hex "0000")
(define bits_hex-vector (vector-of-n-strings END ini-string-bits_hex))

; see from here: https://api.call-cc.org/5/doc/chicken/random
(set! old-seed (pseudo-random-integer m))
; (display old-seed)  ; for testing



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
    (string-append padding hex-str))) ; 2025-06-26 change from Chez Scheme


(define (write_to_file filename content)
  (call-with-current-continuation  ; equal to call/cc
    (lambda (return)
      (handle-exceptions
        ; https://api.call-cc.org/5/doc/chicken/exceptions
        ; CHICKEN's exception handling is based on the SRFI-12 exception system.
        ; This document contains the core of the SRFI-12 spec as well as CHICKEN implementation specifics.
        _
        (return #f)
        (call-with-output-file filename
          (lambda (out)
            (display content out)))
        #t))))


;
;---------------------  end of user defined functions  ----------------------


(define (main)

  (display "\ngenerating a random bit stream...")

  (let masterloop ([i 0])

    (when (< i END)
      ; (display "\n\ni = ")  ; for testing
      ; (display i)  ; for testing
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      ; (display "\nnew-seed = ")  ; for testing
      ; (display new-seed)  ; for testing
      (set! old-seed new-seed)
      (vector-set! x i new-seed)  ; set in global vector x of integer numbers

      (let ([bits_x_str (Integer_to_bin_string new-seed)]
            [bits_hex_str (Integer_to_hex_string new-seed)])
            ; (display "\nbits_x_str = ")  ; for testing
            ; (display bits_x_str)  ; for testing
            ; (display "\nbits_hex_str = ")  ; for testing
            ; (display bits_hex_str)  ; for testing

            ; fill vectors:
            (vector-set! bits_x-vector i bits_x_str)
            (vector-set! bits_hex-vector i bits_hex_str)
      )

      (masterloop (+ i 1))))  ; recursion


  ; convert vectors of strings into big strings and
  ; write them to files:
  ; (newline)  ; for testing
  (let ([bits_x   (string-concatenate (vector->list bits_x-vector))]
         ; string-concatenate from: http://api.call-cc.org/5/doc/srfi-152
        [bits_hex (string-concatenate (vector->list bits_hex-vector))])
       ; (display bits_x)    ; for testing
       ; (newline)           ; for testing
       ; (display bits_hex)  ; for testing
       ; (newline)           ; for testing

       ; write bit stream to disk:
       (if (write_to_file file_bits_x bits_x)
         (begin
           (display "\nBit stream has been written to disk under name:  ")
           (display file_bits_x))
         (begin
           (display "\ncould not write to file: ")
           (display file_bits_x)))

       ; write byte stream to disk:
       (if (write_to_file file_bits_hex bits_hex)
         (begin
           (display "\nByte stream has been written to disk under name: ")
           (display file_bits_hex))
         (begin
           (display "\ncould not write to file: ")
           (display file_bits_hex))))

  (newline)
)


; Entry point for the program:
;   this part is needed to execute the main function:
(main)

; end of random_streams_for_perf_stats.scm
