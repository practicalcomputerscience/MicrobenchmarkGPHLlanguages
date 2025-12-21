#| random_streams_for_perf_stats.scm -- this is the Bigloo Scheme version

2025-06-29
2025-12-21: see below

build on Ubuntu 24 LTS: $ bigloo -call/cc -O6 random_streams_for_perf_stats.scm -o random_streams_for_perf_stats

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

                        $ bigloo -help

                        compile to JVM:
                        => not possible because:
                          The JVM back-end supports the entire Bigloo source language but the call/cc function.
                          https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter28.html#Java%20Interface

                 REPL:  $ rlwrap bigloo
                        > (exit)

$ bigloo -version
Bigloo (4.6a)
$

|#


(module random_streams_for_perf_stats
   (main main))


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

; see from here: Bigloo, A practical Scheme compiler (4.3g), User manual for version 4.3g, December 2019
(set! old-seed (+ (random (- m 1)) 1))  ; 0..m, with m is exclusive, 2025-12-21
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


; this works:
; (define (write_to_file filename content)
;   ; does file exist? --> if yes, delete it first
;   (if (file-exists? filename)
;     (begin
;       (delete-file filename)))
;
;   (call-with-output-file filename
;     (lambda (out)
;       (display content out)))
;
;   ; final test and return value true or false as a very simple test
;   (file-exists? filename))


; srfi-34 (Exception Handling for Programs): https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter31.html
; supported by the current Bigloo core library
; this is the Gambit solution:
(define (write_to_file filename content)
  (call-with-current-continuation  ; same as call/cc: compile with -call/cc switch!!
    (lambda (exit)
      (with-exception-handler
        (lambda _ (exit #f))
        (lambda ()
          (begin
            (call-with-output-file filename
              (lambda (out)
                (display content out)))
           #t))))))


;
;---------------------  end of user defined functions  ----------------------


(define (main xx)

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
  (let ([bits_x   (apply string-append (vector->list bits_x-vector))]
         ; string-concatenate from: http://api.call-cc.org/5/doc/srfi-152
        [bits_hex (apply string-append (vector->list bits_hex-vector))])
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

; end of random_streams_for_perf_stats.scm
