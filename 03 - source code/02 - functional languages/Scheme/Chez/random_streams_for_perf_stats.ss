#| random_streams_for_perf_stats.ss

2025-06-25
2025-12-22: see below

test on Ubuntu 24 LTS: OK, and < 50% of exe time of Racket, but still without exception handling when writing files!!

build on Ubuntu 24 LTS: $ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q  # compiling

run on Ubuntu 24 LTS:   $ petite --script random_streams_for_perf_stats.ss  <<<<<<<<<< FIRST TRY HERE TO FIX THE PROGRAM!!
                        $ petite --program random_streams_for_perf_stats.so
                        $ time petite --program random_streams_for_perf_stats.so => real	0m7,048s <<<<<<<<<<

                        $ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q --optimize-level 3
                          "to generate “unsafe” code"
                        $ time petite --program random_streams_for_perf_stats.so => real	0m4,208s <<<<<<<<<<


Standalone executable:
https://github.com/Blugatroff/selfcontained-chez
$ export SCHEME_DIRS=$(echo /usr/lib/csv10.2.0/ta6le)
$ cp ./random_streams_for_perf_stats.ss ./selfcontained-chez-main
$ cd ./selfcontained-chez-main
$ ./random_streams_for_perf_stats
Error: incompatible fasl-object version ~a found in ~a  <<<<<<<<<<<<<<<<<< ???
("0.0.0-pre-release.73" "/tmp/bootfileswU2mk")
Aborted (core dumped)
$


to-do:
  - write files: better exception handling??


|#


(import (chezscheme))  ; for random


(define END 62500)  ; for production; 62500 for exactly 1M binary digits
; (define END   10)  ; for testing


(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)

(define file_bits_x   "random_bitstring.bin")
(define file_bits_hex "random_bitstring.byte")

(define old-seed 0)  ; initialization is needed
(define new-seed 0)

(define x '())  ; empty list


; define multiple ini strings in required number:
;
(define ini-string-bits_x "0000000000000000")
(define (vector-of-n-strings n str)
  (make-vector n str))  ; mutable, initialization
(define bits_x-vector (vector-of-n-strings END ini-string-bits_x))

(define ini-string-bits_hex "0000")
(define bits_hex-vector (vector-of-n-strings END ini-string-bits_hex))

(set! old-seed (+ (random (- m 1)) 1))  ; 0..m, with m is exclusive, 2025-12-22



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
    (string-downcase (string-append padding hex-str))))


(define (write_to_file filename content)
  ; does file exist? --> if yes, delete it first, otherwise call-with-output-file will throw an exception!
  (if (file-exists? filename)
    (begin
      (delete-file filename)))

  (call-with-output-file filename
    (lambda (out)
      (display content out)))

  ; final test and return value true or false as a very simple test
  (file-exists? filename))

;
;---------------------  end of user defined functions  ----------------------



(define (main)

  (display "\ngenerating a random bit stream...")

  ;----------------------  recursive master loop  -----------------------------
  (let masterloop ([i 0])

    (when (< i END)
      ; (display "\n\ni = ")  ; for testing
      ; (display i)  ; for testing
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      ; (display "\nnew-seed = ")  ; for testing
      ; (display new-seed)  ; for testing
      (set! old-seed new-seed)
      (set! x (append x (list new-seed)))  ; append to global list x

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


; Entry point for the program:
;   this part is needed to execute the main function:
(main)


; end of random_streams_for_perf_stats.ss
