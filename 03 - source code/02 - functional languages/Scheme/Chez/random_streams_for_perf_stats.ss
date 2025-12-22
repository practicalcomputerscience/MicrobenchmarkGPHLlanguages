#| random_streams_for_perf_stats.ss -- this is the Chez Scheme version

2025-06-25
2025-12-22: see below

test on Ubuntu 24 LTS: OK

build on Ubuntu 24 LTS: $ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q  # compiling

run on Ubuntu 24 LTS:   $ petite --script random_streams_for_perf_stats.ss  <<< FIRST TRY THIS TO TEST THE PROGRAM!!
                        $ petite --program random_streams_for_perf_stats.so
                        $ time petite --program random_streams_for_perf_stats.so => real	0m0.709s

                        $ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q --optimize-level 3
                          "to generate “unsafe” code"
                        $ time petite --program random_streams_for_perf_stats.so => real	0m0.668s
                          => this seems to be the fastest solution of all I tried


$ petite --version
10.3.0
$


------
Make a standalone executable: compilation with compile2.scm (my slight adaption) is working:
Download zip file from: https://github.com/Blugatroff/selfcontained-chez --> selfcontained-chez-main.zip
Extract this zip file into dir: ./selfcontained-chez-main
$ export SCHEME_DIRS=$(echo /usr/lib/csv10.3.0/pb)  # this is from: ./configure; make; sudo make install: from csv10.3.0.tar.gz
# pb = portable bytecode
$ cp ./random_streams_for_perf_stats.ss ./selfcontained-chez-main  # copy this file into this new dir
$ cd ./selfcontained-chez-main  # change into this new dir
$ ./compile2.scm ./random_streams_for_perf_stats.ss  # compile this source code file
$ time ./random_streams_for_perf_stats => real	0m0.772s
------

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

(define x (make-vector END 0))  ; essential for a fast program, 2025-12-22


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


; tests: the CHICKEN, Bigloo, Gambit and Racket solutions do not work here (as such):
(define (write_to_file filename content)
  ; does file exist? --> if yes, delete it first if not in read-only mode
  (if (file-exists? filename)
    ; files exists:
    ;   first, check if file is read-only:
    (let ([mode (get-mode filename)])  ; Big AI solution
      ;; Check for the owner-write bit (0o200 or 128 in decimal)
      ;; Use logand for bitwise AND comparison
      (if (not (zero? (logand mode #o200)))
          ; file is not read-only and can be deleted and created again
          ; otherwise, call-with-output-file will throw an exception!
          (begin
            (delete-file filename)
            (call-with-output-file filename
              (lambda (out)
                (display content out)))
            ; final test and return value true or false as a very simple test
            (file-exists? filename)
          )
          ; else:
          (begin
            (newline)
            (display filename)
            (display ": this file is read-only and won't be overwritten.")
            #f)))

    ; file doesn't exist:
    (begin
      (call-with-output-file filename
        (lambda (out)
          (display content out)))
      ; final test and return value true or false as a very simple test
      (file-exists? filename))
  )
)


; 2025-12-22, alternatively: solution from Google AI:
;(define (write_to_file filename content)
;  (guard (con
;          (else
;           ; (display "An error occurred during file writing: ")
;           ; (display-condition con) ;; Displays specific Chez Scheme error details
;           ; (newline)
;           #f)) ;; Return false if an error occurs
;    (with-output-to-file filename
;      (lambda () (display content))
;      'replace) ;; Replaces file if it exists; use 'error to fail if exists
;                ;; this overwrites a read-only file. So, this behavior is a little bit different from the other versions.
;    #t)) ;; Return true if successful


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
      (vector-set! x i new-seed)  ; set in global vector x of integer numbers: essential for a fast program, 2025-12-22

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
