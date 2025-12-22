#| random_streams_for_perf_stats.scm -- this is the Gambit Scheme version

2025-06-26/27/28, 2025-07-01
2025-12-22: see below

build on Ubuntu 24 LTS: (don't do like this because no batteries included and old: $ sudo apt install gambc)
                        Instead, download latest zip file from: https://github.com/gambit/gambit/tags,
                        here gambit-4.9.7.tar.gz, unzip it and in the unzipped dir do:
                        $ ./configure --enable-single-host  # only this option worked OK with my system
                        $ make
                        $ make check
                        $ make modules  # optional, but I did this too
                        $ sudo make install
                        fix .bashrc for pathes to subdirs ./gsi + ./gsc in dir ./gambit-4.9.7/


                        $ gsc -exe ./random_streams_for_perf_stats.scm
                        => this is apparently as fast as this compilation:
                          gsc -exe -cc gcc -cc-options -O3 ./random_streams_for_perf_stats.scm

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

                 REPL:  $ gsi
                        > (exit)

$ gsc -v
v4.9.7 20250713105902 x86_64-pc-linux-gnu "./configure '--enable-single-host'"
$ 

|#

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


(define s (random-source-state-ref default-random-source))
(random-source-randomize! default-random-source)
; see from here: https://gambitscheme.org/latest/manual/#Pseudo-random-numbers
(set! old-seed (+ (random-integer (- m 1)) 1))  ; exact integer in the range 0 to n-1, 2025-12-22
; (display old-seed)  ; for testing



;----------------------  user defined functions  ----------------------------
;
(define (Integer_to_bin_string n)
  (let* ((binary-str (number->string n 2))  ; Convert number to binary string
        (padding (make-string (max 0 (- 16 (string-length binary-str))) #\0))) ; Create padding
    (string-append padding binary-str)))


(define (Integer_to_hex_string n)
  (let* ((hex-str (number->string n 16))    ; Convert number to hexadecimal string
        (padding (make-string (max 0 (- 4 (string-length hex-str))) #\0))) ; Create padding
    (string-append padding hex-str))) ; 2025-06-26 change from Chez Scheme


(define (write_to_file filename content)
  (call-with-current-continuation
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


(define (main)

  (display "\ngenerating a random bit stream...")

  (let masterloop ((i 0))

    (when (< i END)
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      (set! old-seed new-seed)
      (vector-set! x i new-seed)  ; set in global vector x of integer numbers

      (let ((bits_x_str (Integer_to_bin_string new-seed))
            (bits_hex_str (Integer_to_hex_string new-seed)))

            ; fill vectors:
            (vector-set! bits_x-vector i bits_x_str)
            (vector-set! bits_hex-vector i bits_hex_str)
      )

      (masterloop (+ i 1))))  ; recursion


  ; convert vectors of strings into big strings and
  ; write them to files:
  (let ((bits_x   (string-concatenate (vector->list bits_x-vector)))
        (bits_hex (string-concatenate (vector->list bits_hex-vector))))

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
