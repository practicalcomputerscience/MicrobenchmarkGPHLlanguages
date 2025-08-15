#| random_streams_for_perf_stats.scm -- this is the Gambit Scheme version

2025-06-26/27/28

test on Ubuntu 24 LTS: OK and super-speedy with 0,040 sec!!!!!
                       checks done OK at: https://mzsoltmolnar.github.io/random-bitstream-tester/

build on Ubuntu 24 LTS: (don't do like this because no batteries included and old: $ sudo apt install gambc)
                        Instead, download this zip file:
                        https://github.com/gambit/gambit/tree/6b898fc90c0a2842093d9c92cd6c30be329c4cea
                        $ ./configure --enable-single-host  # only this option worked OK with my system
                        $ make
                        $ make check
                        $ make modules  # optional, but I did this too
                        $ sudo make install
                        fix .bashrc for pathes to ./gsi + ./gsc

                        $ gsc -exe ./random_streams_for_perf_stats.scm


run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats ==>real	0m0,036s!!!
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats
                        => 0,039638 +- 0,000614 seconds time elapsed  ( +-  1,55% )


                 REPL:  $ gsi
                        > (exit)


to-do:
  - write files: better exception handling??

$ gsc -v
v4.9.6 20250310181758 x86_64-pc-linux-gnu "./configure '--enable-single-host'"
$


|#


; (import (srfi 152))  ; for string-concatenate  --> "...Operator is not a PROCEDURE (#!unbound 152)"

; (load "srfi 13")



; (define END 62500)  ; for production: old version: handle everything in one batch => very slow performance only with >> 1 sec
(define END 8192)  ; for testing => real	0m0,200s: limit of arguments for append function in Gambit Scheme:
                   ; https://gambitscheme.org/latest/manual/#System-limitations
                   ; https://github.com/gambit/gambit/blob/6b898fc90c0a2842093d9c92cd6c30be329c4cea/lib/mem.h

(define END2 5156) ; 7 * 8192 + 5156 = 62500

(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)

(define file_bits_x   "random_bitstring.bin")
(define file_bits_hex "random_bitstring.byte")

(define old-seed 0)  ; initialization is needed
(define new-seed 0)


(define x0 (make-vector END 0))  ; first vector of random integer numbers:
                                 ;   also to have a vector here seems to be a much faster solution!
(define x1 (make-vector END 0))
(define x2 (make-vector END 0))
(define x3 (make-vector END 0))
(define x4 (make-vector END 0))
(define x5 (make-vector END 0))
(define x6 (make-vector END 0))
(define x7 (make-vector END2 0))


; define multiple ini strings in required number:
;
(define (vector-of-n-strings n str)
  (make-vector n str))  ; mutable, initialization

(define ini-string-bits_x "0000000000000000")
(define bits_x-vector0 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector1 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector2 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector3 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector4 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector5 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector6 (vector-of-n-strings END ini-string-bits_x))
(define bits_x-vector7 (vector-of-n-strings END2 ini-string-bits_x))

(define ini-string-bits_hex "0000")
(define bits_hex-vector0 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector1 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector2 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector3 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector4 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector5 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector6 (vector-of-n-strings END ini-string-bits_hex))
(define bits_hex-vector7 (vector-of-n-strings END2 ini-string-bits_hex))


(define s (random-source-state-ref default-random-source))
(random-source-randomize! default-random-source)
; see from here: https://gambitscheme.org/latest/manual/#Pseudo-random-numbers
(set! old-seed (random-integer m))  ; 2025-06-26: change from Chez Scheme + Gambit Scheme
; (display old-seed)  ; for testing => this is always the same number!!



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
  ; does file exist? --> if yes, delete it first, otherwise call-with-output-file will throw an exception!
  (if (file-exists? filename)
    (begin
      (delete-file filename)))

  (call-with-output-file filename
    (lambda (out)
      (display content out)))

  ; final test and return value true or false as a very simple test
  (file-exists? filename))


;----------------------  recursive master loop  -----------------------------
(define (masterloop x_ vct_of_str_bin vct_of_str_hex end_)

  (let loop ((i 0))

    (when (< i end_)
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      (set! old-seed new-seed)

      (vector-set! x_ i new-seed)  ; set in global vector x of integer numbers

      (let ((bits_x_str (Integer_to_bin_string new-seed))
            (bits_hex_str (Integer_to_hex_string new-seed)))

            ; fill vectors of strings:
            (vector-set! vct_of_str_bin i bits_x_str)
            (vector-set! vct_of_str_hex i bits_hex_str))

      (loop (+ i 1)))))  ; recursion


;
;---------------------  end of user defined functions  ----------------------



(define (main)

  (display "\ngenerating a random bit stream: working in 8 batches...")

  ; filling global variables:
  (masterloop x0 bits_x-vector0 bits_hex-vector0 END)
  (masterloop x1 bits_x-vector1 bits_hex-vector1 END)
  (masterloop x2 bits_x-vector2 bits_hex-vector2 END)
  (masterloop x3 bits_x-vector3 bits_hex-vector3 END)
  (masterloop x4 bits_x-vector4 bits_hex-vector4 END)
  (masterloop x5 bits_x-vector5 bits_hex-vector5 END)
  (masterloop x6 bits_x-vector6 bits_hex-vector6 END)
  (masterloop x7 bits_x-vector7 bits_hex-vector7 END2)


  ; binary string
  ;   make a big let here to be more compatible to other Scheme dialects:
  ;   reason: no define is allowed inside a function in Gambit Scheme for example!
  (let* ((bits_x0 (apply string-append (vector->list bits_x-vector0)))
         (bits_x1 (apply string-append (vector->list bits_x-vector1)))
         (bits_x2 (apply string-append (vector->list bits_x-vector2)))
         (bits_x3 (apply string-append (vector->list bits_x-vector3)))
         (bits_x4 (apply string-append (vector->list bits_x-vector4)))
         (bits_x5 (apply string-append (vector->list bits_x-vector5)))
         (bits_x6 (apply string-append (vector->list bits_x-vector6)))
         (bits_x7 (apply string-append (vector->list bits_x-vector7))))

    ; make one big and final binary string:
    (let*  ((bits_x (string-append bits_x0 bits_x1 bits_x2 bits_x3 bits_x4 bits_x5 bits_x6 bits_x7)))
      ; write bit stream to disk:
      (if (write_to_file file_bits_x bits_x)
        (begin
          (display "\nBit stream has been written to disk under name:  ")
          (display file_bits_x))
        (begin
          (display "\ncould not write to file: ")
          (display file_bits_x)))))

  ; hexadecimal string
  (let* ((bits_hex0 (apply string-append (vector->list bits_hex-vector0)))
         (bits_hex1 (apply string-append (vector->list bits_hex-vector1)))
         (bits_hex2 (apply string-append (vector->list bits_hex-vector2)))
         (bits_hex3 (apply string-append (vector->list bits_hex-vector3)))
         (bits_hex4 (apply string-append (vector->list bits_hex-vector4)))
         (bits_hex5 (apply string-append (vector->list bits_hex-vector5)))
         (bits_hex6 (apply string-append (vector->list bits_hex-vector6)))
         (bits_hex7 (apply string-append (vector->list bits_hex-vector7))))

    (let* ((bits_hex (string-append bits_hex0 bits_hex1 bits_hex2 bits_hex3 bits_hex4 bits_hex5 bits_hex6 bits_hex7)))
      ; write byte stream to disk:
      (if (write_to_file file_bits_hex bits_hex)
        (begin
          (display "\nByte stream has been written to disk under name: ")
          (display file_bits_hex))
        (begin
          (display "\ncould not write to file: ")
          (display file_bits_hex)))))

  (newline)
)


; Entry point for the program:
;   this part is needed to execute the main function:
(main)


; end of random_streams_for_perf_stats.scm
