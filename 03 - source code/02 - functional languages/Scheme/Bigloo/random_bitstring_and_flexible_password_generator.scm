#| random_bitstring_and_flexible_password_generator.scm -- this is the Bigloo Scheme version

2025-12-25

build on Ubuntu 24 LTS: $ bigloo -call/cc -O6 random_bitstring_and_flexible_password_generator.scm -o random_bitstring_and_flexible_password_generator

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator

                        $ bigloo -help

                 REPL:  $ rlwrap bigloo
                        > (exit)

$ bigloo -version
Bigloo (4.6a)
$

|#


(module random_streams_for_perf_stats
   (main main))


(define END 62500)  ; for production
; (define END 50)  ; for testing

(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)

(define file_bits_x   "random_bitstring.bin")
(define file_bits_hex "random_bitstring.byte")

(define old-seed 0)  ; initialization is needed
(define new-seed 0)

(define x (make-vector END 0))

(define n_char_default 12)

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
; (printf old-seed)  ; for testing

(define (valid_pw_length? a)
  (and (number? a) (integer? a) (exact? a) (>= a 8)))


; there variables are only defined here to keep the intended execution order of the whole program!
(define n_char 0)
(define with_special_chars #f)
(define char_set "")



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
;       (printf content out)))
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


(define (input_a_valid_number n_char)
  (printf "\nPassword of ~a printable chars OK? 'y' or another integer number >= 8: " n_char)

  (let ((answer_str (read-line)))  ; very important to evaluate (read-line) here
  ; if not, the former printf expression won't be flushed!!

    (if (equal? "y" answer_str)
      n_char  ; return n_char immediately as a good answer
      (if (valid_pw_length? (string->number answer_str))  ; valid_pw_length? is my own contract; very elegant construct
        (string->number answer_str)
        (begin
        ; begin: the expressions are evaluated in order, and the result of all but the last expr is ignored.
        ; The result from the last expr is the result of the begin form
          (printf "enter an integer number >= 8 or 'y'\n")
           (input_a_valid_number n_char))))))


(define (answer_yes_or_no)
  (printf "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  (let ((answer_str (read-line)))  ; very important to evaluate (read-line) here
    (equal? "y" answer_str)))  ; very minimalist


;; Generates a list of integers from low to high (inclusive)
;; from Google AI
(define (iota-range low high)
  (if (> low high)
      '()
      (cons low (iota-range (+ low 1) high))))

(define (string-from-range start-point end-point)
  (let* ((points (iota-range start-point end-point))
         (chars  (map integer->char points)))
    (list->string chars)))

;
;---------------------  end of user defined functions  ----------------------


(define (main xx)

  (printf "\ngenerating a random bit stream...")

  (let masterloop ([i 0])

    (when (< i END)
      ; (printf "\n\ni = " i)  ; for testing
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      ; (printf "\nnew-seed = " new-seed)  ; for testing
      (set! old-seed new-seed)
      (vector-set! x i new-seed)  ; set in global vector x of integer numbers

      ; (let ([x_i (vector-ref x i)])  ; for testing
      ;   (printf "\ni = ~a" i)    ; for testing
      ;   (printf "\nx_i = ~a" x_i))    ; for testing


      (let ([bits_x_str (Integer_to_bin_string new-seed)]
            [bits_hex_str (Integer_to_hex_string new-seed)])
            ; (printf "\nbits_x_str = " bits_x_str)  ; for testing
            ; (printf "\nbits_hex_str = " bits_hex_str)  ; for testing

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
       ; (printf bits_x "\n")  ; for testing
       ; (printf bits_hex "\")  ; for testing

       ; write bit stream to disk:
       (if (write_to_file file_bits_x bits_x)
         (begin
           (printf "\nBit stream has been written to disk under name:  " file_bits_x))
         (begin
           (printf "\ncould not write to file: " file_bits_x)))

       ; write byte stream to disk:
       (if (write_to_file file_bits_hex bits_hex)
         (begin
           (printf "\nByte stream has been written to disk under name: " file_bits_hex))
         (begin
           (printf "\ncould not write to file: " file_bits_hex))))
  (newline)


  ; ---------------------------------------------------------------------------
  ; this part is very different from the Racket solution to keep the
  ; intended execution order of the whole program!
  ; 
  ; make a password of n_char printable chars: user input requested here and now!
  (set! n_char (input_a_valid_number n_char_default))
  ; (printf "\nmain: n_char = ~a" n_char)  ; for testing

  (set! with_special_chars (answer_yes_or_no))
  ; (printf "\nmain: with_special_chars = ~a" with_special_chars)  ; for testing

  (set! char_set
    (if with_special_chars
      (string-from-range 33 126)  ; end of range is inclusive
      (string-append (string-from-range 48 57)    ; false branch
                     (string-from-range 65 90)
                     (string-from-range 97 122))))
  ; (printf "\nchar_set = ~a" char_set)  ; for testing
  ; ---------------------------------------------------------------------------



  ;------------------  recursive password creation  ---------------------------
  ;
  ; very similar to the Racket solution, but not identical! See at char0 and char1:
  (define (pw_generator n_char)
    (let pw_loop ([j 0]  ; j: char counter for x
                  [pw_chars_ ""])

      ; (define x_j (vector-ref x j))  ; for testing
      ; (printf "\n\nj = ~a" j)    ; for testing
      ; (printf "\nx_j = ~a" x_j)    ; for testing

      (define bin0 (Integer_to_bin_string (vector-ref x j)))
      ; (printf "\nbin0 = ~a" bin0)  ; for testing

      (define bin0_0 (substring bin0 0 8))  ; 8 is exclusive
      (define bin0_1 (substring bin0 8 16))
      ; (printf "\nbin0_0 = ~a -- bin0_1 = ~a" bin0_0 bin0_1)  ; for testing

      (define char0 (integer->char (string->number bin0_0 2)))
      (define char1 (integer->char (string->number bin0_1 2)))
      ; (printf "\nchar0 = ~a -- char1 = ~a" char0 char1)  ; for testing

      (define char0_add
        (if (member char0 (string->list char_set))
          (string char0)
          ""))
      ; (printf "\nchar0_add = ~a" char0_add)  ; for testing

      (define char1_add
        (if (and (member char1 (string->list char_set))
                 (< (+ 1 (string-length pw_chars_)) n_char))
          (string char1)
          ""))
      ; (printf "\nchar1_add = ~a" char1_add)  ; for testing

      (define new_pw_chars (string-append pw_chars_ char0_add char1_add))
      ; (printf "\nnew_pw_chars = ~a" new_pw_chars)  ; for testing

      (if (>= (string-length new_pw_chars) n_char)
        new_pw_chars
        (pw_loop (+ j 1) new_pw_chars))  ; recursion
    ))

  ; this is very different from the Racket solution!
  ; if done like in Racket, then the masterloop would be executed only after calling the pw_generator!!
  (let ([pw_chars (pw_generator n_char)])
    (printf "\nYour password of ~a characters is: ~a\n" n_char pw_chars))

  ; always have an expression at last for main function to have some return value:
  ; (newline)  ; for testing
)

; end of random_bitstring_and_flexible_password_generator.scm
