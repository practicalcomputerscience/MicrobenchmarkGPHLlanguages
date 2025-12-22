#| random_bitstring_and_flexible_password_generator.rkt

2025-06-20/21/27
2025-07-22: change from "#:exists 'replace" to "#:exists 'can-update": now exception behavior is same like with other Scheme dialects

run on Ubuntu 24 LTS:   $ racket random_bitstring_and_flexible_password_generator.rkt

build on Ubuntu 24 LTS: $ raco exe random_bitstring_and_flexible_password_generator.rkt
                        $ ./random_bitstring_and_flexible_password_generator

                        compile source to bytecode:
                        $ raco make random_bitstring_and_flexible_password_generator.rkt

$ racket --version
Welcome to Racket v9.0 [cs].
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

(define n_char_default 12)


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

(define (valid_pw_length? a)
  (and (number? a) (integer? a) (exact? a) (>= a 8)))



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


(define (input_a_valid_number n_char)
  (printf "\nPassword of ~a printable chars OK? 'y' or another integer number >= 8: " n_char)
  (define answer_str (read-line))

  (if (equal? "y" answer_str)
    n_char  ; return n_char immediately as a good answer
    (if (valid_pw_length? (string->number answer_str))  ; valid_pw_length? is my own contract; very elegant construct
      (string->number answer_str)
      (begin
      ; begin: the expressions are evaluated in order, and the result of all but the last expr is ignored.
      ; The result from the last expr is the result of the begin form
        (printf "enter an integer number >= 8 or 'y'\n")
         (input_a_valid_number n_char)))))


(define (answer_yes_or_no)
  (printf "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  (define answer_str (read-line))
  (equal? "y" answer_str))  ; very minimalist

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


  ; make a password of n_char printable chars: user input requested here
  (define n_char (input_a_valid_number n_char_default))
  ; (printf "\nmain: n_char = ~a" n_char)  ; for testing


  (define with_special_chars (answer_yes_or_no))
  ; (printf "\nmain: with_special_chars = ~a" with_special_chars)  ; for testing


  (define char_set
    (if with_special_chars
      (map (lambda (cp) (integer->char cp)) (for/list ([i (in-range 33 127)]) i))   ; end of range is exclusive
      (map (lambda (cp) (integer->char cp)) (append
                                              (for/list ([i (in-range 48 58)]) i)
                                              (for/list ([i (in-range 65 91)]) i)
                                              (for/list ([i (in-range 97 123)]) i)))))  ; all alphanumerical ASCII values
  ; (printf "\nchar_set = ~a" char_set)  ; for testing



  ;------------------  recursive password creation  ---------------------------
  ;
  ; similar to the Clojure solution
  (define (pw_generator n_char)
    (let pw_loop ([j 0]  ; j: char counter for x
                  [pw_chars_ ""])

      ; (define bin0 (Integer_to_bin_string (list-ref x j)))  ; old solution
      (define bin0 (Integer_to_bin_string (vector-ref x j)))  ; new solution
      ; (printf "\n\nbin0 = ~a" bin0)  ; for testing

      (define bin0_0 (substring bin0 0 8))  ; 8 is exclusive
      (define bin0_1 (substring bin0 8 16))
      ; (printf "\nbin0_0 = ~a -- bin0_1 = ~a" bin0_0 bin0_1)  ; for testing

      (define char0 (integer->char (string->number bin0_0 2)))
      (define char1 (integer->char (string->number bin0_1 2)))
      ; (printf "\nchar0 = ~a -- char1 = ~a" char0 char1)  ; for testing

      (define char0_add
        (if (member char0 char_set)
          (string char0)
          ""))

      (define char1_add
        (if (and (member char1 char_set)
                 (< (+ 1 (string-length pw_chars_)) n_char))
          (string char1)
          ""))

      (define new_pw_chars (string-append pw_chars_ char0_add char1_add))

      (if (>= (string-length new_pw_chars) n_char)
        new_pw_chars
        (pw_loop (+ j 1) new_pw_chars))  ; recursion
    ))


  (define pw_chars (pw_generator n_char))

  (printf "\nYour password of ~a characters is: ~a\n" n_char pw_chars)
  ; always have an expression at last for main function to have some return value!!

)


; Entry point for the program:
;   this part is needed to execute the main function like this for example:
;   $ racket random_bitstring_and_flexible_password_generator.rkt
(module+ main
  (main))


; end of random_bitstring_and_flexible_password_generator.rkt
