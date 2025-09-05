; random_streams_for_perf_stats.scm -- this is the JScheme version
;
; 2025-07-01
;
; test on Ubuntu 24 LTS: - only a partly solution: only 8192 random numbers are created
;                        - no exception handling when writing to files
;                        - and super-slow! 2m28,595s for 8192 random numbers!!
;
; build on Ubuntu 24 LTS: in your local working dir, rename random_streams_for_perf_stats.scm into main.scm
;                         copy runtime.jar into the local working dir and
;                         rename it into random_streams_for_perf_stats.jar
;                         copy main.scm into random_streams_for_perf_stats.jar:
;                         $ jar -uf random_streams_for_perf_stats.jar main.scm
;
; run on Ubuntu 24 LTS:   $ java -jar random_streams_for_perf_stats.jar
;                         $ time java -jar random_streams_for_perf_stats.jar => doesn't stop within a minute!
;                         now: (define END 8192) => real	2m28,595s !!!!!!!
;
;
; $ java -jar runtime_org.jar
; JS+> (version)
; JSchemePlus version 1.4PF2
;
; Copyright (c) 2025, Pasquale Frega
; All rights reserved.
; https://pasqualefrega.antiblog.com/
;
; Released under the simplified BSD license.
;
;
; An hack of JScheme version 1.4
;
; Copyright (c) 1998, Peter Norvig
; All rights reserved.
; http://www.norvig.com/
;
; Released under [SEE LICENSE FILE]
; JS+>


; (define END 62500)  ; for production
(define END 8192)  ; for testing => 2m28,595s exe time!
; (define END 10)  ; for testing

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


(set! old-seed (random-up-to m))
; (display old-seed)  ; for testing
; (newline)  ; for testing



;----------------------  user defined functions  ----------------------------
;
(define (Integer_to_bin_string n)
  (let* ((binary-str (number->string n 2))  ; Convert number to binary string
                                            ; let* form allows later clauses to use earlier bindings
         (padding (make-string (max 0 (- 16 (string-length binary-str))) #\0))) ; Create padding
    (string-append padding binary-str)))


(define (Integer_to_hex_string n)
  (let* ((hex-str (number->string n 16))    ; Convert number to hexadecimal string
         (padding (make-string (max 0 (- 4 (string-length hex-str))) #\0))) ; Create padding
    (string-append padding hex-str))) ; 2025-06-26 change from Chez Scheme


(define (write_to_file filename content)
  (call-with-output-file filename
    (lambda (out)
      (display content out)))

  ; with procedure read-all-from-file from JSchemePlus version 1.4PF2:
  ; however, this part is only good to return #t to the calling function
  ; because in case of a problem a runtime exception will completely stop this program anyway, see below
  (let ((read_back (read-all-from-file filename)))
    ; (display "read_back = ")  ; for testing
    ; (display read_back)  ; for testing
    ; (newline)  ; for testing
    (if (string=? read_back content)  ; are both strings equal?
      #t
      #f)))

; test with random_bitstring.bin has been set to Read-Only:
;   **** ERROR: No such file: "random_bitstring.bin"
;   Exception in thread "main" java.lang.RuntimeException: No such file: "random_bitstring.bin"
;   	at SchemeUtils.error(SchemeUtils.java:88)
;   	at Primitive.openOutputFile(Primitive.java:657)
;   	at Primitive.apply(Primitive.java:458)
;   	at Scheme.eval(Scheme.java:152)
;   	at Scheme.eval(Scheme.java:125)
;   	at Scheme.eval(Scheme.java:136)
;   	at Scheme.eval(Scheme.java:125)
;   	at Scheme.eval(Scheme.java:160)
;   	at Scheme.load(Scheme.java:102)
;   	at Scheme.readEvalWriteLoop(Scheme.java:78)
;   	at Scheme.main(Scheme.java:71)


;
;---------------------  end of user defined functions  ----------------------


(define (main)

  (newline)
  (display "generating a random bit stream...")
  (newline)

  (let masterloop ((i 0))

    (if (< i END)
      (begin
        ; (newline)  ; for testing
        ; (display "i = ")  ; for testing
        ; (display i)  ; for testing
        ; (newline)  ; for testing
        (set! new-seed (modulo (+ (* a old-seed) c) m))
        ; (display "new-seed = ")  ; for testing
        ; (display new-seed)  ; for testing
        ; (newline)  ; for testing
        (set! old-seed new-seed)
        (vector-set! x i new-seed)  ; set in global vector x of integer numbers

        (let ((bits_x_str (Integer_to_bin_string new-seed))
              (bits_hex_str (Integer_to_hex_string new-seed)))
              ; (display "bits_x_str = ")  ; for testing
              ; (display bits_x_str)  ; for testing
              ; (newline)  ; for testing
              ; (display "bits_hex_str = ")  ; for testing
              ; (display bits_hex_str)  ; for testing
              ; (newline)  ; for testing

              ; fill vectors:
              (vector-set! bits_x-vector i bits_x_str)
              (vector-set! bits_hex-vector i bits_hex_str)
        )
        (masterloop (+ i 1)))))  ; recursion


  ; convert vectors of strings into big strings and
  ; write them to files:
  (let ((bits_x   (apply string-append (vector->list bits_x-vector)))
        (bits_hex (apply string-append (vector->list bits_hex-vector))))
        ; (newline)           ; for testing
        ; (display bits_x)    ; for testing
        ; (newline)           ; for testing
        ; (display bits_hex)  ; for testing
        ; (newline)           ; for testing

        ; write bit stream to disk:
        (if (write_to_file file_bits_x bits_x)
          (begin
            (display "Bit stream has been written to disk under name:  ")
            (display file_bits_x)
            (newline))
          (begin
            (display "could not write to file: ")
            (display file_bits_x)
            (newline)))

        ; write byte stream to disk:
        (if (write_to_file file_bits_hex bits_hex)
          (begin
            (display "Byte stream has been written to disk under name: ")
            (display file_bits_hex)
            (newline))
          (begin
            (display "could not write to file: ")
            (display file_bits_hex)
            (newline))))

)

; Entry point for the program:
;   this part is needed to execute the main function:
(main)

(exit)  ; don't forget, otherwise this program stops inside the JScheme REPL!

; end of random_streams_for_perf_stats.scm
