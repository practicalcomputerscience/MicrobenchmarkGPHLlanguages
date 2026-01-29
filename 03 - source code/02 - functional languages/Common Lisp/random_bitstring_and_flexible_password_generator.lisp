#| random_bitstring_and_flexible_password_generator.lisp

2025-06-17/21
2025-12-21: see below

build on Ubuntu 24 LTS:
  $ sbcl --load random_bitstring_and_flexible_password_generator.lisp --eval "(sb-ext:save-lisp-and-die \"random_bitstring_and_flexible_password_generator\" \
    :executable t :toplevel #'main)"
  #' is for searching for a function name, rather than a value of the function

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ sbcl --version
SBCL 2.5.11
$

|#


(defparameter *END* 62500)  ; 62500 for exactly 1M binary digits; defining a variable
; (defparameter *END* 20)  ; for testing

(defparameter m 65521)  ; = 2^16 - 15; Scala: x(i) = (a*x(i - 1) + c) % m
(defparameter a 17364)
(defparameter c 0)

(defparameter *file_bits_x*   "random_bitstring.bin")
(defparameter *file_bits_hex* "random_bitstring.byte")

(defvar *old-seed*)  ; global var to bet set in main: old-seed to be global is very practical
(defvar *new-seed*)

(defvar *x*)  ; declare a global list variable
(defvar *bits_x*)  ; declare a global string variable
(defvar *bits_hex*)  ; declare a global string variable

(defvar *n_char*)
(defparameter n_char_default 12)
(defvar *with_special_chars*)
(defvar *char_set*)
(defvar *pw_chars*)



;----------------------  user defined functions  ----------------------------
;
(defun input_a_valid_number (n_char_)
  (format *query-io* "~%Password of ~d printable chars OK? 'y' or another integer number >= 8: " n_char_)
  (force-output *query-io*)

  (let ((answer_str (read-line *query-io*)))
       (if (string= answer_str "y")
         n_char_  ; return n_char immediately as a good answer
         (handler-case (let  ((n_char_ (parse-integer answer_str)))  ; this whole let-expression is the happy path
                         (if (>= n_char_ 8)
                           n_char_  ; return a good number
                           (progn
                             (format *query-io* "enter an integer number >= 8 or 'y'~%")
                             (force-output *query-io*)
                             (input_a_valid_number n_char_))))

           (error (c)
                 (format *query-io* "enter an integer number >= 8 or 'y'~%")
                 (force-output *query-io*)
                 (input_a_valid_number n_char_))

           ))))


(defun answer_yes_or_no ()
  (format *query-io* "~%Do you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  (force-output *query-io*)

  (let ((answer_str (read-line *query-io*)))
       (if (string= answer_str "y") t nil)))

;
;---------------------  end of user defined functions  ----------------------



(defun main ()  ; uncomment for sbcl
; (defun main (argv)  ; comment for sbcl
  ; (declare (ignore argv))  ; comment for sbcl

  (setf *random-state* (make-random-state t))  ; random seeding must be in main(): http://johnj.com/from-elegance-to-speed.html

  (setf *old-seed* (+ (random (- m 1)) 1))  ; setf macro to set value of old-seed:
  ; here should be no warning like "In MAIN: Undeclared free variable END"
  ; end is exclusive; 2025-12-21
  ; (format t "old-seed = ~d~%" *old-seed*)  ; for testing


  (format t "~%generating a random bit stream...~%")
  ;
  ;----------------------  iterative master loop  -----------------------------
  (defun masterloop ()
    (let ((bits_x_   (make-string-output-stream))  ; let for local variables with their individual declarations
          (bits_hex_ (make-string-output-stream))
          (x_ nil))  ; trick with nil!! nil represents an empty list

         (loop for i from 1 to *END* do
           ; (format t "~%i = ~a~%" i)  ; for testing
           ; (format t "old-seed  = ~a~%" *old-seed*)  ; for testing

           (setf *new-seed* (mod (+ (* a *old-seed*) c) m))
           ; (format t "new-seed     = ~a~%" *new-seed*)  ; for testing
           (setf *old-seed* *new-seed*)

           (push *new-seed* x_ )  ; growing on the LHS
           ; (format t "x_ = ~a~%" x_)  ; for testing

           (let ((bits_x_str_   (format nil "~16,'0b" *new-seed*))  ; string conversion to binary string
                 (bits_hex_str_ (format nil "~(~4,'0x~)" *new-seed*)))  ; string conversion to hexadecimal string
                 ; Tilde Left-Paren: Case Conversion
                 ; https://www.lispworks.com/documentation/HyperSpec/Body/22_cha.htm

               ; (format t "bits_x_str_ = ~a~%" bits_x_str_)  ; for testing
               ; (format t "bits_hex_str_ = ~a~%" bits_hex_str_)  ; for testing

               (write-string bits_x_str_   bits_x_)
               (write-string bits_hex_str_ bits_hex_)))

    (values (get-output-stream-string bits_x_)      ; Return values
            (get-output-stream-string bits_hex_)
            (reverse x_)))
                 ; https://lisp-docs.github.io/cl-language-reference/chap-21/cb-c-dictionary/get-output-stream-string_function
  )

  (multiple-value-bind (bits_x_loc bits_hex_loc x_loc) (masterloop)
    (setf *bits_x* bits_x_loc *bits_hex* bits_hex_loc *x* x_loc)
      ; (format t "~%x = ~d~%" *x*)  ; for testing
      ; (format t "bits_x = ~d~%" *bits_x*)  ; for testing
      ; (format t "bits_hex = ~d~%" *bits_hex*)  ; for testing
  )

  ; write bit stream to disk:
  (handler-case
    (with-open-file (stream *file_bits_x* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream *bits_x*)
      (format t "Bit stream has been written to disk under name:  ~a~%" *file_bits_x*))
    (file-error (e)
      (format t "could not write to file: ~a -- ~a~%" *file_bits_x* e)))

  ; write byte stream to disk:
  (handler-case
    (with-open-file (stream *file_bits_hex* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream *bits_hex*)
      (format t "Byte stream has been written to disk under name: ~a~%" *file_bits_hex*))
    (file-error (e)
      (format t "could not write to file: ~a -- ~a~%" *file_bits_hex* e)))



  ; make a password of n_char printable chars: user input requested here
  (multiple-value-bind (n_char_loc) (input_a_valid_number n_char_default)
    (setf *n_char* n_char_loc)
    ; (format t "~%n_char = ~d~%" *n_char*)  ; for testing
  )

  (multiple-value-bind (with_special_chars_loc) (answer_yes_or_no)
    (setf *with_special_chars* with_special_chars_loc)
    ; (format t "~%with_special_chars = ~a~%" *with_special_chars*)  ; for testing
  )


  (if *with_special_chars*
    (setf *char_set* (mapcar #'code-char (loop for i from 33 to 126 collect i)))  ; #' = searching for function name
    (setf *char_set* (mapcar #'code-char (let ((result '()))
                                            (loop for i from 48 to 57
                                                  do (setf result (append result (list i))))
                                            (loop for j from 65 to 90
                                                  do (setf result (append result (list j))))
                                            (loop for j from 97 to 122
                                                  do (setf result (append result (list j))))
                                            result))))
  ; (format t "~%char_set = ~a~%" *char_set*)  ; for testing


  ;------------------  password creation  ---------------------------
  ;
  (setf *pw_chars*
    (let ((i 0) (j 0) ; i = char counter for the password, j = counter for x
          (bin0) (bin0_0) (bin0_1)
          (char0) (char1)
          (pw_chars_ nil))
      (loop while (< i *n_char*) do
        (setf bin0 (format nil "~16,'0b" (nth j *x*)))
        ; (format t "~%~%bin0 = ~a" bin0)  ; for testing

        (setf bin0_0 (subseq bin0 0 8))
        (setf bin0_1 (subseq bin0 8 16))
        ; (format t "~%bin0_0 = ~a -- bin0_1 = ~a" bin0_0 bin0_1)  ; for testing

        (setf char0 (code-char (parse-integer bin0_0 :radix 2)))
        (setf char1 (code-char (parse-integer bin0_1 :radix 2)))
        ; (format t "~%char0 = ~a -- char1 = ~a" char0 char1)  ; for testing

        (if (member char0 *char_set*)
          (progn
            (setf pw_chars_ (concatenate 'string pw_chars_ (list char0)))
            (incf i)))

        (if (and (member char1 *char_set*) (< i *n_char*))
          (progn
            (setf pw_chars_ (concatenate 'string pw_chars_ (list char1)))
            (incf i)))

        ; (format t "~%i = ~a" i)  ; for testing
        ; (format t "~%j = ~a" j)  ; for testing
        ; (format t "~%pw_chars_ = ~a" pw_chars_)  ; for testing
        ; (format t "~%length pw_chars_ = ~a" (length pw_chars_))  ; for testing

        (incf j)

      )
      (values pw_chars_)))

  (format t "~%Your password of ~d characters is: ~a~%" *n_char* *pw_chars*)

)


; end of random_bitstring_and_flexible_password_generator.lisp

