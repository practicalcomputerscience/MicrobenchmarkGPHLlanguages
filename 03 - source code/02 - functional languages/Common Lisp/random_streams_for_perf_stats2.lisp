#| random_streams_for_perf_stats2.lisp

2025-06-16/17/21, 2025-07-02
2025-12-21: see below

build on Ubuntu 24 LTS:
  $ sbcl --load random_streams_for_perf_stats2.lisp --eval "(sb-ext:save-lisp-and-die \"random_streams_for_perf_stats2\" \
    :executable t :toplevel #'main)"

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats2
                        $ time ./random_streams_for_perf_stats2
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats2


$ sbcl --version
SBCL 2.5.11
$

|#


(defparameter *END* 62500)  ; 62500 for exactly 1M binary digits; defining a variable
; (defparameter *END* 10)  ; for testing ##############################################################################################

(defparameter m 65521)  ; = 2^16 - 15; Scala: x(i) = (a*x(i - 1) + c) % m
(defparameter a 17364)
(defparameter c 0)

(defparameter *file_bits_x*   "random_bitstring.bin")  ; declaring a string variable
(defparameter *file_bits_hex* "random_bitstring.byte")

(defvar *old-seed*)  ; global var to bet set in main: old-seed needs to be global
(defvar *new-seed*)


(defvar *x*)  ; define a global list variable
(defvar *bits_x*)  ; define a global string variable
(defvar *bits_hex*)  ; define a global string variable


(defun main ()  ; defun = standard function-defining macro
  ; (declare (ignore argv))

  (setf *random-state* (make-random-state t))  ; random seeding must be in main(): http://johnj.com/from-elegance-to-speed.html

  (setf *old-seed* (+ (random (- m 1)) 1))  ; setf macro to set value of old-seed:
  ; here should be no warning like "In MAIN: Undeclared free variable END"
  ; end is exclusive; 2025-12-21
  ; (format t "old-seed = ~d~%" *old-seed*)  ; for testing


  (format t "~%generating a random bit stream...~%")
  ;
  ;----------------------  iterative master loop  -----------------------------
  (defun masterloop ()
    (let ((bits_x_   (make-string-output-stream))
          (bits_hex_ (make-string-output-stream))
          (x_ nil))  ; trick with nil!! nil represents an empty list

         (loop
           for i from 1 to *END* do
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

)

; end of random_streams_for_perf_stats2.lisp
