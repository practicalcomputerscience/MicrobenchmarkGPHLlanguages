; random_bitstring_and_flexible_password_generator.hy
;
; 2026-06-30
;
;
; run on Ubuntu 24 LTS, in a dedicated virtual Python environment, here named Hy:
;                       (Hy) $ hy -m random_bitstring_and_flexible_password_generator
;
;
; (Hy) $ hy --version  # in virtual Python environment named Hy
; hy 1.3.0
; (Hy) $
;
;
; mostly transpiled from core.clj (Clojure) of random_bitstring_and_flexible_password_generator with Google AI,
; so, keeping "a touch of functional programming".


(import random)
(import re)


;----------------------  user defined functions  ----------------------------
;

(defn input_a_valid_number [n_char]
  "Prompts the user for a valid password length, supporting recursion."
  ;; print in Python flushes automatically if it ends with a newline,
  ;; but we use flush=True to match Clojure's behavior exactly
  (print f"\nPassword of {n_char} printable chars OK? 'y' or another integer number >= 8: " :end "" :flush True)
  (setv answer_str (input))

  (if (= "y" answer_str)
      n_char
      (try
        (setv n_char_ (int answer_str))
        (if (>= n_char_ 8)
            n_char_
            (do (print "enter an integer number >= 8 or 'y'")
                (input_a_valid_number n_char)))
        (except [e Exception]
          (print "enter an integer number >= 8 or 'y'")
          (input_a_valid_number n_char)))))


(defn answer_yes_or_no []
  "Prompts the user for a yes/no configuration choice."
  (print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': " :end "" :flush True)
  (setv answer_str (input))
  (= "y" answer_str))

;
;---------------------  end of user defined functions  ----------------------



(setv END 62500)  ; 62500 for exactly 1M binary digits
; (setv END 10)  ; for testing

(setv m 65521)  ; = 2^16 - 15
(setv a 17364)
(setv c 0)

(setv file_bits_x   "random_bitstring.bin")
(setv file_bits_hex "random_bitstring.byte")

; using Python's random module
(setv x0 (random.randint 1 (- m 1)))

(print "\ngenerating a random bit stream...")

(defn masterloop [n seed]
  "PRNG loop mimicking Clojure's tail-recursive loop/recur structure."
  (setv acc_nbr_v [])
  (setv current_seed seed)
  (setv bits_x_list [])
  (setv bits_hex_list [])

  ; replaced while- with for-loop:
  ;   this doesn't make a faster program, but simplifies this loop:
  (for [_ (range n)]
    (setv bits_x_str   (.format "{:016b}" current_seed))
    (setv bits_hex_str (.format "{:04x}" current_seed))

    (setv next_seed (% (+ (* a current_seed) c) m))

    ; Update loop state variables (recur simulation)
    (.append acc_nbr_v next_seed)
    (setv current_seed next_seed)

    (.append bits_x_list bits_x_str)
    (.append bits_hex_list bits_hex_str))

  ; Return final tuple matching the Clojure vector return format
  [acc_nbr_v (.join "" bits_x_list) (.join "" bits_hex_list)])

(setv results (masterloop END x0))  ; generate END random numbers with seed x0

(setv x (get results 0))
(setv bits_x (get results 1))
(setv bits_hex (get results 2))


; write bit stream to disk:
(try
  (with [f (open file_bits_x "w")]
    (.write f bits_x))
  (print "Bit stream has been written to disk under name: " file_bits_x)
  (except [e Exception]
    (print "could not write to file:" file_bits_x "! --" (str e))))


; write byte stream to disk:
(try
  (with [f (open file_bits_hex "w")]
    (.write f bits_hex))
  (print "Byte stream has been written to disk under name:" file_bits_hex)
  (except [e Exception]
    (print "could not write to file:" file_bits_hex "! --" (str e))))


; make a password of n_char printable chars: user input requested here
(setv n_char_default 12)
(setv n_char (input_a_valid_number n_char_default))
; (print "n_char =" (str n_char))  ; for testing

(setv with_special_chars (answer_yes_or_no))
; (print "with_special_chars =" (str with_special_chars))  ; for testing

;; Compile regex patterns using Python's 're' module
(setv print-re (re.compile r"[!-~]+"))
(setv alnum-re (re.compile r"[A-Za-z0-9]+"))
(setv pattern (if with-special-chars print-re alnum-re))


;; --- Password Generator Logic ---
(defn pw_generator [n_char acc_nbr_v]
  "Generates a password using the pseudo-random seed integers computed earlier."
  (setv j 0)  ; char counter for the password
  (setv pw_chars_ "")

  (while (< (len pw_chars_) n_char)
    (setv bin0a (get acc_nbr_v j))

    (setv bin0b (.format "{:016b}" bin0a))

    (setv bin0_0 (cut bin0b 0 8))
    (setv bin0_1 (cut bin0b 8 16))

    (setv char0 (chr (int bin0_0 2)))
    (setv char1 (chr (int bin0_1 2)))

    ; function valid_char? inside password loop like in Clojure:
    (defn valid_char? [c]
      (bool (.match pattern c)))

    (setv pw_chars_ (+ pw_chars_ (if (valid_char? char0) char0 "")))
    (setv pw_chars_ (+ pw_chars_
      (if (and (valid_char? char1) (< (len pw_chars_) n_char)) char1 "")))

    (setv j (+ j 1)))

  pw_chars_)


(setv pw_chars (pw_generator n_char x))

(print f"\nYour password of {n_char} characters is: {pw_chars}")

; end of random_bitstring_and_flexible_password_generator.hy
