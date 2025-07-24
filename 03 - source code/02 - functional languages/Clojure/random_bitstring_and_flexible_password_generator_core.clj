;; core.clj of random_bitstring_and_flexible_password_generator
;;
;; 2025-06-10/11/12/13/14/15/18/21, 2025-07-13
;;
;; build on Ubuntu 24 LTS: $ lein new app random_bitstring_and_flexible_password_generator
;;                         $ cd random_bitstring_and_flexible_password_generator
;;                         $ lein check
;;                         $ lein run
;;                         $ lein uberjar
;;
;; run on Ubuntu 24 LTS:   $ java -jar ./target/uberjar/random_bitstring_and_flexible_password_generator-0.1.0-SNAPSHOT-standalone.jar
;;
;;
;; $ lein version
;; Leiningen 2.10.0 on Java 21.0.7 OpenJDK 64-Bit Server VM
;; $


(ns random-bitstring-and-flexible-password-generator.core
  (:gen-class))


;----------------------  user defined functions  ----------------------------
;
; user defined functions must come before -main:
;
(defn pad16 [str_unpadded]
  (def pad_size (- 16 (count str_unpadded)))  ; pad_size: number of "0"'s needed to pad the input string
  (letfn [(pad-helper [count str_pad]
    (if (zero? count)
      str_pad
      (recur (dec count) (str "0" str_pad))))]
  (pad-helper pad_size str_unpadded)))


(defn input_a_valid_number [n_char]
  (print "\nPassword of" n_char "printable chars OK? 'y' or another integer number >= 8: ")
  (flush)  ; needed, see: https://clojuredocs.org/clojure.core/read-line
  (def answer_str (read-line))

  (if (= "y" answer_str)
    ; (println "n_char after 'y' ==" n_char "==")  ; for testing
    n_char  ; return n_char immediately as a good answer
    (do  ; do can be used to mark a series of expressions that need to be treated as one block
      (try
        (def n_char_ (Integer/parseInt answer_str))  ; Integer/parseInt: calling the Java ecosystem ("Java Interop")
        ; (println "n_char_ ==" n_char_ "==")  ; for testing
        (if (>= n_char_ 8)
          n_char_  ; return a good number
          (do (println "enter an integer number >= 8 or 'y'")
              ; (println "n_char ==" n_char "==")  ; for testing
              (input_a_valid_number n_char)))  ; recursion
        (catch Exception e
          (do (println "enter an integer number >= 8 or 'y'")
              ; (println "n_char ==" n_char "==")  ; for testing
              (input_a_valid_number n_char))))  ; recursion
    )))


(defn answer_yes_or_no []
  (print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  (flush)
  (def answer_str (read-line))

  (if (= "y" answer_str) true false))

;
;---------------------  end of user defined functions  ----------------------


(defn -main  ; defn defines a named function (fn for an anonymous function); minus char at main: starting point of program
  [& args]

  (def END 62500)  ; 62500 for exactly 1M binary digits; defining a variable
  ; (def END 26)  ; for testing
  ; (def M1 (* END 16))
  ; (def K250 (* END 4))

  (def m 65521)  ; = 2^16 - 15; Scala: x(i) = (a*x(i - 1) + c) % m
  (def a 17364)
  (def c 0)

  (def file_bits_x   "random_bitstring.bin")  ; declaring a string variable
  (def file_bits_hex "random_bitstring.byte")

  (def x0 (rand-int m))
  ; (println x0) ; for testing


  (println "\ngenerating a random bit stream...")
  ;
  ; MS Bing AI: Make a pseudo random number generator with an accumulator for n random numbers in Clojure
  ; --> after my modifications and expansions:
  ;----------------------  recursive master loop  -----------------------------
  (defn masterloop [n seed]
    (loop [count n  ; loop is a keyword in Clojure: https://clojuredocs.org/clojure.core/loop
           acc_nbr_v []
           ; acc_nbr_v is a vector; vectors [] are type heterogeneous and evaluate each item in order and have indexed access
           current-seed seed
           bits_x_ (StringBuilder.)  ; using Java StringBuilder Class
           bits_hex_ (StringBuilder.)]
      ; (println "\n" current-seed)  ; for testing

      ; string conversion to binary string:
      (def bits_x_str_ (Integer/toBinaryString ^long current-seed))  ; no padding
      ; (println "  bits_x_str_ =" bits_x_str_)  ; for testing
      ; (def bits_x_str   (format "%016s" ^string bits_x_str_))  ; crashing!!
      ;
      ; padding to 16 binary digits with a user defined function:
      (def bits_x_str (pad16 bits_x_str_))
      ; (println "  bits_x_str =" bits_x_str)  ; for testing

      ; string conversion to hex string:
      ; (def bits_hex_str   (Integer/toString ^long current-seed 16))  ; no padding, no crashing, but very slow
      (def bits_hex_str (format "%04x" ^long current-seed))  ; convert to hex digits with padding => very slow, but OK
      ; (def bits_hex_str (String/format "%04x" (to-array [current-seed])))  ; OK, but also very slow
      ; (def bits_hex_str (String/format "%04x" ^long current-seed))  ; crashing
      ; (println "  bits_hex_str =" bits_hex_str)  ; for testing

      ; Continuation-Passing Style (CPS):
      (if (zero? count)                ; Continuation-Passing Style (CPS): Accept part
        [acc_nbr_v (.toString bits_x_) (.toString bits_hex_)]  ; CPS: Return part, here in form of a vector
        ; Continuation part to provide the next step in the computation:
        (let [next-seed (mod (+ (* a current-seed) c) m)]  ; CPS: Continuation part to provide the next step in the computation
          (recur (dec count) (conj acc_nbr_v next-seed) next-seed
                        ; the conjoin function returns a new collection with an added element at the end of the original vector
                        ; https://clojuredocs.org/clojure.core/conj
                        ;
                        ; string concatenations:
                        (.append bits_x_   bits_x_str)  ; using Java StringBuilder Class method .append
                        (.append bits_hex_ bits_hex_str))))))


  (def results (masterloop END x0))  ; generate END random numbers with seed x0

  ; results is a vector: [acc_nbr_v bits_x_ bits_hex_]:
  (def x (first results))
  (def bits_x (second results))
  (def bits_hex (last results))
  ; (println x) ; for testing
  ; (println bits_x) ; for testing
  ; (println bits_hex) ; for testing

  ; write bit stream to disk:
  (try
    (spit file_bits_x bits_x)
    (println "Bit stream has been written to disk under name: " file_bits_x)
    (catch Exception e
      (println "could not write to file:" file_bits_x " ! -- " (.getMessage e))))

  ; write byte stream to disk:
  (try
    (spit file_bits_hex bits_hex)
    (println "Byte stream has been written to disk under name:" file_bits_hex)
    (catch Exception e
      (println "could not write to file:" file_bits_hex " ! -- " (.getMessage e))))



  ; make a password of n_char printable chars: user input requested here
  (def n_char_default 12)
  (def n_char (input_a_valid_number n_char_default))
  ; (println "main: n_char =" n_char)  ; for testing

  (def with_special_chars (answer_yes_or_no))
  ; (println "with_special_chars =" with_special_chars)  ; for testing

  (if (true? with_special_chars)
    (def char_set (map char (range 33 127)))  ; end of range is exclusive; type of char_set = clojure.lang.LazySeq
    (def char_set (concat (map char (range 48 58))
                          (map char (range 65 91))
                          (map char (range 97 123)))))  ; all alphanumerical ASCII values; type of char_set = clojure.lang.LazySeq
  ; (println "char_set =" char_set)  ; for testing
  ; (println "type of char_set =" (type char_set))  ; for testing



  ;------------------  recursive password creation  ---------------------------
  ;
  ; similar to the OCaml solution
  ; and not like with Roc with two different user defined functions for two different char_set
  ;
  (defn pw_generator [n_char]
    (loop [j 0 pw_chars_ ""]  ; loop is a keyword in Clojure: https://clojuredocs.org/clojure.core/loop
      ; like in the other languages: j: char counter for x
      ; (println "\nj =" j "-- pw_chars_ = " pw_chars_) ;  for testing

      (def bin0a (Integer/toBinaryString ^long (nth x j)))
      (def bin0b (pad16 bin0a))
      ; (println "nth x =" (nth x j)); for testing
      ; (println "bin0b =" bin0b); for testing

      (def bin0_0 (subs bin0b 0 8))
      (def bin0_1 (subs bin0b 8 16))
      ; (println "bin0_0 =" bin0_0 "-- bin0_1 =" bin0_1) ;  for testing

      (def char0 (char (Integer/parseInt bin0_0 2)))
      (def char1 (char (Integer/parseInt bin0_1 2)))
      ; (println "char0 =" char0 "-- char1 =" char1) ;  for testing

      (def char0_add
        (if (boolean (some #(= % char0) char_set))
          char0
          ""))

      (def char1_add
        (if (and (boolean (some #(= % char1) char_set))
                 (< (+ 1 (count pw_chars_)) n_char))
          char1
          ""))

      (def new_pw_chars (str (str pw_chars_ char0_add) char1_add))

      (if (>= (count new_pw_chars) n_char)
        new_pw_chars
        (recur (inc j) new_pw_chars))))  ; recursion


  (def pw_chars (pw_generator n_char))

  (println "\nYour password of" n_char "characters is:" pw_chars)
)

; end of random-bitstring-and-flexible-password-generator
