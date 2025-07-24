;; core.clj of random_streams_for_perf_stats
;;
;; 2025-06-10/11/12/15
;;
;; build on Ubuntu 24 LTS: $ lein new random_streams_for_perf_stats
;;                         $ cd random_streams_for_perf_stats
;;                         $ lein check
;;                         $ lein run
;;                         $ lein uberjar
;;
;; run on Ubuntu 24 LTS:   $ java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
;;
;; exe time measurement:
;; $ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
;;
;; $ time java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
;;
;;
;; Experiment: (if (> (.length bits_x_str_) 15)...) is slower
;;   .length Java call to overcome this exception:
;;     class java.lang.Long cannot be cast to class clojure.lang.IFn
;;     (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IFn is in unnamed module of loader 'app')
;;
;;
;; $ lein version
;; Leiningen 2.10.0 on Java 21.0.7 OpenJDK 64-Bit Server VM
;; $


(ns random_streams_for_perf_stats.core
  (:gen-class)
  )

(set! *warn-on-reflection* false)  ; not useful for me


; user defined functions must come before -main:
;
(defn pad16 [str_unpadded]
  (def pad_size (- 16 (count str_unpadded)))  ; pad_size: number of "0"'s needed to pad the input string
  (letfn [(pad-helper [count str_pad]
    (if (zero? count)
      str_pad
      (recur (dec count) (str "0" str_pad))))]
  (pad-helper pad_size str_unpadded)))



(defn -main  ; defn defines a named function (fn for an anonymous function); minus char at main: starting point of program
  [& args]

  (def END 62500)  ; 62500 for exactly 1M binary digits; defining a variable
  ; (def END 15)  ; for testing
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
  (defn masterloop [n seed]  ; the pseudo random number generator (PRNG)
    (loop [count n
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


      (if (zero? count)                ; Continuation-Passing Style (CPS): Accept part
        [acc_nbr_v (.toString bits_x_) (.toString bits_hex_)]  ; CPS: Return part, here in form of a vector
        (let [next-seed (mod (+ (* a current-seed) c) m)]  ; CPS: Continuation part to provide the next step in the computation
          (recur (dec count) (conj acc_nbr_v next-seed) next-seed
                        ; the conjoin function returns a new collection with an added element at the end of the original vector
                        ; https://clojuredocs.org/clojure.core/conj
                        ;
                        ; string concatenations:
                        (.append bits_x_   bits_x_str)  ; using Java StringBuilder Class method .append
                        (.append bits_hex_ bits_hex_str)
          )))
    )
  )

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
)


; end of random_streams_for_perf_stats
