; random_streams_for_perf_stats.hy
;
; 2026-06-30
;
;
; run on Ubuntu 24 LTS: $ hy random_streams_for_perf_stats.hy
;                       $ time hy random_streams_for_perf_stats.hy => real	0m0.200s
;
;
; mostly transpiled from core.clj (Clojure) of random_streams_for_perf_stats with Google AI,
; so, keeping "a touch of functional programming".
;
; interestingly, using Python's StringIO(), that is here in Hy: (import io) and (setv bits_x (io.StringIO)),
; is hardly improving execution speed!


(import random)


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

; end of random_streams_for_perf_stats.hy
