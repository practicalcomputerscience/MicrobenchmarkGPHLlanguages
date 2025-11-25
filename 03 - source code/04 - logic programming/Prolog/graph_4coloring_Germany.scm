;; ============================
;;
;; graph_4coloring_Germany.scm
;;
;; German Map Coloring in (fast) Bigloo Scheme
;;
;; 2025-11-25
;;
;; transpiled from graph_4coloring_Germany2a.pl (for GNU Prolog, version gprolog v.1.4.5)
;; with ChatGPT with prompts:
;;   "Translate this Prolog source code into Scheme source code:"
;;   "A more efficient backtracking Scheme version (instead of Cartesian product)"
;;
;; test in Ubuntu 24 LTS, Bigloo (4.6a): OK!!
;;
;; build in Ubuntu 24 LTS, Bigloo (4.6a): $ bigloo -call/cc -O6 graph_4coloring_Germany.scm -o graph_4coloring_Germany
;; run:                                   $ ./graph_4coloring_Germany
;;                                        $ sudo perf stat -r 20 ./graph_4coloring_Germany
;;                                        $ 0,36744 +- 0,00247 seconds time elapsed  ( +-  0,67% )
;;
;; ============================


(module graph_4coloring_Germany
   (main main))


;; ==============================================
;; Efficient Backtracking Germany Map Coloring
;; ==============================================

(define colors '(red green blue yellow))

;; neighbor predicate (color inequality)
(define (neighbor a b)
  (not (eq? a b)))

;; Check adjacency constraints for partial assignment
;; bindings is an assoc-list: '((SH . red) (MV . green) ...)
(define (consistent? var color bindings)
  (let ((neighbors
         (cond
           ((eq? var 'SH) '(NI HH MV))
           ((eq? var 'MV) '(NI BB SH))
           ((eq? var 'HH) '(NI SH))
           ((eq? var 'HB) '(NI))
           ((eq? var 'NI) '(SH HH MV HB BB ST TH HE NW))
           ((eq? var 'ST) '(NI BB SN TH))
           ((eq? var 'BE) '(BB))
           ((eq? var 'BB) '(MV NI ST BE SN))
           ((eq? var 'SN) '(ST TH BY BB))
           ((eq? var 'NW) '(NI HE RP))
           ((eq? var 'HE) '(NI NW RP BW TH BY))
           ((eq? var 'TH) '(NI ST SN HE BY))
           ((eq? var 'RP) '(NW SL HE BW))
           ((eq? var 'SL) '(RP))
           ((eq? var 'BW) '(RP HE BY))
           ((eq? var 'BY) '(SN TH HE BW))
           (else '()))))

    (let loop ((ns neighbors))
      (cond
        ((null? ns) #t)
        (else
         (let ((other (assoc (car ns) bindings)))
           (if (and other (not (neighbor color (cdr other))))
               #f
               (loop (cdr ns)))))))))


;; === Backtracking search ===
(define (choose vars bindings)
  (if (null? vars)
      (list (reverse bindings))   ;; complete solution
      (let ((var (car vars))
            (rest-vars (cdr vars)))
        (apply append
               (map (lambda (color)
                      (if (consistent? var color bindings)
                          (choose rest-vars
                                  (cons (cons var color) bindings))
                          '()))
                    colors)))))

;; Start variable order (can affect speed)
(define state-order
  '(SH MV HH HB NI ST BE BB SN NW HE TH RP SL BW BY))

(define (all-solutions)
  (choose state-order '()))


;; === Printing functions ===

(define (write-header)
  (newline)
  (display "               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY")
  (newline))

(define (write-solution sol)
  (write (map cdr sol)))

(define (write-solutions sols)
  (let ((n (length sols)))
    (display "number N of different solutions = ")
    (display n)
    (newline)

    (write-header)

    (display "1st solution = ")
    (write-solution (car sols))
    (newline)

    (display "...")
    (newline)

    (display "Last solution = ")
    (write-solution (list-ref sols (- n 1)))
    (newline)))


;; ================
;; main entry point
;; ================
(define (main xx)
  (let ((sols (all-solutions)))
    (write-solutions sols))

  (newline)
)

;; output:
;;   $ ./graph_4coloring_Germany
;;   number N of different solutions = 191808

;;                  SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
;;   1st solution = (red green green red blue red red yellow green red green yellow blue red red blue)
;;   ...
;;   Last solution = (yellow blue blue yellow green yellow yellow red blue yellow blue red green yellow yellow green)
;;
;;   $

;; end of graph_4coloring_Germany.scm
