#lang racket

(require srfi/1)
(require "score-syms.rkt")

(provide (all-defined-out))
 
(define/contract (octave-list-idx oct)
  (-> octave? natural?)
  (list-index ((curry eq?) oct) octave-syms))

(define/contract (octave-list-ref idx)
  (-> natural? octave?)
  (when (or (< idx 0) (>= idx (length octave-syms)))
    (error 'octave-list-ref "idx: ~s out of range for octave-syms ~s" idx octave-syms))
  (list-ref octave-syms idx))

(define/contract (pitch-class-list-idx pitch-class-syms pitch-class)
  (-> (non-empty-listof pitch-class?) pitch-class? natural?)
  (let ([ret (list-index ((curry eq?) pitch-class) pitch-class-syms)])
    (when (not ret)
      (error 'pitch-class-list-idx "pitch-class ~s is not in list ~s" pitch-class pitch-class-syms))
    ret))

(define/contract (pitch-class-list-ref pitch-class-syms idx)
  (-> (non-empty-listof pitch-class?) natural? pitch-class?)
  (when (or (< idx 0) (>= idx (length pitch-class-syms)))
    (error 'pitch-class-list-ref "idx: ~s out of range for pitch-class-syms ~s" idx pitch-class-syms))
  (list-ref pitch-class-syms idx))

(define/contract (rotate-list-by lst cnt)
  (-> list? integer? list?)
  (let* ([l (length lst)]
         [c (modulo (abs cnt) l)])
    (if (< cnt 0)
        (append (drop lst (- l c)) (take lst (- l c)))
        (append (drop lst c) (take lst c)))))

;; create a thunk from a list and an optional starting index with two commands
;; * 'val answers list-ref of remainder of current index and list length, answers any/c
;; * 'inc increments index and answers void?
;; mutable state: index advances per each 'inc infinitely
(define/contract (make-cycle lst [start 0])
  (->* ((listof any/c)) (exact-nonnegative-integer?) (-> symbol? (or/c any/c void?)))
  (let ([i start]
        [l (length lst)])
    (lambda (cmd)
      (case cmd
        ['val (list-ref lst (remainder i l))]
        ['inc (set! i (add1 i))]))))
