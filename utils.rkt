#lang racket

;; routines with no internal dependencies

(provide
 (contract-out
  [rotate-list-by (-> list? integer? list?)])
 (contract-out
  [sum<=? (-> (-> any/c natural-number/c) natural-number/c (-> any/c boolean?))]))

;; - - - - - - - - -
;; implementation

;; rotate list forward by cnt, e.g. given '(1 2 3) and 1, give '(2 3 1)
(define(rotate-list-by lst cnt)
  (let* ([l (length lst)]
         [c (modulo (abs cnt) l)])
    (if (< cnt 0)
        (append (drop lst (- l c)) (take lst (- l c)))
        (append (drop lst c) (take lst c)))))

;; given:
;; - a function to convert any/c to a natural number
;; - a maximum value
;; answer
;; - a function that takes any/c, converts it to a natural,
;;   adds it to a running total, and answers if the total is
;;   still <= the maximum
(define(sum<=? any->nat maximum)
  (let ([total 0])
    (lambda (any)
      (set! total (+ total (any->nat any)))
      (<= total maximum))))
