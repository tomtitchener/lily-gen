#lang racket

(require srfi/1)

(provide (all-defined-out))

(define/contract (rotate-list-by lst cnt)
  (-> list? integer? list?)
  (let* ([l (length lst)]
         [c (modulo (abs cnt) l)])
    (if (< cnt 0)
        (append (drop lst (- l c)) (take lst (- l c)))
        (append (drop lst c) (take lst c)))))

(define/contract (sum<=? any->nat maximum)
  (-> (-> any/c natural?) natural? (-> any/c boolean?))
  (let ([total 0])
    (lambda (any)
      (set! total (+ total (any->nat any)))
      (<= total maximum))))
#|

;;;;;;;;;;;;;;;
;; GRAVEYARD ;;
;;;;;;;;;;;;;;;

(define/contract (repeat-list n lst)
  (-> exact-nonnegative-integer? list? list?)
  (apply append (repeat n lst)))

|#
