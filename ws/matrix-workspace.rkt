#lang racket

(require (only-in algorithms repeat))

(require simple-matrix)
 
(require lily-gen/ws/workspace)

;; basic concept:  ripple transform as sums to change input of len adjacent integers

(define/contract (ripple-matrix len ripple def)
  (-> exact-positive-integer? (listof exact-integer?) exact-integer? matrix?)
  (transpose
   (for/list ([i (range 0 len)]
              [j (range (sub1 len) -1 -1)])
     (apply append (list (repeat i def) ripple (repeat j def))))))

;; abstract:
;; - def is optional function default id
;; - ripple is list of functions to modify input
;; - result is a list of list of functions to apply to a list

;; > (ripple-matrix 5 '(1 -1) 0)
;;     '((1 0 0 0 0) (-1 1 0 0 0) (0 -1 1 0 0) (0 0 -1 1 0) (0 0 0 -1 1) (0 0 0 0 -1))
