#lang racket

;; utils: routines with no internal dependencies

(provide
 (contract-out
  ;; rotate list forward or backward for positive
  ;; or negative value e.g. '(1 2 3) 1 -> '(2 3 1)
  [rotate-list-by (-> list? signed-integer/c list?)]

  ;; answer a thunk that accumulates successive any/c
  ;; converted into a natural until total is >= target
  ;; e.g. to take up to 10 items from a list,
  ;; (take-while (sum<=? identity 10) (range 1 50))
  [sum<=? (-> (-> any/c natural-number/c) natural-number/c (-> any/c boolean?))]

  ;; group by sequential values that match a boolean predicate 
  [group-by-adjacent-sequences (-> (-> any/c any/c boolean?) (listof any/c) (listof (listof any/c)))]))

(require binary-class/contract)

;; - - - - - - - - -
;; implementation

;; rotate list forward by cnt, e.g. given '(1 2 3) and 1, give '(2 3 1)
;; [rotate-list-by (-> list? signed-integer/c list?)
(define (rotate-list-by lst cnt)
  (let* ([l (length lst)]
         [c (modulo (abs cnt) l)])
    (if (< cnt 0)
        (append (drop lst (- l c)) (take lst (- l c)))
        (append (drop lst c) (take lst c)))))

(module+ test
  (require rackunit)
  (check-equal?
   (rotate-list-by '(1 2 3) 0)
   '(1 2 3))
  (check-equal?
   (rotate-list-by '(1 2 3) 1)
   '(2 3 1))
  (check-equal?
   (rotate-list-by '(1 2 3) 2)
   '(3 1 2))
  (check-equal?
   (rotate-list-by '(1 2 3) 4)
   (rotate-list-by '(1 2 3) 1))
  (check-equal?
   (rotate-list-by '(1 2 3) -1)
   '(3 1 2))
  (check-equal?
   (rotate-list-by '(1 2 3) -2)
   '(2 3 1))
  (check-equal?
   (rotate-list-by '(1 2 3) -4)
   (rotate-list-by '(1 2 3) -1)))

;; given:
;; - a function to convert any/c to a natural number
;; - a maximum value
;; answer
;; - a function that takes any/c, converts it to a natural,
;;   adds it to a running total, and answers if the total is
;;   still <= the maximum
;; (-> (-> any/c natural-number/c) natural-number/c (-> any/c boolean?))
(define (sum<=? any->nat maximum)
  (let ([total 0])
    (lambda (any)
      (set! total (+ total (any->nat any)))
      (<= total maximum))))

(module+ test
  (require srfi/1)
  (check-equal?
   (take-while (sum<=? identity 10) (range 1 50))
   '(1 2 3 4)))

;; group sequentially by binary predicate similar to Haskell groupBy
;; but distinct from racket group-by, binary predicate lets me group
;; adjacent Note or Rest to recognize beginning and end of lists of
;; tied notes
;; (-> (-> any/c any/c boolean?) (listof any/c) (listof (listof any/c)))
(define (group-by-adjacent-sequences binp xs)
  (define f (lambda (x acc)
              (let ([cur (car acc)]
                    [ret (cadr acc)])
                (if (null? cur)
                    (list (list x) ret)
                    (if (binp (car cur) x)
                        (list (cons x cur) ret)
                        (list (list x) (if (= 1 (length cur))
                                           (cons cur ret)
                                           (cons (reverse cur) ret))))))))
  (let ([acc (foldl f '(()()) xs)])
    (let ([cur (car acc)]
          [ret (cadr acc)])
      (reverse (cons (reverse cur) ret)))))

(module+ test
  (define (p x y) (and (even? x) (even? y)))
  (check-equal?
   (group-by-adjacent-sequences p '(1 1 2 4 3 7 8 9 4 4 6 10 11 20 30))
   '((1) (1) (2 4) (3) (7) (8) (9) (4 4 6 10) (11) (20 30))))


