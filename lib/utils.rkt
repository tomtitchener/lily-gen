#lang racket

;; utils: routines with no internal dependencies

(provide
 
 (contract-out
  ;; rotate list forward or backward for positive
  ;; or negative value e.g. '(1 2 3) 1 -> '(2 3 1)
  [rotate-list-by (-> list? exact-integer? list?)]

  ;; answer a thunk that accumulates successive any/c
  ;; converted into a natural until total is >= target
  ;; e.g. to take up to 10 items from a list,
  ;; (take-while (sum<=? identity 10) (range 1 50))
  [sum<=? (-> (-> any/c natural-number/c) natural-number/c (-> any/c boolean?))]

  ;; group by sequential values that match a boolean predicate 
  [group-by-adjacent-sequences (-> (-> any/c any/c boolean?) (listof any/c) (listof (listof any/c)))]

  ;; (gen-buckets '(1 1 4)) -> '(1/6 1/3 1)
  [gen-buckets (-> (non-empty-listof exact-positive-integer?) (non-empty-listof positive?))]
 
  ;; '() -> '()
  ;; '(1) -> '()
  ;; '(1 2 3) -> '((1 2) (2 3))
  ;; '(1 2 3 4) -> '((1 2) (2 3) (3 4))
  [list->pairs (-> (listof any/c) (listof (cons/c any/c any/c)))]

  ;; scanl binary op there will be three params, carry, (i, sum)
  ;; * i is #t and s is #t => add them and continue, doesn't matter about carry
  ;; * i is #f and s is #t => (set! c s) and answer #f (on next call, s will be #f)
  ;; * i is #t and s is #f => (set! c (+ c i)) and answer c (wipes out previous s #f)
  ;; * i is #f and s is #f => answer #f and preserve c as is
  [op-maybe (-> (-> any/c any/c any/c) (-> any/c any/c (or/c #f any/c)))]

  ;; take all but last element in list
  [inits (-> (listof any/c) any/c)]
  ))

(require (only-in algorithms scanl))

;; - - - - - - - - -
;; implementation

(define (inits l)
  (if (null? l)
      '()
      (take l (- (length l) 1))))

;; rotate list forward by cnt, e.g. given '(1 2 3) and 1, give '(2 3 1)
;; [rotate-list-by (-> list? exact-integer? list?)
(define (rotate-list-by lst cnt)
  (let* ([l (length lst)]
         [c (modulo (abs cnt) l)])
    (if (< cnt 0)
        (append (drop lst (- l c)) (take lst (- l c)))
        (append (drop lst c) (take lst c)))))

;;generators.rkt> (ez-weighted-random-list-element '(1 1 1))
;; '(1/3 1/3 1/3)
;; generators.rkt> (ez-weighted-random-list-element '(1 1 4))
;; '(1/6 1/6 2/3)
(define (gen-fractions ws) ;; (w)eight(s) 
  (let ([tot (apply + ws)])
    (map (lambda (w) (/ w tot)) ws)))

;; (-> (non-empty-listof exact-positive-integer?) (non-empty-listof positive?))
(define (gen-buckets ws) ;; (w)eight(s)
  (scanl + (gen-fractions ws)))

(module+ test
  (require rackcheck)
  (check-property
   (property ([nats (gen:list (gen:integer-in 1 100))])
             ;; can't ask gen:list for a minimum number of elements, need at least two
             (let* ([weights (cond [(null? nats) '(1 1)]
                                   [(= 1 (length nats)) (cons 1 nats)]
                                   [else nats])]
                    [ix (list-index (lambda (bucket) (<= (random) bucket)) (gen-buckets weights))])
               (and (check >= ix 0) (check <= ix (sub1 (length weights))))))))

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

(define (list->pairs l)
  (if (or (null? l) (null? (cdr l)))
      '()
      (let ([t (cdr l)])
        (cons (cons (car l) (car t)) (list->pairs t)))))

;; * (s)um starts off as first element in the list
;; * i(ncrement) iterates across the rest of the elements in the list
;; * scanl iterates by apply op with i and s successively
;; except, op-maybe knows of carry so it can propagate #f
(define (op-maybe op)
  (let ([c 0]) ;; (c)arry across #f
    (lambda (i s) ;; from scanl: next (i)ncrement, (s)um or accumulator
      (cond [(and i s)
             (op i s)]
            [(and (not i) s)
             (set! c s)
             #f]
            [(and i (not s))
             (set! c (op c i))
             c]
            [(and (not i) (not s))
             #f]))))
  
(module+ test
  (check-equal?
   (scanl (op-maybe +) '(1 2 3 4))
   '(1 3 6 10))
  (check-equal?
   (scanl (op-maybe +) '(1 2 3 #f))
   '(1 3 6 #f))
  (check-equal?
   (scanl (op-maybe +) '(#f 2 3 4))
   '(#f 2 5 9))
  (check-equal?
   (scanl (op-maybe +) '(1 #f 3 4))
   '(1 #f 4 8))
  (check-equal?
   (scanl (op-maybe +) '(1 2 #f 4))
   '(1 3 #f 7))
  (check-equal?
   (scanl (op-maybe +) '(1 #f #f 4))
   '(1 #f #f 5))
  (check-equal?
   (scanl (op-maybe +) '(1 #f #f #f))
   '(1 #f #f #f))
  (check-equal?
   (scanl (op-maybe +) '(#f #f #f #f))
   '(#f #f #f #f)))
