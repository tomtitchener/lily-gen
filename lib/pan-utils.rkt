#lang racket

;; utils: routines with no internal dependencies

(provide
 (contract-out
  [voice-cnt->pan-distrib (-> exact-nonnegative-integer? (non-empty-listof pan?))]
  [voice-cnt->staggered-pan-distrib (-> exact-nonnegative-integer? (non-empty-listof pan?))]))

(require (only-in lily-gen/lib/score pan? pan-syms))

(define pan-distributions
  '((PanCenter)
    (PanQuarterLeft PanQuarterRight)
    (PanEighthLeft PanCenter PanThreeEighthsRight)
    (PanEighthLeft PanThreeEighthsLeft PanEighthRight PanThreeEighthsRight)
    (PanLeft PanQuarterLeft PanCenter PanQuarterRight PanRight)
    (PanLeft PanEightheft PanThreeEighthsLeft PanEighthRight PanThreeEighthsRight PanRight)
    (PanLeft PanEightheft PanThreeEighthsLeft PanCenter PanEighthRight PanThreeEighthsRight PanRight)
    (PanLeft PanEighthLeft PanQuarterLeft PanThreeEighthsLeft PanEighthRight PanQuarterRight PanThreeEighthsRight PanRight)
    pan-syms))

(define pan-distrib-hash (make-hash (map cons (range 1 10) pan-distributions)))

;; (1 2 3 4 5) -> (1 5 2 4 3)
;; (1 2 3 4 5 6) -> (1 6 2 5 3 4)
(define (bounce-ixs seq-pans)
  (let ([l (length seq-pans)])
    (match l
      [0 '()]
      [1 seq-pans]
      [_ (cons (first seq-pans)
               (cons (last seq-pans)
                     (bounce-ixs (drop-right (drop seq-pans 1) 1))))])))

(module+ test
  (require rackunit)
  (check-equal?
   (bounce-ixs (range 1 6))
   '(1 5 2 4 3))
  (check-equal?
   (bounce-ixs (range 1 7))
   '(1 6 2 5 3 4)))

(define (voice-cnt->pan-distrib cnt-voices)
  (let ([incr (/ 2.0 (sub1 cnt-voices))]
        [ixs  (range 0 cnt-voices)])
    (map (lambda (i) (+ -1 (* i incr))) ixs)))

(define (voice-cnt->staggered-pan-distrib cnt-voices)
  (let ([incr (/ 2.0 (sub1 cnt-voices))]
        [ixs  (bounce-ixs (range 0 cnt-voices))])
    (map (lambda (i) (+ -1 (* i incr))) ixs)))

;; (voice-cnt->pan-distrib-2 16)
;; cnt: 0.13333333333333333 ixs: '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
;; '(-1
;;   -0.8666666666666667
;;   -0.7333333333333334
;;   -0.6
;;   -0.4666666666666667
;;   -0.33333333333333337
;;   -0.19999999999999996
;;   -0.06666666666666665
;;   0.06666666666666665
;;   0.19999999999999996
;;   0.33333333333333326
;;   0.46666666666666656
;;   0.6000000000000001
;;   0.7333333333333334
;;   0.8666666666666667
;;   1.0)


