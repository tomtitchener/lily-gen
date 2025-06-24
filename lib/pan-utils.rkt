#lang racket

;; utils: routines with no internal dependencies

(provide
 (contract-out
  [voice-cnt->pan-distrib (-> exact-nonnegative-integer? (non-empty-listof pan?))])
 )

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

(define (voice-cnt->pan-distrib cnt-voices)
  (if (> cnt-voices (length pan-syms))
      (let ([incr (/ 2.0 (sub1 cnt-voices))]
            [ixs  (sequence->list (in-range 0 cnt-voices))])
        (map (lambda (i) (+ -1 (* i incr))) ixs))
      (hash-ref pan-distrib-hash cnt-voices)))

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


