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
  (when (> cnt-voices (length pan-syms))
    (error 'gen-pans-for-voices "cnt-voices: ~v exceeds length pan-syms: ~v" cnt-voices (length pan-syms)))
  (hash-ref pan-distrib-hash cnt-voices))
