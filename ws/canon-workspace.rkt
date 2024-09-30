#lang racket

;; canon-workspace.rkt:  ressurect canon texture
;;

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)

(define pitch&offset-list/c
  (make-flat-contract #:name 'pitch&offset-list/c #:first-order (list/c pitch/c exact-nonnegative-integer?)))
  
(define pitch&offset-lists/c
  (make-flat-contract #:name 'pitch&offset-lists/c #:first-order (non-empty-listof pitch&offset-list/c)))

(define/contract (gen-rotated-canons scale start-pitch&rotate-offsets motif)
  (-> Scale? pitch&offset-lists/c maybe-intervalss-motif/c notes-motifs/c)
  (for/list ([start-pitch&offset start-pitch&rotate-offsets])
    (match start-pitch&offset
      [(list start-pitch offset)
       (rotate-list-by (second (render-maybe-intervalss-motifs scale start-pitch motif)) offset)])))

(module+ test
  (define ex-mot (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E))))
  (require rackunit)
  (check-equal? (gen-rotated-canons C-major (list (list (cons 'C '0va) 0) (list (cons 'C '8vb) 1) (list (cons 'C '15va) 2)) ex-mot)
                (list
                 (list (Note 'D '0va 'E '() #f) (Note 'E '0va 'E '() #f) (Note 'F '0va 'E '() #f))
                 (list (Note 'E '8vb 'E '() #f) (Note 'F '8vb 'E '() #f) (Note 'D '8vb 'E '() #f))
                 (list (Note 'F '15va 'E '() #f) (Note 'D '15va 'E '() #f) (Note 'E '15va 'E '() #f)))))

;; repeat, but with morpher callback

(define morph-maybe-intervalss-motif/c (-> pitch/c notes-motif/c))

;; morpher carries state: scale, maybe-intervalss-motif/c, index
(define/contract (morph-rotated-canons start-pitches morpher)
  (-> (non-empty-listof pitch/c) morph-maybe-intervalss-motif/c notes-motifs/c)
    (for/list ([start-pitch start-pitches])
      (morpher start-pitch)))

(module+ test
  ;; closure captures initial state, possible to mutate all index-by-index, just index for now
  ;; for arbitrary (not-per-index) control, have first arg to morph-maybe-intervalss-motif/c be a list
  (define (make-morph-maybe-intervalls-motif scale maybe-intervalss-motif index)
    (lambda (start-pitch)
      (let* ([notes-motif (second (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif))]
             [morphed-notes-motif (rotate-list-by notes-motif index)])
        (set! index (add1 index))
        morphed-notes-motif)))
  (let ([morpher (make-morph-maybe-intervalls-motif C-major ex-mot 0)])
    (check-equal? (morph-rotated-canons (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'E '0va 'E '() #f) (Note 'F '0va 'E '() #f))
                   (list (Note 'E '8vb 'E '() #f) (Note 'F '8vb 'E '() #f) (Note 'D '8vb 'E '() #f))
                   (list (Note 'F '15va 'E '() #f) (Note 'D '15va 'E '() #f) (Note 'E '15va 'E '() #f))))))

;; generalize start-pitch to (non-empty-listof any/c) as enumerator?
;; let's me parameterize scale in render-maybe-intervalss-motifs
;; also possible to parameterize morpher with arbitrary list, updated on each call,
;; or maybe index and arbitrary list, possibly empty?
;; for now, wait and see what pops out with further practice

;; experiment:  need something like render-osc-voices that answers a (listof Voice?) where all can be PitchedVoice
;; target is a canonic texture with repeated chords with accents (downbeat and upbeat) spread over different starting pitches
;; 
;; what I have so far is a (listof (listof voice-event/c) as output from morph-rotate-canons,
;; which I need to wrap each in a PitchedVoice constructor and then pass to
;; (gen-score-file (score/parameterized <>)) which I can parameterize e.g.
;;   (parameterize ((time-signature/param (TimeSignatureSimple 4 'Q)) (tempo/param (TempoDur 'Q 120))) (gen-score-file (score/parameterized (render-osc-voices))))
;; or, if I want, I can push ex-mot into a parameter itself

;; spread-pans:
;; given a) count of voices, b) list of 9 fixed pan values (pan-syms):
;; - generate a list of pan values, one each per count of voices
;;   with pan values spread evenly from first to last voice up to 9 voices
;;   e.g. given voice count of:
;;   - 1 -> PanCenter 
;;   - 2 -> PanQuarterLeft, PanQuarterRight
;;   - 3 -> PanEighthLeft, PanCenter, PanThreeEightsRight
;;   - 4 -> PanEighthLeft, PanThreeEightsLeft, PanEighthRight, PanThreeEightsRight
;;   - 5 -> PanLeft, PanQuarterLeft, PanCenter, PanQuarterRight, PanRight
;;   - 6 -> PanLeft, PanEightheft, PanThreeEightsLeft, PanEighthRight, PanThreeEightsRight, PanRight
;;   - 7 -> PanLeft, PanEightheft, PanThreeEightsLeft, PanCenter, PanEighthRight, PanThreeEightsRight, PanRight
;;   - 8 -> PanLeft, PanEighthLeft, PanQuarterLeft, PanThreeEightsLeft, PanEighthRight, PanQuarterRight, PanThreeEightsRight, PanRight
;;   - 9 -> verbatim

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

(define (voice-cnt->pan-distrib cnt) (hash-ref pan-distrib-hash cnt))

(define/contract (gen-pans-for-voices cnt-voices)
  (-> exact-nonnegative-integer? (non-empty-listof pan?))
  (when (> cnt-voices (length pan-syms))
    (error 'gen-pans-for-voices "cnt-voices: ~v exceeds length pan-syms: ~v" cnt-voices (length pan-syms)))
  (voice-cnt->pan-distrib cnt-voices))

(define mot/param (make-parameter (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E)))))

(define (make-morph-maybe-intervalls-motif scale maybe-intervalss-motif index)
  (lambda (start-pitch)
    (let* ([notes-motif (second (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif))]
           [morphed-notes-motif (rotate-list-by notes-motif index)])
      (set! index (add1 index))
      morphed-notes-motif)))

(gen-score-file
 (let* ([morpher (make-morph-maybe-intervalls-motif C-major (mot/param) 0)]
        [voice-eventss (morph-rotated-canons (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va)) morpher)]
        [voice-pans (voice-cnt->pan-distrib (length voice-eventss))]
        [voices (for/list ([voice-events voice-eventss]
                           [voice-pan    voice-pans])
                  (SplitStaffVoice 'AcousticGrand voice-pan voice-events))])
   (score/parameterized voices)))
