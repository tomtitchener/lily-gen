#lang racket

;; motifs: structs and contracts

(provide
 ;; - - - - - - - - -
 ;; structs
 (struct-out FixedPitchMotifElements)
 (struct-out FixedOctaveMotifElements)
 (struct-out TupletMotifElements)
 
 ;; - - - - - - - - -
 ;; contracts
 maybe-intervalss-motif-element/c
 tuplet-motif-elements/c
 maybe-intervals-motif/c
 
 ;; - - - - - - - - -
 ;; utilities
 )

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(define maybe-intervalss-motif-element/c
  (make-flat-contract #:name 'maybe-intervalss-motif-element/c #:first-order (list/c maybe-interval-or-intervals/c (listof control/c) (non-empty-listof duration?))))

(define maybe-intervalss-motif-elements/c
  (make-flat-contract #:name 'maybe-intervalss-motif-elements/c #:first-order (non-empty-listof maybe-intervalss-motif-element/c)))

(struct/contract FixedPitchMotifElements ([starting-pitch pitch/c]
                                          [motif-elements maybe-intervalss-motif-elements/c]))

(struct/contract FixedOctaveMotifElements ([starting-octave octave?]
                                          [motif-elements  maybe-intervalss-motif-elements/c]))

(define tuplet-motif-elements/c
  (make-flat-contract #:name 'tuplet-motif-elements/c #:first-order (or/c maybe-intervalss-motif-elements/c FixedPitchMotifElements? FixedOctaveMotifElements?)))

(define/contract (tuplet-motif-element-ctor-guard num denom dur elements type-name)
  (-> natural-number/c
      natural-number/c
      duration?
      tuplet-motif-elements/c
      symbol?
      (values natural-number/c natural-number/c duration? tuplet-motif-elements/c))
  (let ([tot-dur (apply + (map (lambda (element) (apply + (map duration->int (third element))) elements)))])
    (tuplet-ctor-guard-durs num denom dur tot-dur type-name)
    (values num denom dur elements)))

(struct TupletMotifElements (num denom dur elements) #:guard tuplet-motif-element-ctor-guard #:transparent)

;; boils down to (non-empty-listof (non-empty-listof maybe-intervalss-motif-element/c))
;; though the inner list can be just that or container fixed-pitch, fixed-octave, or tuplet with that in a context
;; first three of maybe-intervalss-motif-element/c become (non-empty-listof (or/c Note? Chord? Rest?)) with variants
;; - transposed from last pitch of previous motif or initial starting pitch
;; - anchored by contained starting pitch and carrying previous pitch forward
;; - anchored by contained starting octave and carrying previous pitch forward
;; last becomes Tuplet with contained (non-empty-listof (or/c Note? Chord? Rest?))
;; where transposition behavior depends on tuplet-motif-elements/c, which is one
;; of previous three 
(define maybe-intervals-motif/c
  (make-flat-contract #:name 'maybe-intervals-motif/c #:first-order (or/c FixedPitchMotifElements?
                                                                          FixedOctaveMotifElements?
                                                                          TupletMotifElements?
                                                                          maybe-intervalss-motif-elements/c)))

