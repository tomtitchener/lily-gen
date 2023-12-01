#lang racket

;; motifs: structs and contracts

(provide
 ;; - - - - - - - - -
 ;; structs
 (struct-out FixedPitchMotif)
 (struct-out FixedOctaveMotif)
 (struct-out TupletMotif)
 
 ;; - - - - - - - - -
 ;; contracts
 maybe-intervalss-motif/c
 maybe-intervalss-motif-element/c
 tuplet-motif-element/c
 maybe-intervals-motif/c
 
 ;; - - - - - - - - -
 ;; utilities
 )

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(define maybe-intervalss-motif-element/c
  (make-flat-contract #:name 'maybe-intervalss-motif-element/c #:first-order (list/c maybe-interval-or-intervals/c (listof control/c) (non-empty-listof duration?))))

(define maybe-intervalss-motif/c
  (make-flat-contract #:name 'maybe-intervalss-motif/c #:first-order (non-empty-listof maybe-intervalss-motif-element/c)))

(struct/contract FixedPitchMotif ([starting-pitch pitch/c] [motif-elements maybe-intervalss-motif/c]))

(struct/contract FixedOctaveMotif ([starting-octave octave?] [motif-elements  maybe-intervalss-motif/c]))

(define tuplet-motif-element/c
  (make-flat-contract #:name 'tuplet-motif-element/c #:first-order (or/c maybe-intervalss-motif/c FixedPitchMotif? FixedOctaveMotif?)))

(define/contract (tuplet-motif-element->duration element)
  (-> tuplet-motif-element/c natural-number/c)
  (match element
    [(FixedPitchMotif _ elements)
     (tuplet-motif-element->duration elements)]
    [(FixedOctaveMotif _ elements)
     (tuplet-motif-element->duration elements)]
    [maybe-intervalss-motif
     (apply + (map duration->int (flatten (map third maybe-intervalss-motif))))]))

(define/contract (tuplet-motif-element-ctor-guard num denom dur element type-name)
  (-> natural-number/c
      natural-number/c
      duration?
      tuplet-motif-element/c
      symbol?
      (values natural-number/c natural-number/c duration? tuplet-motif-element/c))
  (let ([tot-dur (tuplet-motif-element->duration element)])
    (tuplet-ctor-guard-durs num denom dur tot-dur type-name)
    (values num denom dur element)))

(struct TupletMotif (num denom dur element) #:guard tuplet-motif-element-ctor-guard #:transparent)

;; boils down to (non-empty-listof (non-empty-listof maybe-intervalss-motif-element/c))
;; though the inner list can be just that or container fixed-pitch, fixed-octave, or tuplet with that in a context
;; first three of maybe-intervalss-motif-element/c become (non-empty-listof (or/c Note? Chord? Rest?)) with variants
;; - transposed from last pitch of previous motif or initial starting pitch
;; - anchored by contained starting pitch and carrying previous pitch forward
;; - anchored by contained starting octave and carrying previous pitch forward
;; last becomes Tuplet with contained (non-empty-listof (or/c Note? Chord? Rest?))
;; where transposition behavior depends on tuplet-motif-element/c, which is one
;; of previous three 
(define maybe-intervals-motif/c
  (make-flat-contract #:name 'maybe-intervals-motif/c #:first-order (or/c FixedPitchMotif?
                                                                          FixedOctaveMotif?
                                                                          TupletMotif?
                                                                          maybe-intervalss-motif/c)))

