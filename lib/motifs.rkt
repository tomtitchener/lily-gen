#lang racket

;; motifs: structs and contracts

(provide
 ;; - - - - - - - - -
 ;; structs
 (struct-out FixedPitchMaybeIntervalsMotif)
 (struct-out FixedOctaveMaybeIntervalsMotif)
 (struct-out TupletMaybeIntervalsMotif)
 
 ;; - - - - - - - - -
 ;; contracts
 maybe-intervalss-motif/c
 maybe-intervalss-motif-element/c
 tuplet-motif-element/c
 maybe-intervalss-motifs/c
 
 ;; - - - - - - - - -
 ;; utilities
 )

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(define maybe-intervalss-motif-element/c
  (make-flat-contract #:name 'maybe-intervalss-motif-element/c #:first-order (list/c maybe-interval-or-intervals/c (listof control/c) (non-empty-listof duration?))))

(define maybe-intervalss-motif/c
  (make-flat-contract #:name 'maybe-intervalss-motif/c #:first-order (non-empty-listof maybe-intervalss-motif-element/c)))

(struct/contract FixedPitchMaybeIntervalsMotif ([starting-pitch pitch/c] [motif-elements maybe-intervalss-motif/c]))

(struct/contract FixedOctaveMaybeIntervalsMotif ([starting-octave octave?] [motif-elements  maybe-intervalss-motif/c]))

(define tuplet-motif-element/c
  (make-flat-contract #:name 'tuplet-motif-element/c #:first-order (or/c maybe-intervalss-motif/c FixedPitchMaybeIntervalsMotif? FixedOctaveMaybeIntervalsMotif?)))

(define/contract (tuplet-motif-element->duration element)
  (-> tuplet-motif-element/c natural-number/c)
  (match element
    [(FixedPitchMaybeIntervalsMotif _ elements)
     (tuplet-motif-element->duration elements)]
    [(FixedOctaveMaybeIntervalsMotif _ elements)
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

(struct TupletMaybeIntervalsMotif (num denom dur element) #:guard tuplet-motif-element-ctor-guard #:transparent)

;; TBD: tricky names that vary by plural.
;; This is a sum, which should be singular, with abstract label?
;; Rename maybe-intervalss-motif/c with transposing-maybe-intervalss-motif/c?
(define maybe-intervalss-motifs/c
  (make-flat-contract #:name 'maybe-intervalss-motifs/c #:first-order (or/c FixedPitchMaybeIntervalsMotif?
                                                                            FixedOctaveMaybeIntervalsMotif?
                                                                            TupletMaybeIntervalsMotif?
                                                                            maybe-intervalss-motif/c)))

