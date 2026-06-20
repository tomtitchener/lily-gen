#lang racket

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(require (only-in seq zip unzip))

;; motifs-new:  simplify motifs.rkt
;; * eliminate structs with start pitch
;;   - FixedPitchMaybeIntervalsMotif
;;   - FixedOctaveMaybeIntervalsMotif
;;   - TupletMaybeIntervalsMotif
;; * use only transpose/absolute call for scale and start pitch, 
;;   much more intuitive than transpose/successive where each
;;   interval refers to immediately preceeding pitch in sequence,
;;   converts old render* to single motif->notes-motif
;; * eliminates possible mismatch between pitch and key in when 
;;   I pick up new start pitches in render-motifs

;; raw
(define motif-note-chord-or-rest/c
  (make-flat-contract #:name 'motif-note-chord-or-rest/c #:first-order
                      (list/c maybe-interval-or-intervals/c (listof control/c) (non-empty-listof duration?))))

(define motif-note-chord-or-rests/c
  (make-flat-contract #:name 'motif-note-chord-or-rests/c #:first-order
                      (listof motif-note-chord-or-rest/c)))

;; shouldn't this have a guarded constructor?
(define motif-tuplet/c
  (make-flat-contract #:name 'motif-tuplet/c #:first-order
                      (list/c natural-number/c natural-number/c duration? motif-note-chord-or-rests/c)))

(define motif-element/c
  (make-flat-contract #:name 'motif-element/c #:first-order
                      (or/c motif-note-chord-or-rest/c motif-tuplet/c)))

(define motif/c
  (make-flat-contract #:name 'motif/c #:first-order (listof motif-element/c)))

;; cooked
(define notes-motif-element/c
  (make-flat-contract #:name 'notes-motif-element/c #:first-order (or/c Note? Rest? Chord? Tuplet?)))

(define notes-motif/c
  (make-flat-contract #:name 'notes-motif/c #:first-order (listof notes-motif-element/c)))
 
(define notes-motifs/c
  (make-flat-contract #:name 'notes-motifs/c #:first-order (listof notes-motif/c)))

(provide
 ;; - - - - - - - - -
 ;; contracts
 motif/c
 motif-element/c
 motif-note-chord-or-rest/c
 motif-tuplet/c
 notes-motifs/c
 notes-motif/c
 notes-motif-element/c

 ;; - - - - - - - - -
 ;; utilities
 (contract-out
  [motif->notes-motif
   (-> Scale? pitch/c motif/c notes-motif/c)])
 )

;; internally, transposition is 0 based, e.g. 0 => unison, +/-1 one step up, one step down, etc.
;; but historically the name for a single step up and down is a second and the name for identity
;; as in 1 with respect to multiplication is unison, so it's easier when writing down an interval
;; series to use 1, 2, 3, 4, etc. for unison, second, third, fourth etc.
;; this routine squeezes the traditional terms down so e.g. -1/1 becomes 0, 2,3.. become 1,2..,
;; and -2,-3.. become -1,-2..
;; note maybe-interval-or-intervalss/c includes #f for rests and a sub-list for chords, so this
;; routine has to handle those, too
;; the only illegal input value is 0
;; note the definition of maybe-interval-or-intervalss/c does not allow nested lists
(define/contract (squeeze-mint-or-intss mint-or-ints)
  (-> maybe-interval-or-intervalss/c maybe-interval-or-intervalss/c)
  (match mint-or-ints
    [#f #f] ;; rest
    [(? list? mints) ;; chord
     (map squeeze-mint-or-intss mints)]
    [i ;; single interval
     (match i
       [0 (error 'squeeze-mint-or-intss "illegal 0 val not in range [..-2,-1,1,2..]")]
       [(? positive? i) (sub1 i)]
       [i (add1 i)])]))

;; helper:  interpret maybe-pitch-or-pitches/c into Rest, Note, or Chord
;; where there are multiple durations, repeat, with ties for Note or Chord
;; and controls only for first Note or Chord
(define/contract (maybe-pitch-or-pitches-motif->motif maybe-pitch-or-pitches-motif)
  (-> (list/c maybe-pitch-or-pitches/c (listof control/c) (listof duration?)) (non-empty-listof (or/c Note? Rest? Chord?)))
  (match maybe-pitch-or-pitches-motif
    [(list maybe-pitch-or-pitches controls durations)
     (match maybe-pitch-or-pitches
       ;; Rest
       [#f
        (map Rest durations)]
       ;; Note
       [(cons (? pitch-class? pitch-class) (? octave? octave))
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Note pitch-class octave duration (if first controls '()) (not last))  acc)))
                 '() durations))]
       ;; Chord
       [(list (? pitch/c) ...) ;; matches, but doesn't bind
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Chord maybe-pitch-or-pitches duration (if first controls '()) (not last))  acc)))
                 '() durations))])]))

;; (-> Scale? pitch/c motif/c notes-motif/c)])
(define (motif->notes-motif scale starting-pitch motif)
  (let ([max-pitch-range-pair (scale->pitch-range-pair scale)])
    (define/contract
      (proc-motif-note-chord-or-rests motif-note-chord-or-rests)
      (-> motif-note-chord-or-rests/c notes-motif/c)
      (match (sequence->list (unzip motif-note-chord-or-rests))
        [(list maybe-intervalss controlss durationss)
         (let* ([squeezed-maybe-intervalss (squeeze-mint-or-intss maybe-intervalss)]
                [maybe-pitch-or-pitchess (transpose/absolute scale max-pitch-range-pair starting-pitch squeezed-maybe-intervalss)]
                [pitch-or-pitches-motif  (sequence->list (zip maybe-pitch-or-pitchess controlss durationss))])
           (flatten (map maybe-pitch-or-pitches-motif->motif pitch-or-pitches-motif)))]))
    (define/contract
      (proc-motif-tuplet motif-tuplet)
      (-> motif-tuplet/c notes-motif/c)
      (match motif-tuplet
        [(list num denom dur maybe-intervalss-note-chord-or-rests)
         (let ([notes (proc-motif-note-chord-or-rests maybe-intervalss-note-chord-or-rests)])
           (list (Tuplet num denom dur notes)))]))
    (define/contract
      (proc-motif-element motif-element notes-motif)
      (-> motif-element/c notes-motif/c notes-motif/c)
      (match motif-element
        [(? motif-note-chord-or-rest/c motif-element)
         (let ([note (proc-motif-note-chord-or-rests (list motif-element))])
           (append note notes-motif))]
        [(? motif-tuplet/c motif-element)
         (let ([tuplet (proc-motif-tuplet motif-element)])
           (append tuplet notes-motif))]))
    (flatten (foldr proc-motif-element '() motif))))

(module+ test
  (require rackunit)
  (check-equal? (squeeze-mint-or-intss '(-1 -2 #f (-3 -2) 3 2 1)) '(0 -1 #f (-2 -1) 2 1 0))
  (define intervals '(4 5 2 -2 1 -6 -4 9 -7 6 -6 -3 1 3 -3))
  (check-equal?
   (motif->notes-motif C-major '(C . 0va) (map (lambda (i) (list i '() '(E))) intervals))
   (list
    (Note 'F '0va 'E '() #f)  ;  4
    (Note 'G '0va 'E '() #f)  ;  5
    (Note 'D '0va 'E '() #f)  ;  2
    (Note 'B '8vb 'E '() #f)  ; -2
    (Note 'C '0va 'E '() #f)  ;  1
    (Note 'E '8vb 'E '() #f)  ; -6
    (Note 'G '8vb 'E '() #f)  ; -4 
    (Note 'D '8va 'E '() #f)  ;  9
    (Note 'D '8vb 'E '() #f)  ; -7
    (Note 'A '0va 'E '() #f)  ;  6
    (Note 'E '8vb 'E '() #f)  ; -6
    (Note 'A '8vb 'E '() #f)  ; -3 
    (Note 'C '0va 'E '() #f)  ;  1
    (Note 'E '0va 'E '() #f)  ;  3
    (Note 'A '8vb 'E '() #f)  ; -3
    ))
  (check-equal?
   (motif->notes-motif C-major '(C . 0va)
                                        (list (list #f '() '(E))
                                              (list 3 2 'H (list (list 2 '() '(Q)) (list 3 '() '(Q)) (list 4 '() '(Q)) ))
                                              (list '(1 2) '() '(E))))
   (list
    (Rest 'E)
    (Tuplet 3 2 'H (list (Note 'D '0va 'Q '() #f) (Note 'E '0va 'Q '() #f) (Note 'F '0va 'Q '() #f)))
    (Chord '((C . 0va) (D . 0va)) 'E '() #f)))
  )
