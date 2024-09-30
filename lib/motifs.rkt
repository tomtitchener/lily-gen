#lang racket

;; motifs: structs and contracts
;; data types and contracts for motifs vary by
;; - anchored register by pitch:  FixedPitchMaybeIntervalsMotif
;; - anchored register by octave:  FixedOctaveMaybeIntervalsMotif
;; - tuplet:  TupletMaybeIntervalsMotif
;; - transposed register by context (e.g. last previous pitch) maybe-intervalss-motif/c
;; first two are wrappers around fourth, third is wrapper around first two and fourth
;; fourth is raw data
;;
;; motifs: utilities for motifs:
;; - render-maybe-intervalss-motifs:
;;     given scale, starting pitch, and motif, render to listof (or/c Note? Rest? Chord? Tuplet?),
;;     also answers last pitch for segue to render again as new starting pitch
;; - weighted-maybe-intervalss-motifs/generator:
;;     given lists of weights and motifss with Scale and initial starting-pitch, randomly select
;;     one of motifs to render, then repeat using last pitch from previous iteration
;; - travelling-motifs/generator:
;;     given scale, starting-pitch, range as min/max pitches, and initial direction (up,down), 
;;     and a list of motifs, assign weights to motifs based on their ranges vs. the direction,
;;     then pick randomly using those weights, with goal of keeping output range within limits
;;     doesn't work reliably due to randomness, can exceed ranges anyway (see ws/motifs-workspace.rkt)
;; - morph-motifs:
;;     given starting scale, pitch, maybe-intervals-motif, and general morpher routine to map 
;;     old scale, pitch, and motif and previous ending pitch to new scale, starting pitch, and 
;;     motif or else #f to signal termination, emit list of list of (or/c Note? Rest? Chord? Tuplet?), 
;;     one per iteration

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
 weight&maybe-intervalss-motifss/c
 notes-motif-element/c
 notes-motif/c
 notes-motifs/c
 morph/c
 
 ;; - - - - - - - - -
 ;; utilities
 ;; generates a list of (non-empty-listof (or/ Note? Rest?)) randomly by weights until done?
 ;; done? function input is most recent (list of (Note or Rest))
 (contract-out
  [render-maybe-intervalss-motifs 
   (-> Scale? pitch/c maybe-intervalss-motifs/c (list/c maybe-pitch/c notes-motif/c))]
   
  [weighted-maybe-intervalss-motifs/generator
   (-> Scale? pitch/c weight&maybe-intervalss-motifss/c generator?)]

  [travelling-motifs/generator
   (-> Scale? pitch/c pitch-range-pair/c direction/c (non-empty-listof maybe-intervalss-motif/c) generator?)]

  [morph-motifs
   (-> Scale? pitch/c maybe-intervalss-motif/c morph/c (non-empty-listof notes-motif/c))]

  ;; increment with wrap of pan for current motif, init to left
  [increment-pan-morpher
   morph/c]
 ))

(require racket/generator)

(require (only-in seq zip unzip))

(require lily-gen/lib/utils)

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(require lily-gen/lib/generators)

;; individual pitch or chord represented as one or more intervals, with zero or more controls and one or more durations (tied together)
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
    [(? maybe-intervalss-motif/c maybe-intervalss-motif)
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

;; sum of non-empty-listof duration? from element has to be even multiple of duration of tuplet 
(struct TupletMaybeIntervalsMotif (num denom dur element) #:guard tuplet-motif-element-ctor-guard #:transparent)

;; TBD: tricky names that vary by plural.
;; This is a sum, which should be singular, with abstract label?
;; Rename maybe-intervalss-motif/c with transposing-maybe-intervalss-motif/c?
(define maybe-intervalss-motifs/c
  (make-flat-contract #:name 'maybe-intervalss-motifs/c #:first-order (or/c FixedPitchMaybeIntervalsMotif?
                                                                            FixedOctaveMaybeIntervalsMotif?
                                                                            TupletMaybeIntervalsMotif?
                                                                            maybe-intervalss-motif/c)))
(define weight&maybe-intervalss-motifss/c
  (make-flat-contract #:name 'weight&maybe-intervalss-motifss/c #:first-order
                      (non-empty-listof (list/c relative-weight/c maybe-intervalss-motifs/c))))

(define weight&maybe-intervalss-motifs/c
  (make-flat-contract #:name 'weight&maybe-intervalss-motifs/c #:first-order
                      (non-empty-listof (list/c relative-weight/c maybe-intervalss-motif/c))))

(define notes-motif-element/c
  (make-flat-contract #:name 'notes-motif-element/c #:first-order (or/c Note? Rest? Chord? Tuplet?)))

(define notes-motif/c
  (make-flat-contract #:name 'notes-motif/c #:first-order (non-empty-listof notes-motif-element/c)))

(define notes-motifs/c
  (make-flat-contract #:name 'notes-motifs/c #:first-order (non-empty-listof notes-motif/c)))

;; a name for a function contract without interface/class overhead
(define morph/c (-> Scale? pitch/c maybe-intervalss-motif/c maybe-pitch/c notes-motifs/c (or/c #f (list/c Scale? pitch/c maybe-intervalss-motifs/c))))

(define/contract (maybe-pitch-or-pitches-motif->motif maybe-pitch-or-pitches-motif)
  (-> (list/c maybe-pitch-or-pitches/c (listof control/c) (listof duration?)) (non-empty-listof (or/c Note? Rest? Chord?)))
  (match maybe-pitch-or-pitches-motif
    [(list maybe-pitch-or-pitches controls durations)
     (match maybe-pitch-or-pitches
       [#f
        (map Rest durations)]
       [(cons (? pitch-class? pitch-class) (? octave? octave))
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Note pitch-class octave duration (if first controls '()) (not last))  acc)))
                 '() durations))]
       [(list (? pitch/c) ...) ;; matches, but doesn't bind
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Chord maybe-pitch-or-pitches duration (if first controls '()) (not last))  acc)))
                 '() durations))])]))

;; given a previous starting-pitch and a list of maybe-intervalss-motif/c,
;; transpose maybe-intervalss into list of Rest? Note? or Chord? and answer
;; the list with the final maybe-pitch from the list for the next motif 
(define/contract (render-maybe-intervalss-motif scale starting-pitch maybe-intervalss-motif-elements)
  (-> Scale? pitch/c maybe-intervalss-motif/c (list/c maybe-pitch/c (non-empty-listof (or/c Note? Chord? Rest?))))
  (define (accum-motifs maybe-pitch-or-pitches-motif motifs)
    (cons (maybe-pitch-or-pitches-motif->motif maybe-pitch-or-pitches-motif) motifs))
  (define (maybe-pitch-or-pitches->maybe-pitch maybe-pitch-or-pitches)
    (match maybe-pitch-or-pitches
      [#f #f]
      [(? pitch/c pitch) pitch]
      [(list (? pitch/c) ...) (car maybe-pitch-or-pitches)]))
  (let ([begin-pitch starting-pitch]
        [max-pitch-range-pair (scale->pitch-range-pair scale)])
    (match (sequence->list (unzip maybe-intervalss-motif-elements))
      [(list maybe-intervalss controlss durationss)
       (let* ([maybe-pitch-or-pitchess (transpose/successive scale max-pitch-range-pair begin-pitch maybe-intervalss)]
              [maybe-pitch             (maybe-pitch-or-pitches->maybe-pitch (last maybe-pitch-or-pitchess))]
              [motifss                 (foldr accum-motifs '() (sequence->list (zip maybe-pitch-or-pitchess controlss durationss)))])
         (list maybe-pitch (flatten motifss)))])))

;; (-> Scale? pitch/c maybe-intervalss-motifs/c (list/c maybe-pitch/c notes-motif/c))
(define (render-maybe-intervalss-motifs scale begin-pitch motif)
  (match motif
    [(FixedPitchMaybeIntervalsMotif starting-pitch motif)
     (match (render-maybe-intervalss-motif scale starting-pitch motif)
       [(list _ motifs)
        (list begin-pitch motifs)])]
    [(FixedOctaveMaybeIntervalsMotif starting-octave motif)
     (match (render-maybe-intervalss-motif scale (cons (car begin-pitch) starting-octave) motif)
       [(list _ motifs)
        (list begin-pitch motifs)])]
    [(TupletMaybeIntervalsMotif num denom dur motif)
     (match (render-maybe-intervalss-motifs scale begin-pitch motif)
       [(list maybe-pitch motifs)
        (list (or maybe-pitch begin-pitch) (list (Tuplet num denom dur motifs)))])]
    [(? maybe-intervalss-motif/c maybe-intervalss-motif)
     (match (render-maybe-intervalss-motif scale begin-pitch maybe-intervalss-motif)
       [(list maybe-pitch motifs)
        (list (or maybe-pitch begin-pitch) motifs)])]))

;; (-> Scale? pitch/c weight&maybe-intervalss-motifss/c generator?)
;; each (maybe-intervals-motif/generator) from weight&maybe-intervalss-motifs gives
;; a maybe-intervals-motif/c as either a (non-empty-listof maybe-intervalss-motif-element/c),
;; a FixedPitchMaybeIntervalsMotif?, a FixedOctaveMaybeIntervalsMotif? or a TupletMaybeIntervalsMotif?
;; generator creates (non-empty-listof (or/c Note? Chord? Rest? Tuplet?))
;; (-> Scale? pitch/c weight&maybe-intervalss-motifss/c generator?)
(define (weighted-maybe-intervalss-motifs/generator scale starting-pitch weight&maybe-intervalss-motifss)
  (let ([maybe-intervals-motifss/generator (weighted-list-element/generator weight&maybe-intervalss-motifss)])
    (generator ()
       (let loop ([begin-pitch             starting-pitch]
                  [maybe-intervalss-motifs (maybe-intervals-motifss/generator)])
         (match (render-maybe-intervalss-motifs scale begin-pitch maybe-intervalss-motifs)
           [(list next-begin-pitch motifs)
            (yield motifs)
            (loop next-begin-pitch (maybe-intervals-motifss/generator))])))))
    
;; FSM for parsing a motif into an maybe-interval motif to check results from generate-maybe-intervals-weighted-motifs.
;; NB: uses only the simplest of motif types (maybe-intervalss-motif-elements/c with single intervals),
;; which makes the reverse construction from the list of Note or Rest possible.  It's probably not worth
;; the effort to expand this test case to cover the other motif types FixedPitchMaybeIntervalsMotif, etc.
;; Would would work as a grammar?  Took a lot of work to implement from scratch and more to debug.
;; TBD: use logging instead of printf so messages can be enabled from REPL?
;; Note there's a racket logger running a logger integrated with emacs Racket mode.
(module+ test
  (require rackunit)
  (require (only-in lily-gen/lib/utils sum<=?))
  (define/contract (motif->maybe-interval-motif scale starting-pitch motif)
    (-> Scale? pitch/c (non-empty-listof (or/c Note? Rest?)) (list/c pitch/c maybe-intervalss-motif/c))
    (struct ST (current-state maybe-interval-motif previous-pitch current-pitch controls durations) #:transparent)
    (define (pitches->interval first-pitch this-pitch)
      (- (pitch->index scale this-pitch) (pitch->index scale first-pitch)))
    (let loop ([sm (ST 'E '() starting-pitch #f '() '())]
               [mot motif])
      (match (ST-current-state sm)
        ['E
         (match mot
           ['()
            #;(printf "ST 'E END, state ~v\n ret: ~v\n" sm (list (ST-previous-pitch sm) (ST-maybe-interval-motif sm)))
            (list (ST-previous-pitch sm) (ST-maybe-interval-motif sm))
            ]
           [(cons m ms)
            (match m
              [(Note pc oct dur ctrls #t) ;; start of list of notes
               (let* ([this-pitch (cons pc oct)])
                 (let ([st (ST 'AN (ST-maybe-interval-motif sm) (ST-previous-pitch sm) this-pitch ctrls (list dur))])
                   #;(printf "Note with tie:\n old state ~v\n new state ~v\n" sm st)
                   (loop st ms)))]
              [(Note pc oct dur ctrls #f) ;; singleton note 
               (let* ([this-pitch (cons pc oct)]
                      [interval   (pitches->interval (ST-previous-pitch sm) this-pitch)]
                      [note-elem  (list interval ctrls (list dur))]
                      [abs-motif  (append (ST-maybe-interval-motif sm) (list note-elem))])
                 (let ([st (ST 'E abs-motif this-pitch #f '() '())])
                   #;(printf "Note no tie:\n old state ~v\n new state ~v\n" sm st)
                   (loop st ms)))]
              [(Rest dur)
               (let [(st (ST 'AR (ST-maybe-interval-motif sm) (ST-previous-pitch sm) #f '() (list dur)))]
                 #;(printf "Rest:\n old state ~v\n new state ~v\n" sm st)
                 (loop st ms))])])]
        ['AN
         (match mot
           ['()
            (error 'motif->maybe-interval-motif "match EOL in state 'AN, state: ~v" sm)]
           [(cons m ms)
            (match m
              [(Note pc oct dur _ #t)
               (when (not (equal? (cons pc oct) (ST-current-pitch sm)))
                 (error 'motif->maybe-interval-motif "no match current-pitch ~v with this pitch ~v state ~v" (cons pc oct) (ST-current-pitch sm) sm))
               (let ([st (struct-copy ST sm (durations (append (ST-durations sm) (list dur))))])
                 #;(printf "Note with tie:\n old state ~v\n new state ~v\n" sm st)
                 (loop st ms))]
              [(Note pc oct dur _ #f)
               (when (not (equal? (cons pc oct) (ST-current-pitch sm)))
                 (error 'motif->maybe-interval-motif "note no tie no match current-pitch ~v with this pitch ~v state ~v" (cons pc oct) (ST-current-pitch sm) sm))
               (let* ([this-pitch (cons pc oct)]
                      [interval (pitches->interval (ST-previous-pitch sm) this-pitch )]
                      [note-elem (list interval (ST-controls sm) (append (ST-durations sm) (list dur)))])
                 (let ([st (ST 'E (append (ST-maybe-interval-motif sm) (list note-elem)) this-pitch #f '() '())])
                   #;(printf "Note no tie:\n old state ~v\n new state ~v\n" sm st)
                   (loop st ms)))]
              [(Rest dur)
               (error 'motif->maybe-interval-motif "Unexpected Rest dur: ~v in state 'AN, state: ~v" dur sm)])])]
        ['AR
         (match mot
           ['()
            (let ([ret (append (ST-maybe-interval-motif sm) (list (list #f '() (ST-durations sm))))])
              #;(printf "ST 'AR END, state ~v\n ret: ~v" sm (list (ST-previous-pitch sm) ret))
              (list (ST-previous-pitch sm) ret))]
           [(cons m ms)
            (match m
              [(Note pc oct dur ctrls #t)
               (let* ([this-pitch (cons pc oct)]
                      [rests-elem (list #f '() (ST-durations sm))])
                 (let ([st (ST 'AN (append (ST-maybe-interval-motif sm) (list rests-elem)) (ST-previous-pitch sm) this-pitch ctrls (list dur))])
                   #;(printf "Note with tie:\n old state ~v\n new state ~v\n" sm st)
                   (loop st ms)))]
              [(Note pc oct dur ctrls #f)
               (let* ([this-pitch (cons pc oct)]
                      [interval (pitches->interval (ST-previous-pitch sm) this-pitch)]
                      [rests-elem (list #f '() (ST-durations sm))]
                      [note-elem (list interval ctrls (list dur))])
                 (let ([st (ST 'E (append (ST-maybe-interval-motif sm) (list rests-elem  note-elem)) this-pitch #f '() '())])
                   #;(printf "Note no tie\n old state ~v\n new state ~v\n" sm st)
                   (loop st ms)))]
              [(Rest dur)
               (let ([st (struct-copy ST sm (durations (append (ST-durations sm) (list dur))))])
                 #;(printf "Rest:\n old state ~v\n new state ~v\n" sm st)
                 (loop st ms))])])])))
  ;; don't set done? count higher than 10 or risk transposing beyond range of C-major scale
  (let* ([start-pitch  (cons 'C '0va)]
         [abs-motif-1  (list (list 1 '(Accent) '(E Q)) (list -1 '() '(W)))]
         [abs-motif-2  (list (list 3 '(Accent) '(S)) (list -1 '() '(S)))]
         [abs-motif-3  (list (list #f '() '(E)))]
         [abs-motif-4  (list (list #f '() '(Q W)))]
         [abs-motifs   (list abs-motif-1 abs-motif-2 abs-motif-3 abs-motif-4)]
         [w-abs-motifs (map (curry list 1) abs-motifs)]
         [motifs/gen   (weighted-maybe-intervalss-motifs/generator C-major start-pitch w-abs-motifs)]
         [motifs       (while/generator->list (sum<=? (const 1) 10) motifs/gen)])
    #;(printf "motifs: ~v\n" motifs)
    (void (foldl (lambda (motif this-start-pitch)
             (match (motif->maybe-interval-motif C-major this-start-pitch motif)
               [(list next-start-pitch maybe-interval-motif)
                (let ([is-member? (if (member maybe-interval-motif abs-motifs) #t #f)])
                  #;(printf "checking for motif: ~v\n as abs-motif: ~v\n in abs-motifs: ~v\n is-member? ~v\n" motif maybe-interval-motif abs-motifs is-member?)
                  (check-true is-member?)
                  next-start-pitch)]))
           start-pitch motifs))))


;; travelling-motifs/generator support
(define direction/c
  (make-flat-contract #:name 'direction/c #:first-order (or/c 'up 'down)))

(define/contract (maybe-interval-or-intervals->total-interval maybe-interval-or-intervals)
  (-> maybe-interval-or-intervals/c  interval/c)
  (match maybe-interval-or-intervals
    [#f 0]
    [(? interval/c interval) interval]
    [(? (non-empty-listof interval/c) intervals) (car intervals)]))

(define/contract (maybe-intervalss-motifs->total-interval maybe-intervalss-motif)
  (-> maybe-intervalss-motif/c interval/c)
  (apply + (map (compose maybe-interval-or-intervals->total-interval car) maybe-intervalss-motif)))

(define max-boost (make-parameter 5))

(define min-boost (make-parameter 1))

;; motif-total-intervals cannot be uniformly positive or uniformly negative, otherwise it would
;; be impossible to adjust the weights to reverse direction once we traverse a boundary
(define/contract (next-motif-weights starting-pitch direction pitch-range motif-total-intervals)
  (-> pitch/c direction/c pitch-range-pair/c (non-empty-listof interval/c) (non-empty-listof relative-weight/c))
  (let ([total-range (- (cadr pitch-range) (car pitch-range))])
    (match direction
      ['up
       (when (compare-pitches > starting-pitch (cadr pitch-range))
         (error 'next-motif-weights "starting-pitch: ~v > max pitch in range: ~v for direction: ~v" starting-pitch pitch-range direction))
       (let* ([available-range (- (pitch->index (cadr pitch-range)) (pitch->index starting-pitch))]
              [positive-boost (* (max-boost) (/ available-range total-range))])
         (map (lambda (total-interval) (if (positive? total-interval) positive-boost (min-boost))) motif-total-intervals))]
      ['down
       (when (compare-pitches < starting-pitch (cadr pitch-range))
         (error 'next-motif-weights "starting-pitch: ~v < max pitch in range: ~v for direction: ~v" starting-pitch pitch-range direction))
       (let* ([available-range (- (pitch->index starting-pitch) (pitch->index (car pitch-range)))]
              [negative-boost (* (max-boost) (/ available-range total-range))])
         (map (lambda (total-interval) (if (negative? total-interval) negative-boost (min-boost))) motif-total-intervals))])))

(define/contract (find-next-direction current-direction pitch-range next-starting-pitch)
  (-> direction/c pitch-range-pair/c pitch/c direction/c)
  (let ([min-pitch (car pitch-range)]
        [max-pitch (cadr pitch-range)])
    (cond
      [(and (symbol=? current-direction 'up) (compare-pitches >= next-starting-pitch max-pitch))
       'down]
      [(and (symbol=? current-direction 'down) (compare-pitches <= next-starting-pitch min-pitch))
       'up]
      [else
       current-direction])))

;; given pitch range, render motifs until range is exceeded, then change weights to favor transposition in opposite direction
;; doesn't really work:  given randomness, possible for list of transpositions with negative total to ascend anyway
(define (travelling-motifs/generator scale initial-starting-pitch pitch-range initial-direction maybe-intervalss-motifss)
  (let ([motif-total-intervals (map maybe-intervalss-motifs->total-interval maybe-intervalss-motifss)])
    (generator ()
      (let loop ([starting-pitch initial-starting-pitch]
                 [direction      initial-direction])
        (let* ([weights (next-motif-weights starting-pitch direction pitch-range motif-total-intervals)]
               [weights&motifs (zip weights maybe-intervalss-motifss)]
               [next-maybe-intervalss-motif ((weighted-list-element/generator weights&motifs))])
          (match (render-maybe-intervalss-motifs scale starting-pitch next-maybe-intervalss-motif)
            [(list next-starting-pitch motifs)
             (yield motifs)
             (let ([next-direction (find-next-direction direction pitch-range next-starting-pitch)])
               (loop next-starting-pitch next-direction))]))))))

;; not a generator because the morpher routine signals conclusion with #f
;; * initial inputs are enough to get first generation, which serves as input to morpher
;; * morpher
;;   - accepts previous scale, starting-pitch, motif, maybe-last-pitch, generations
;;   - updates scale, starting-pitch, and motif for next generation of notes
;;   - list with morphed scale, start-pitch, and motif else #f to stop
;; * answers successive generations of morphed initial-motif as notes-motifs/c
;;   renderer can annotate generations, swallows motifs
;; nb: maybe should be morph-and-render, does it all-in-one
;; (-> Scale? pitch/c maybe-intervalss-motif/c morph/c notes-motifs/c)
(define (morph-motifs initial-scale initial-start-pitch initial-ints-motif morpher)
  (let loop ([scale        initial-scale]
             [start-pitch  initial-start-pitch]
             [ints-motif   initial-ints-motif]
             [notes-motifs '()])
    (match (render-maybe-intervalss-motifs scale start-pitch ints-motif)
      [(list maybe-last-pitch new-notes-motif)
       (let ([notes-motifs (append notes-motifs (list new-notes-motif))])
         (match (morpher scale start-pitch ints-motif maybe-last-pitch notes-motifs)
           [#f
            notes-motifs]
           [(list next-scale next-start-pitch next-ints-motif)
            (loop next-scale next-start-pitch next-ints-motif notes-motifs)]))])))

(module+ test
  (require rackunit)
  ;; trivial: answer scale, motif, last-pitch as new start pitch 
  ;; so same as successive transposition of same motif,
  ;; stops after two iterations
  (define/contract (morpher scale _ motif last-pitch note-motifs)
    morph/c
    (if (= 2 (length note-motifs))
        #f
        (list scale last-pitch motif)))
  (let* ([intervals-motif (map (lambda (i) (list i '() (list 'Q))) (list 0 2 2 2 -3 2))]
         [notes-motif     (morph-motifs C-major (cons 'C '0va) intervals-motif morpher)])
    (check-equal? notes-motif
                  (list (list (Note 'C '0va 'Q '() #f)
                              (Note 'E '0va 'Q '() #f)
                              (Note 'G '0va 'Q '() #f)
                              (Note 'B '0va 'Q '() #f)
                              (Note 'F '0va 'Q '() #f)
                              (Note 'A '0va 'Q '() #f))
                        (list (Note 'A '0va 'Q '() #f)
                              (Note 'C '8va 'Q '() #f)
                              (Note 'E '8va 'Q '() #f)
                              (Note 'G '8va 'Q '() #f)
                              (Note 'D '8va 'Q '() #f)
                              (Note 'F '8va 'Q '() #f))))))


(define (increment-pan pan)
  (let ([pan-idx (index-of pan-syms pan)])
    (list-ref pan-syms (modulo (add1 pan-idx) (length pan-syms)))))

(define (increment-or-mod-pan ctrls)
  (match ctrls
    ['() (list 'PanLeft)]
    [(cons ctrl ctrls) 
     (if (pan? ctrl)
         (cons (increment-pan ctrl) ctrls)
         (cons 'PanLeft (cons ctrl ctrls)))]))

(define (increment-intervals-motif-pan motif)
  (match (car motif)
    [(list ints ctrls durs) (list ints (increment-or-mod-pan ctrls) durs)]))

(module+ test
  (require rackunit)
  (check-equal? (increment-intervals-motif-pan (list (list '() '() '()))) (list '() '(PanLeft) '()))
  (check-equal? (increment-intervals-motif-pan (list (list '() '(PanLeft) '()))) (list '() '(PanEighthLeft) '()))
  (check-equal? (increment-intervals-motif-pan (list (list '() '(PanRight) '()))) (list '() '(PanLeft) '()))
  (check-equal? (increment-intervals-motif-pan (list (list '() '(Accent) '()))) (list '() '(PanLeft Accent) '())))

;; (-> Scale? pitch/c maybe-intervalss-motif/c maybe-pitch/c notes-motifs/c (or/c #f (list/c Scale? pitch/c maybe-intervalss-motifs/c))))
(define (increment-pan-morpher scale initial-pitch motif last-pitch notes-motifs)
  (list scale initial-pitch (increment-intervals-motif-pan motif)))

