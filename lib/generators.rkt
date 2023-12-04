#lang racket

;; generators:
;; - note or rest for as long as the shortest generator
;; - num-denom pairs a time-signature/c
;; 
;; Utilities:
;; - generate while predicate succeeds e.g. up to some max total length

(provide
 
  ;; typed list with - maybe-interval/c, #f => Rest else xpose interval, listof control/c, non-empty-listof duration?
  ;; multiple durations for Rest just means multiple Rests, for Notes means list of tied Notes except last
  #;maybe-interval-motif-element/c

  ;; non-empty-listof maybe-interval-motif-element/c
  #;maybe-interval-motif/c

  ;; non-empty-listof (list/c relative-weight/c interval/c)
  weight&intervalss/c
  
  ;; (non-empty-listof (list/c relative-weight/c maybe-intervals-motif/c)
  weight&maybe-intervalss-motifs/c

  ;; (non-empty-listof (list/c relative-weight/c (list/c exact-positive-integer? exact-positive-integer?)))
  #;weight&count-lengthss/c
 
 (contract-out
  ;; proof of concept for nested generators, yield a Note or a Rest until a 
  ;; nested generator is done, then answers void without yielding so state
  ;; state becomes 'done, nested generators are pitch-or-f, duration, accent-or-f
  [note-or-rest/generator (-> generator? generator? generator? generator?)]
 
  ;; inifinite generator of num-denom pair(s) from time-signature
  [time-signature->num-denom/generator (-> time-signature/c generator?)]

  ;; infinite generator of random pick from listof any/c based on relative weights
  [weighted-list-element/generator (-> (non-empty-listof (list/c relative-weight/c any/c)) generator?)]

  ;; generate intervals from pitch/c as start to stop in pitch-range-pair/c in scale given
  ;; weights as listof relative-weight/c and intervals as listof exact-integer?
  [weighted-intervals->pitches/generator (-> Scale? pitch-range-pair/c pitch/c weight&intervalss/c generator?)]
  
  ;; sequence generator for step-wise list of pitch/c given
  ;; Scale?, start-pitch and exact-integer? as step count, 
  ;; positive for ascending, negative for descending
  [scale-steps/generator (-> Scale? pitch/c exact-integer? generator?)]

  ;; sequence generator for transpose/absolute of scale, pitch, integer list
  [scale-xpose/generator (-> Scale? pitch/c (listof exact-integer?) generator?)]

  ;; given a way to initialize an inner generator from the output of an
  ;; outer generator and an outer generator, return a generator that 
  ;; generates n * m elements where n is the count elements from the
  ;; outer generator and n is the count of elements from the inner
  ;; generator
  [generator/generator (-> (-> any/c generator?) generator? generator?)]

  ;; choose one of (inner-gen) or '(#f) based on pair of relative weights
  ;; where first relative weight is for the generated value and the second
  ;; is for #f
  [weighted-maybe/generator (-> (cons/c relative-weight/c relative-weight/c) generator? generator?)]

  ;; generate until predicate fails or generator-state is done,
  ;; supply (const #t) as predicate to collect all output as a list
  [while/generator (-> predicate/c generator? generator?)]

  ;; force generator? until (not predicate/c)
  [while/generator->list (-> predicate/c generator? (listof any/c))]
  
  ;; (yield (op (generator1) (generator2)) unless either of generator1 or generator2 are 'done
  [combine-generators/generator (-> (-> any/c any/c any/c) generator? generator? generator?)]

  ;; answer generator that applies function to all generator results, arities must match
  [map/generator (-> (-> any/c any/c) generator? generator?)]

  ;; incrementally accumulate left-to-right using fun from init over generator?
  [foldl/generator (-> (-> any/c any/c any/c) any/c generator? generator?)]

  ;; generates a list of (non-empty-listof (or/ Note? Rest?)) randomly by weights until done?
  ;; done? function input is most recent (list of (Note or Rest))
  [weighted-maybe-intervalss-motifs/generator (-> Scale? pitch/c weight&maybe-intervalss-motifs/c generator?)]
  ))

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in algorithms repeat))

(require (only-in srfi/1 list-index find))

(require (only-in racket/sequence sequence->list))

(require (only-in seq zip unzip))

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(require lily-gen/lib/utils)

(require lily-gen/lib/motifs)

(define weight&intervalss/c
  (make-flat-contract #:name 'weight&intervalss/c #:first-order
                      (non-empty-listof (list/c relative-weight/c interval/c))))

(define weight&maybe-intervalss-motifs/c
  (make-flat-contract #:name 'weight&maybe-intervalss-motifs/c #:first-order
                      (non-empty-listof (list/c relative-weight/c maybe-intervalss-motifs/c))))

;; (-> generator? generator? generator? generator?)
(define (note-or-rest/generator pitch-or-f-gen duration-gen accent-or-f-gen)
  (generator ()
     (define (generator-done? gen)
       (symbol=? 'done (generator-state gen)))
     (let loop ()
       (let ([pitch-or-f  (pitch-or-f-gen)]
             [duration    (duration-gen)]
             [accent-or-f (accent-or-f-gen)])
         (when (ormap generator-done? (list pitch-or-f-gen duration-gen accent-or-f-gen))
           (void))
         (match pitch-or-f
           [#f
            (yield (Rest duration))]
           [(cons pitch octave)
             (let ([controls (if accent-or-f (list accent-or-f) '())])
               (yield (Note pitch octave duration controls #f)))])
         (loop)))))

;; Endless generator for (cons/c natural-number/c duration?) pairs
;; according to different time signatures, simple, grouping, compound.
;; Useful for keeping track of beats and bars.
;; (-> time-signature/c generator?)
(define (time-signature->num-denom/generator timesig)
  (match timesig
    ;; TimeSignatureSimple is just a single-item list of a num denom pair
    [(TimeSignatureSimple num denom)
     (sequence->repeated-generator (list (cons num denom)))]
    ;; TimeSignatureGrouping is a list of num denom pairs, one for each of nums
    [(TimeSignatureGrouping nums _ denom)
     (let ([num-denom-prs (map (lambda (num) (cons num denom)) nums)])
       (sequence->repeated-generator num-denom-prs))]
    ;; TimeSignatureCompound is the concatenated list of list of num denom pairs
    ;; with each inner list containing one list of num denom pairs per outer list
    ;; of one or nums that ends with a denom
    [(TimeSignatureCompound nums-denoms)
     (let* ([num-denom-prs (map (lambda (nums-denom)
                                  (let ([nums (take nums-denom (- (length nums-denom) 1))]
                                        [denom (last nums-denom)])
                                    (map (lambda (num) (cons num denom)) nums)))
                                nums-denoms)])
       (sequence->repeated-generator (apply append num-denom-prs)))]))

;; (-> (non-empty-listof (list/c relative-weight/c any/c)) generator?)
(define (weighted-list-element/generator weight&element-prs)
  (match (sequence->list (unzip weight&element-prs))
    [(list weights elements)
     (let ([buckets (gen-buckets weights)])
       (infinite-generator
        (let* ([r (random)]
               [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
          (yield (list-ref elements ix)))))]))

;; (-> Scale? pitch-range-pair/c pitch/c weight&intervalss/c generator?)
;; successively xpose from start-pitch via interval, stops on #f from xpose
(define (weighted-intervals->pitches/generator scale pitch-range-pair start-pitch weight&intervalss)
  (while/generator
   identity
   (foldl/generator
    (lambda (interval prev-pitch)
      (xpose scale pitch-range-pair prev-pitch interval))
    start-pitch
    (weighted-list-element/generator weight&intervalss))))

(module+ test
  (require rackunit)
  (let* ([scale C-major]
         [start-pitch (cons 'C '0va)]
         [pitch-range-pr (scale->pitch-range-pair scale)]
         [weight&intervalss (list (list 1 1) (list 1 -2) (list 1 -3))]
         [pitches-gen (weighted-intervals->pitches/generator C-major pitch-range-pr start-pitch weight&intervalss)])
    (let ([pitches (while/generator->list (sum<=? (const 1) 10) pitches-gen)])
        (foldl (lambda (pitch prev-pitch)
                 (check-true (ormap (lambda (i) (equal? pitch (xpose scale pitch-range-pr prev-pitch i)))
                                    (map second weight&intervalss)))
                 pitch)
               start-pitch pitches)
        (void))))

;; (-> Scale? pitch/c exact-integer? generator?)
(define (scale-steps/generator scale start-pitch interval)
  (let* ([pitch-range-pr (scale->pitch-range-pair scale)]
         [interval-adj   (if (positive? interval) (sub1 interval) (add1 interval))]
         [stop-pitch     (xpose scale pitch-range-pr start-pitch interval-adj)])
    (sequence->generator (scale->pitch-range scale (cons start-pitch stop-pitch)))))

;; (-> Scale? pitch/c (listof exact-integer?) generator?)
(define (scale-xpose/generator scale start-pitch intervals)
  (let ([pitch-range-pr (scale->pitch-range-pair scale)])
    (sequence->generator (transpose/absolute scale pitch-range-pr start-pitch intervals))))

;; outer-gen is like big framing wheel where each (outer-gen) element
;; initializes inner inner wheel with successive (inner-gen) until 'done
;; ((-> any/c generator?) generator? generator?)
(define (generator/generator gen-inner-gen outer-gen)
  (generator ()
    (let loop ([inner-gen #f])
      (cond
        [(symbol=? 'done (generator-state outer-gen))
         (void)]
        [(or (symbol=? 'fresh (generator-state outer-gen))
             (symbol=? 'done  (generator-state inner-gen)))
         (let ([next-outer-element (outer-gen)])
           (if (symbol=? 'suspended (generator-state outer-gen))
             (let ([new-inner-gen (gen-inner-gen next-outer-element)])
               (yield (new-inner-gen))
               (loop new-inner-gen))
             (loop #f)))] ;; outer-gen is now 'done, loop to return (void)
        [(symbol=? 'suspended (generator-state inner-gen))
         (let ([next-inner-element (inner-gen)])
           (when (symbol=? 'suspended (generator-state inner-gen))
             (yield next-inner-element))
           (loop inner-gen))])))) ;; if inner-gen is 'done, loop to gen next inner-gen

;; given a pair of relative weights representing #t and #f,
;; answer a function that takes a value, then picks randomly
;; from #t or #f given the pair of weights and for #t answers
;; the val else for #f answers #f
(define/contract (gen-random-or-f-fun weights-pr)
  (-> (cons/c relative-weight/c relative-weight/c) (-> any/c any/c))
  (let ([buckets (gen-buckets (list (car weights-pr) (cdr weights-pr)))])
    (lambda (val)
      (let* ([r  (random)]
             [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
        (list-ref (list val #f) ix)))))

;; (-> (cons/c relative-weight/c relative-weight/c) generator? generator?)
(define (weighted-maybe/generator weights-pr gen)
  (map/generator (gen-random-or-f-fun weights-pr) gen))

;; (-> (-> any/c any/c any/c) generator? generator? generator?)
(define (combine-generators/generator op gen1 gen2)
  (generator ()
    (let loop ([g1 (gen1)]
               [g2 (gen2)])
      (if (or (symbol=? 'done (generator-state gen1))
              (symbol=? 'done (generator-state gen2)))
          (void)
          (begin
            (yield (op g1 g2))
            (loop (gen1) (gen2)))))))
  
;; (-> predicate/c generator? (listof any/c))
(define (while/generator->list pred gen)
  (sequence->list (in-producer (while/generator pred gen) (void))))

;; (-> predicate/c generator? generator?)
(define (while/generator pred gen)
  (generator  ()
    (let loop ()
      (let ([next (gen)])
        (cond [(or (eq? 'done (generator-state gen))
                   (not (pred next)))
               (void)]
              [else
               (yield next)
               (loop)])))))

;; (-> (-> any/c any/c) generator? generator?)
(define (map/generator fun gen)
  (generator ()
    (let loop ([val (gen)])
      (if (symbol=? 'done (generator-state gen))
          (void)
          (begin
            (yield (fun val))
            (loop (gen)))))))

;; (-> (-> any/c any/c any/c) any/c generator? generator?)
(define (foldl/generator fun init gen)
  (generator ()
    (let loop ([g (gen)]
               [a init])
      (if (symbol=? 'done (generator-state gen))
          (void)
          (let ([acc (fun g a)])
            (yield acc)
            (loop (gen) acc))))))

(module+ test
  (require rackunit)
  (require (only-in lily-gen/lib/utils sum<=?))
  (require (only-in lily-gen/lib/score voice-event->duration-int))
  (let* ([pitch-or-f-gen  (sequence->repeated-generator (list (cons 'C '0va) #f (cons 'E '0va)))]
         [duration-gen    (sequence->repeated-generator '(E S S E S S))]
         [accent-or-f-gen (sequence->repeated-generator '(Accent #f #f))]
         [note-or-rest-gen (note-or-rest/generator pitch-or-f-gen duration-gen accent-or-f-gen)]
         [max-duration-int 100]
         [sum-note-or-rest-durations<=? (sum<=? voice-event->duration-int max-duration-int)]
         [note-or-rests (while/generator->list sum-note-or-rest-durations<=? note-or-rest-gen)])
    (check-equal? note-or-rests
                 (list
                  (Note 'C '0va 'E '(Accent) #f)
                  (Rest 'S)
                  (Note 'E '0va 'S '() #f)
                  (Note 'C '0va 'E '(Accent) #f)
                  (Rest 'S)
                  (Note 'E '0va 'S '() #f)
                  (Note 'C '0va 'E '(Accent) #f)
                  (Rest 'S)
                  (Note 'E '0va 'S '() #f)))
    (check <= (apply + (map voice-event->duration-int note-or-rests)) max-duration-int))
  (let ([three-four-time-signature (TimeSignatureSimple 3 'Q)]
        [seven-eight-time-signature (TimeSignatureGrouping (list 2 2 3) 7 'E)]
        [seven-eight-six-eight-time-signature (TimeSignatureCompound (list (list 2 2 3 'E) (list 2 2 2 'E)))]
        [length<=? (lambda (m) (sum<=? (lambda (_) 1) m))])
    (check-equal? (while/generator->list (length<=? 3) (time-signature->num-denom/generator three-four-time-signature))
                  '((3 . Q) (3 . Q) (3 . Q)))
    (check-equal? (while/generator->list (length<=? 6) (time-signature->num-denom/generator seven-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (3 . E)))
    (check-equal? (while/generator->list (length<=? 9) (time-signature->num-denom/generator seven-eight-six-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (2 . E) (2 . E) (2 . E) (3 . E))))
  (let* ([elements-list '(a b c d)]
         [weighted-list-gen (weighted-list-element/generator (sequence->list (zip '(1 1 1 1) elements-list)))])
    (for ((_ (in-range 100))) (check member (weighted-list-gen) elements-list)))
  (check-equal? (while/generator->list identity (scale-steps/generator C-major '(C . 0va) 7))
                '((C . 0va) (D . 0va) (E . 0va) (F . 0va) (G . 0va) (A . 0va) (B . 0va)))
  (check-equal? (while/generator->list identity (scale-steps/generator C-major (cons 'C '0va) -7))
                '((C . 0va) (B . 8vb) (A  . 8vb) (G . 8vb) (F . 8vb) (E . 8vb) (D . 8vb)))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) 3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (while/generator->list identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (D . 8vb) (E . 8vb) (F . 8vb) (E . 8vb) (F . 8vb) (G . 8vb))))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (while/generator->list identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (B . 15vb) (C . 8vb) (D . 8vb) (A . 15vb) (B . 15vb) (C . 8vb))))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch -3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (while/generator->list identity gen-gen)
                  '((C . 8vb) (B . 15vb) (A . 15vb) (B . 15vb) (A . 15vb) (G . 15vb) (A . 15vb) (G . 15vb) (F . 15vb))))
  (check-equal? (while/generator->list identity (scale-xpose/generator C-major '(C . 0va) '(0 6 0 6)))
                '((C . 0va) (B . 0va) (C . 0va) (B . 0va)))
  ;; compose: (generator/generator (generator/generator ..))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8va) -3)]
         [gen-inner-outer (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner-outer outer)]
         [gen-inner-inner (lambda (pitch) (scale-xpose/generator C-major pitch '(0 1 0 -1)))]
         [gen-gen-gen (generator/generator gen-inner-inner gen-gen)])
    (check-equal? (while/generator->list identity gen-gen-gen)
                  '((C . 8va) ;; (C . 8va) (C . 8va) 
                    (D . 8va)
                    (C . 8va)
                    (B . 0va)
                    (D . 8va) ;; (D . 8va)
                    (E . 8va)
                    (D . 8va)
                    (C . 8va)
                    (E . 8va) ;; (E . 8va)
                    (F . 8va)
                    (E . 8va)
                    (D . 8va)
                    (B . 0va) ;; (B . 0va) (B . 0va)
                    (C . 8va)
                    (B . 0va)
                    (A . 0va)
                    (C . 8va) ;; (C . 8va) 
                    (D . 8va)
                    (C . 8va)
                    (B . 0va)
                    (D . 8va) ;; (D . 8va)
                    (E . 8va)
                    (D . 8va)
                    (C . 8va)
                    (A . 0va) ;; (A . 0va) (A . 0va)
                    (B . 0va)
                    (A . 0va)
                    (G . 0va)
                    (B . 0va) ;; (B . 0va)
                    (C . 8va)
                    (B . 0va)
                    (A . 0va)
                    (C . 8va) ;; (C . 8va)
                    (D . 8va)
                    (C . 8va)
                    (B . 0va))))
  (let* ([inner (scale-steps/generator C-major (cons 'C '8va) -3)]
         [outer (weighted-maybe/generator (cons 1 1) inner)])
    (check-equal? (length (while/generator->list (const #t) outer)) 3))
  ;; map/generator works with generator that answers a list as well as a val
  (let* ([inner (sequence->generator '((1 2 3) (4 5 6)))]
         [outer (map/generator (lambda (l) (cdr l)) inner)])
    (check-equal? (while/generator->list (const #t) outer) '((2 3) (5 6))))
  )

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
       [pitches ;; TBD: this is a match (list (cons (? pitch-class?) (? octave?)) ...) but I want a binding
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Chord pitches duration (if first controls '()) (not last))  acc)))
                 '() durations))])]))

(define/contract (render-maybe-intervalss-motif scale starting-pitch maybe-intervalss-motif-elements)
  (-> Scale? pitch/c maybe-intervalss-motif/c (list/c maybe-pitch/c (non-empty-listof (or/c Note? Chord? Rest?))))
  (define (accum-motifs maybe-pitch-or-pitches-motif motifs)
    (cons (maybe-pitch-or-pitches-motif->motif maybe-pitch-or-pitches-motif) motifs))
  (define (maybe-pitch-or-pitches->maybe-pitch maybe-pitch-or-pitches)
    (cond
      [(not maybe-pitch-or-pitches)     #f]
      [(pitch/c maybe-pitch-or-pitches) maybe-pitch-or-pitches]
      [else                             (car maybe-pitch-or-pitches)]))
  (let ([begin-pitch starting-pitch]
        [max-pitch-range-pair (scale->pitch-range-pair scale)])
    (match (sequence->list (unzip maybe-intervalss-motif-elements))
      [(list maybe-intervalss controlss durationss)
       (let* ([maybe-pitch-or-pitchess (transpose/successive scale max-pitch-range-pair begin-pitch maybe-intervalss)]
              [motifss                 (foldr accum-motifs '() (sequence->list (zip maybe-pitch-or-pitchess controlss durationss)))]
              [maybe-pitch-or-pitches  (find identity (reverse maybe-pitch-or-pitchess))]
              [maybe-pitch             (maybe-pitch-or-pitches->maybe-pitch maybe-pitch-or-pitches)])
         (list maybe-pitch (flatten motifss)))])))

(define/contract (render-maybe-intervalss-motifs scale begin-pitch motif)
  (-> Scale? pitch/c maybe-intervalss-motifs/c (list/c maybe-pitch/c (non-empty-listof (or/c Note? Chord? Rest? Tuplet?))))
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
     (match (render-maybe-intervalss-motif scale begin-pitch motif)
       [(list next-begin-pitch motifs)
        (list next-begin-pitch (list (Tuplet num denom dur motifs)))])]
    [maybe-intervalss-motif
     (match (render-maybe-intervalss-motif scale begin-pitch maybe-intervalss-motif)
       [(list maybe-pitch motifs)
        (list (or maybe-pitch begin-pitch) motifs)])]))

;; (-> Scale? pitch/c weight&maybe-intervalss-motifs/c generator?)
;; each (generator) -> (or/c (non-empty-listof (or/c Note? Chord? Rest?)) Tuplet?)
;; each (maybe-intervals-motif/generator) from weight&maybe-intervalss-motifs gives
;; a maybe-intervals-motif/c as either a (non-empty-listof maybe-intervalss-motif-element/c),
;; a FixedPitchMaybeIntervalsMotif?, a FixedOctaveMaybeIntervalsMotif? or a TupletMaybeIntervalsMotif?
;; generates (non-empty-listof (or/c Note? Chord? Rest? Tuplet?))
(define (weighted-maybe-intervalss-motifs/generator scale starting-pitch weight&maybe-intervalss-motifs)
  (let ([maybe-intervals-motif/generator (weighted-list-element/generator weight&maybe-intervalss-motifs)])
    (generator ()
       (let loop ([begin-pitch             starting-pitch]
                  [maybe-intervalss-motifs (maybe-intervals-motif/generator)])
         (match (render-maybe-intervalss-motifs scale begin-pitch maybe-intervalss-motifs)
           [(list next-begin-pitch motifs)
            (yield motifs)
            (loop next-begin-pitch (maybe-intervals-motif/generator))])))))
    
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

;; parameters:
;; a) inner generator
;; b) weighted list of pairs of exact-positive-integer: backtrack count / backtrack length
;;    Note: sum of backtrack length should be <= max backtrack count
;; c) weighted list of exact-positive-integer: running length (when to backtrack)
;;
;; idea is to emit a minimal series of (gen) to >= max (backtrack count) so when you backtrack
;; you can't go past the start of the list
;;
;; once past that minimum, pick a running length from c) and emit that many (gen)
;; after which pick a pair of backtrack count / backtrack length from b) AND a
;; new running length from c) and emit the previous (gen) counting from the end of
;; the list backward for backtrack length going forward until EITHER backtrack
;; length is exhausted at which you pick a new running length from c) and start
;; accumulating fresh (gens) until count runs down OR running length is exhausted
;; and you do the same
;; 
;; needs inner state
;; * to reflect whether you're currently
;;   a) in the initialization stage where you're accumulating a list of (gen)s to
;;      backtrack over (maybe using running length set to (max backtrack count)
;;   b) pulling from new calls to (gen) while working through running length
;;   c) pulling from backtrack lists while working through running length
;; * current running length (init to max backtrack count)
;; * current backtrack count and backtrack length (init to 0)
;; 
;; note:
;; * always accumulate list of (gen)s even when backtracking, so it's possible to
;;   backtrack over previously backtracked space, maybe optimize by only remembering
;;   max backtrack count
;; * generate forever and let the caller determine when to stop
;;
;; but does this really work, isn't there state in gen that carries over from
;; call to call such that there'll be a sharp discontinuity after the completion
;; of a backtrack and resumption of gen?
;; that's certainly the case for weighted-motif/generator, where the inner loop
;; remembers the end pitch from the previous generation and uses it to convert
;; the next maybe-interval motif into a note-or-rest motif
;; so I can back up and repeat ok, but when I next resume calling the inner 
;; generator it's going to be from the pitch where I stopped the last time
;;
;; if I'm going to make this work with weighted-motif/generator then I'll need to
;; * create a routine to extract the latest pitch going backward in the list of
;;   list in my history
;; * re-initialize the inner weighted-motifs/generator with that pitch and abandon
;;   the one from the argument
;;
;; all this seems like what I really want is to yank the guts of weighted-motifs/generator
;; into a new backtrack-weighted-motifs/generator where I do that all internally, ugh
;;
;; or else parameterize with a way to initialize the inner generator so you can reset it
;; upon resumption, which can be generic in the sense of handing it the inner state with
;; the list of list already generated as input, then I just need adapters to wrap the
;; weighted-motifs/generator with, yes? and I need an implementation that takes the
;; list of list  context, the adaptor, and gives the new generator, a little less ugh?
;;
;; note I already have generator/generator (-> (-> any/c generator?) generator? generator)
;; which I think probably solves my problems if what I pass in is a let lambda with Scale?
;; and weight&maybe-interval-motifss/c bound and the new any/c input being the (listof (listof any/c))
;; that the implementation uses to extract the remaining pitch/c for the nested weighted-motifs/generator
;;
;; note this seems like a generalized version of generator/generator, except it's  not
;; because generator/generator is an inner, free-running clockwork that outputs an entire
;; sequence at once
;;
;; maybe I jump ahead trying for premature optimization and getting myself all confused
;;
;; I started off with weighted list of count / length pairs and weighted list of lengths
;; which are used to drive backtrack parameters
;;
;; note I haven't parameterized decision *to* backtrack though maybe it doesn't matter
;; seeing as once I've backtracted I'm off afresh anyway by the length in the count / length
;; pair
;;
;; *separately* there's the issue of re-initializing the generator itself, where to be flexible
;; and have this work for more than just the weighted-motifs/generator, you want a function that
;; takes the context, here the accumulated output to date (modulo some reasonable cutoff) which
;; will first be implemented as a backward seach for the last pitch before the backtrack point
;; 
;; and I guess for flexibility *both* gen and regen-gen can have details abstracted away so
;; all the bits specific to weighted-motif/generator *and* the weighted backtrack parameters
;; ... well maybe not so much, unless maybe these aren't constant over regeneration?
;;
;; maybe it makes more sense to just pass in the gen-gen, which can recognize initialization
;; because there's not state to pass in
;;
;; then I only bind the constants to the weighted-motif/generator to get the (-> pitch/c generator?)
;; once (here generalized to (-> any/c generator?), and maybe it'd just be easiest to pass the weighted
;; pairs as their own unchanging values for now
;;
;; but crap, all I'm going to do for those weights is feed it into weighted-list-element/generator
;; so why not just pass in the generator?
;;
;; because I need to know the minimal amount to create before I start backtracking, which is
;; 
;;

#|

#;(define positive-integers/c
  (make-flat-contract #:name 'count-lengths/c #:first-order (list/c exact-positive-integer? exact-positive-integer?)))

#;(define weight&count-lengthss/c
    (make-flat-contract #:name 'weight&count-lengthss/c #:first-order (non-empty-listof (list/c relative-weight/c positive-integers/c))))

  
(define/contract (backtrack-generator/generator start-pitch weight&cntlenss gen-gen)
  (-> pitch/c weight&count-lengths/c (-> any/c generator?) generator?)
  (let ([init-len (max (map (compose first second) weight&cntlenss))]
        [cntlens/gen (weighted-list-element/generator weight&cntlenss)])
    (let loop ([history '()]
               [cnt&len (cntlens/gen)]
               [gen     (gen-gen start-pitch)])
      (generator ()
        (cond ;; init 
              [(< (length history) init-len)
               (let ([next-gen (gen)])
                 (yield next-gen)
                 (loop (append history (gen)) cnt&len gen))]
              ;; filling current len
              [(??)
               (loop (append history (gen)) cnt&len gen)]
              ;; starting a new gen
              [else
               (void)])))))
|#

