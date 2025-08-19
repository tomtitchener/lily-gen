#lang racket

;; generators:
;; - note or rest for as long as the shortest generator
;; - num-denom pairs a time-signature/c
;; 
;; Utilities:
;; - generate while predicate succeeds e.g. up to some max total length

(provide
 
  ;; non-empty-listof (list/c relative-weight/c interval/c)
  weight&intervalss/c
  
 (contract-out
  ;; proof of concept for nested generators, yield a Note or a Rest until a 
  ;; nested generator is done, then answers void without yielding so state
  ;; state becomes 'done, nested generators are pitch-or-f, duration, accent-or-f
  [note-or-rest/generator (-> generator? generator? generator? generator?)]
 
  ;; infinite generator of num-denom pair(s) from time-signature
  [time-signature->num-denom/generator (-> time-signature/c generator?)]
  
  ;; unified infinite generator of num-denom pair(s) from time-signature,
  ;; with grouping and compound time signatures aggregated together
  [time-signature->aggregate-num-denom/generator (-> time-signature/c generator?)]

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
  ))

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in algorithms repeat))

(require (only-in srfi/1 list-index find))

(require (only-in racket/sequence sequence->list))

(require (only-in seq zip unzip))

(require lily-gen/lib/score)

(require lily-gen/lib/score-utils)

(require lily-gen/lib/scale)

(require lily-gen/lib/utils)

(define weight&intervalss/c
  (make-flat-contract #:name 'weight&intervalss/c #:first-order
                      (non-empty-listof (list/c relative-weight/c interval/c))))

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

;; generator to find common end-of-measure including for compound and grouping time signatures
(define (time-signature->aggregate-num-denom/generator timesig)
  (match timesig
    ;; TimeSignatureSimple is just a single-item list of a num denom pair
    [(TimeSignatureSimple num denom)
     (sequence->repeated-generator (list (cons num denom)))]
    ;; TimeSignatureGrouping is a list of nums, a total for nums, and a denom
    ;; so aggregate is just pair tot denom
    [(TimeSignatureGrouping _ tot denom)
     (sequence->repeated-generator (list (cons tot denom)))]
    ;; TimeSignatureCompound is a list of list with each inner list
    ;; a (*list/c natural-number/c denom-duration?)
    ;; e.g. one or more natural-number/c and a denom-duration?
    ;; where denom-duration? is the denominator e.g. 3/8 + 4/4
    [(TimeSignatureCompound nums-denom-lists)
     (let ([barlen (apply + (map nums-denom-list->barlen nums-denom-lists))])
       (sequence->repeated-generator (list (cons barlen 'HTE))))]))

;; (-> (non-empty-listof (list/c relative-weight/c any/c)) generator?)
(define (weighted-list-element/generator weight&element-prs)
  (match (sequence->list (unzip weight&element-prs))
    [(list weights elements)
     (let ([buckets (gen-buckets weights)])
       (infinite-generator
        (let* ([r (random)]
               [ix (list-index (curry < r) buckets)])
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

;; like an incremental scanl, each yield shows a step,
;; if gen gives '(1 2 3) and fun is + then same as scanl
;; maybe with optional fun at the end to compute carry
;; from current acc and result of current (gen) instead of
;; always carrying acc itself, with identity function as
;; default or existing behavior if no function is provided,
;; note that carry is hidden if distinct from acc
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


