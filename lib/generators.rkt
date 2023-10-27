#lang racket

;; generators:
;; - note or rest for as long as the shortest generator
;; - num-denom pairs a time-signature/c
;; 
;; Utilities:
;; - generate while predicate succeeds e.g. up to some max total length

(provide
 relative-weight/c
 
 (contract-out
  ;; proof of concept for nested generators, yield a Note or a Rest until a 
  ;; nested generator is done, then answers void without yielding so state
  ;; state becomes 'done, nested generators are pitch-or-f, duration, accent-or-f
  [note-or-rest-generator (-> generator? generator? generator? generator?)]
 
  ;; inifinite generate of num-denom pair(s) from time-signature
  [time-signature->num-denom-generator (-> time-signature/c generator?)]

  ;; infinite generator of random pick from listof any/c based on weights,
  ;; lengths of weights and lengths of elements must be the same
  [weighted-list-element-generator (-> (non-empty-listof relative-weight/c) (non-empty-listof any/c) generator?)]

  ;; generate intervals from start to stop in pitch-range-pair/c in scale given
  ;; weights as listof relative-weight/c and intervals as listof exact-integer?
  [weighted-intervals-generator (-> (listof relative-weight/c) (listof exact-integer?) Scale? pitch-range-pair/c generator?)]
  
  ;; sequence generator for step-wise list of pitch/c given
  ;; Scale?, start-pitch and exact-integer? as step count, 
  ;; positive for ascending, negative for descending
  [scale-steps-generator (-> Scale? pitch/c exact-integer? generator?)]

  ;; sequence generator for transpose/absolute of scale, pitch, integer list
  [scale-xpose-generator (-> Scale? pitch/c (listof exact-integer?) generator?)]

  ;; given a way to initialize an inner generator from the output of an
  ;; outer generator and an outer generator, return a generator that 
  ;; generates n * m elements where n is the count elements from the
  ;; outer generator and n is the count of elements from the inner
  ;; generator
  [generator-generator (-> (-> any/c generator?) generator? generator?)]

  ;; choose one of (inner-gen) or '(#f) based on weights, length of 
  ;; weights must equal length of (inner-gen) + 1 (for #f)
  ;; weights (1 1 2) with inner generator that answers either 'a or 'b
  ;; means 25% chance for 'a, 25% for 'b, and 50% for #f
  [weighted-maybe-generator (-> (non-empty-listof exact-integer?) generator? generator?)]

  ;; first list is weights, second is repetitions, choose randomly among repetitions
  ;; by weights for count of repetitions for each element from input generator
  [weighted-repeats-generator (-> (non-empty-listof exact-integer?) (non-empty-listof exact-integer?) generator? generator?)]

  [motif-generator (-> Scale? pitch/c (listof (cons/c relative-weight/c abstract-motif/c)) (-> (listof (listof (or/c Note? Rest?))) boolean?) generator?)]
  
  ;; generate until predicate fails or generator-state is done,
  ;; supply (const #t) as predicate to collect all output as a list
  [generate-while (-> predicate/c generator? (listof any/c))]

  ;; answer generator that applies function to all generator results, arities must match
  [generator-map (-> (-> any/c any/c) generator? generator?)]
  ))

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in algorithms repeat))

(require (only-in srfi/1 list-index find))

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(require (only-in lily-gen/lib/utils gen-buckets inits))

(define relative-weight/c
  (make-flat-contract #:name 'relative-weight/c #:first-order exact-positive-integer?))

(define abstract-motif-element/c
  (make-flat-contract #:name 'abstract-motif-element/c #:first-order (cons/c maybe-interval/c (cons/c (listof control/c) (listof duration?)))))

(define abstract-motif/c
  (make-flat-contract #:name 'abstract-motif/c #:first-order (listof abstract-motif-element/c)))

;; (-> generator? generator? generator? generator?)
(define (note-or-rest-generator pitch-or-f-gen duration-gen accent-or-f-gen)
  (generator ()
     (define (generator-done? gen)
       (symbol=? 'done (generator-state gen)))
     (let loop ()
       (let ([pitch-or-f  (pitch-or-f-gen)]
             [duration    (duration-gen)]
             [accent-or-f (accent-or-f-gen)])
         (when (ormap generator-done? (list pitch-or-f-gen duration-gen accent-or-f-gen))
           (void))
         (if pitch-or-f  
             (let ([pitch    (car pitch-or-f)]
                   [octave   (cdr pitch-or-f)]
                   [controls (if accent-or-f (list accent-or-f) '())])
               (yield (Note pitch octave duration controls #f)))
             (yield (Rest duration)))
         (loop)))))

;; Endless generator for (cons/c natural-number/c duration?) pairs
;; according to different time signatures, simple, grouping, compound.
;; Useful for keeping track of beats and bars.
;; (-> time-signature/c generator?)
(define (time-signature->num-denom-generator timesig)
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

;; (-> (non-empty-listof relative-weight/c) (non-empty-listof any/c) generator?)
(define (weighted-list-element-generator weights elements)
  (unless (= (length weights) (length elements))
    (error 'weighted-random-list-element-generator "unequal lengths of weights ~v and elements ~v"
           (length weights) (length weights)))
  (let ([buckets (gen-buckets weights)])
    (infinite-generator
     (let* ([r (random)]
            [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
       (yield (list-ref elements ix))))))

;;(-> (listof relative-weight/c) (listof exact-integer?) Scale? pitch-range-pair/c generator?)
(define (weighted-intervals-generator weights intervals scale pitch-range-pair)
  (unless (= (length weights) (length intervals))
    (error 'weighted-intervals-generator "unequal lengths for weights ~v vs. intervals ~v" weights intervals))
  (let ([buckets       (gen-buckets weights)]
        [prev-pitch    (car pitch-range-pair)]
        [min-max-range (scale->pitch-range-pair scale)])
    (generator ()
       (let loop ()
         (let* ([ix         (list-index (lambda (bucket) (<= (random) bucket)) buckets)]
                [interval   (list-ref intervals ix)]
                [next-pitch (xpose scale min-max-range prev-pitch interval)])
             (set! prev-pitch next-pitch)
             (if (and next-pitch (compare-pitches >= next-pitch (cdr pitch-range-pair)))
                 (begin
                   (yield next-pitch)
                   (loop))
                 (void)))))))

;; (-> Scale? pitch/c exact-integer? generator?)
(define (scale-steps-generator scale start-pitch interval)
  (let* ([pitch-range-pr (scale->pitch-range-pair scale)]
         [interval-adj   (if (positive? interval) (sub1 interval) (add1 interval))]
         [stop-pitch     (xpose scale pitch-range-pr start-pitch interval-adj)])
    (sequence->generator (scale->pitch-range scale (cons start-pitch stop-pitch)))))

;; (-> Scale? pitch/c (listof exact-integer?) generator?)
(define (scale-xpose-generator scale start-pitch intervals)
  (let ([pitch-range-pr (scale->pitch-range-pair scale)])
    (sequence->generator (transpose/absolute scale pitch-range-pr start-pitch intervals))))

;; ((-> any/c generator?) generator? generator?)
(define (generator-generator gen-inner-gen outer-gen)
  (let ([inner-gen '()])
    (generator ()
      (let loop ()
        (cond
          [(symbol=? 'done (generator-state outer-gen))
           (void)]
          [(or (symbol=? 'fresh (generator-state outer-gen))
               (symbol=? 'done  (generator-state inner-gen)))
           (let ([next-outer-element (outer-gen)])
             (when (symbol=? 'suspended (generator-state outer-gen))
               (begin
                 (set! inner-gen (gen-inner-gen next-outer-element))
                 (yield (inner-gen))))
               (loop))]
          [(symbol=? 'suspended (generator-state inner-gen))
           (let ([next-inner-element (inner-gen)])
             (when (symbol=? 'suspended (generator-state inner-gen))
               (yield next-inner-element))
             (loop))])))))

(define/contract (random-or-f-fun weights)
  (-> (non-empty-listof relative-weight/c) (-> any/c any/c))
  (let ([buckets (gen-buckets weights)])
    (lambda (val)
      (unless (= (add1 (if (list? val) (length val) 1)) (length weights))
        (error 'random-or-f-fun "unequal lengths for generated value ~v vs. weights ~v" val weights))
      (let* ([r  (random)]
             [ix (list-index (lambda (bucket) (<= r bucket)) buckets)]
             [sp (if (list? val) identity list)])
        (list-ref (append (sp val) (list #f)) ix)))))

;; (-> (non-empty-listof exact-integer?) generator? generator?)
(define (weighted-maybe-generator weights gen)
  (generator-map (random-or-f-fun weights) gen))

(define/contract (random-or-reps-fun weights reps)
  (-> (non-empty-listof relative-weight/c) (non-empty-listof relative-weight/c) (-> any/c (listof any/c)))
  (unless (= (length weights) (length reps))
    (error 'random-or-reps-fun "unequal lengths for weights ~v vs. reps ~v" weights reps))
  (let ([buckets (gen-buckets weights)])
    (lambda (val)
      (let* ([r  (random)]
             [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
        (repeat (list-ref reps ix) val)))))

;; (-> (non-empty-listof exact-integer?) (non-empty-listof exact-integer?) generator? generator?)
(define (weighted-repeats-generator weights reps gen)
  (generator-map (random-or-reps-fun weights reps) gen))

;; (-> predicate/c generator? (listof any/c))
(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (cond [(eq? 'done (generator-state gen)) '()]
            [(pred next) (cons next (loop))]
            [else '()]))))

;; (-> (-> any/c any/c) generator? generator?)
(define (generator-map fun gen)
  (generator ()
    (let loop ()
      (let ([val (gen)])
        (if (symbol=? 'done (generator-state gen))
            (void)
            (begin
              (yield (fun val))
              (loop)))))))

(module+ test
  (require rackunit)
  (require (only-in lily-gen/lib/utils sum<=?))
  (require (only-in lily-gen/lib/score voice-event->duration-int))
  (let* ([pitch-or-f-gen  (sequence->repeated-generator (list (cons 'C '0va) #f (cons 'E '0va)))]
         [duration-gen    (sequence->repeated-generator '(E S S E S S))]
         [accent-or-f-gen (sequence->repeated-generator '(Accent #f #f))]
         [note-or-rest-gen (note-or-rest-generator pitch-or-f-gen duration-gen accent-or-f-gen)]
         [max-duration-int 100]
         [sum-note-or-rest-durations<=? (sum<=? voice-event->duration-int max-duration-int)]
         [note-or-rests (generate-while sum-note-or-rest-durations<=? note-or-rest-gen)])
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
    (check-equal? (generate-while (length<=? 3) (time-signature->num-denom-generator three-four-time-signature))
                  '((3 . Q) (3 . Q) (3 . Q)))
    (check-equal? (generate-while (length<=? 6) (time-signature->num-denom-generator seven-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (3 . E)))
    (check-equal? (generate-while (length<=? 9) (time-signature->num-denom-generator seven-eight-six-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (2 . E) (2 . E) (2 . E) (3 . E))))
  (let* ([elements-list '(a b c d)]
         [weighted-list-gen (weighted-list-element-generator '(1 1 1 1) elements-list)])
    (for ((_ (in-range 100))) (check member (weighted-list-gen) elements-list)))
  (check-equal? (generate-while identity (scale-steps-generator C-major '(C . 0va) 7))
                '((C . 0va) (D . 0va) (E . 0va) (F . 0va) (G . 0va) (A . 0va) (B . 0va)))
  (check-equal? (generate-while identity (scale-steps-generator C-major (cons 'C '0va) -7))
                '((C . 0va) (B . 8vb) (A  . 8vb) (G . 8vb) (F . 8vb) (E . 8vb) (D . 8vb)))
  (let* ([outer (scale-steps-generator C-major (cons 'C '8vb) 3)]
         [gen-inner (lambda (pitch) (scale-steps-generator C-major pitch 3))]
         [gen-gen (generator-generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (D . 8vb) (E . 8vb) (F . 8vb) (E . 8vb) (F . 8vb) (G . 8vb))))
  (let* ([outer (scale-steps-generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps-generator C-major pitch 3))]
         [gen-gen (generator-generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (B . 15vb) (C . 8vb) (D . 8vb) (A . 15vb) (B . 15vb) (C . 8vb))))
  (let* ([outer (scale-steps-generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps-generator C-major pitch -3))]
         [gen-gen (generator-generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (B . 15vb) (A . 15vb) (B . 15vb) (A . 15vb) (G . 15vb) (A . 15vb) (G . 15vb) (F . 15vb))))
  (check-equal? (generate-while identity (scale-xpose-generator C-major '(C . 0va) '(0 6 0 6)))
                '((C . 0va) (B . 0va) (C . 0va) (B . 0va)))
  ;; compose: (generator-generator (generator-generator ..))
  (let* ([outer (scale-steps-generator C-major (cons 'C '8va) -3)]
         [gen-inner-outer (lambda (pitch) (scale-steps-generator C-major pitch 3))]
         [gen-gen (generator-generator gen-inner-outer outer)]
         [gen-inner-inner (lambda (pitch) (scale-xpose-generator C-major pitch '(0 1 0 -1)))]
         [gen-gen-gen (generator-generator gen-inner-inner gen-gen)])
    (check-equal? (generate-while identity gen-gen-gen)
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
  (let* ([inner (scale-steps-generator C-major (cons 'C '8va) -3)]
         [outer (weighted-maybe-generator '(1 1) inner)])
    (check-equal? (length (generate-while (const #t) outer)) 3))
  ;; generator-map works with generator that answers a list as well as a val
  (let* ([inner (sequence->generator '((1 2 3) (4 5 6)))]
         [outer (generator-map (lambda (l) (cdr l)) inner)])
    (check-equal? (generate-while (const #t) outer) '((2 3) (5 6))))
  )

;; tbd: generator emits motifs
;; - state (let lambda):
;;   * pitch/c starts as #f,
;;     - first call sets to starting pitch/c
;;     - subsequent calls (not #f) set to final pitch/c in motif
;;   * (listof (listof (or/c Note Rest?))) is all previous motifs,
;;     starts as '(), updated by append with new result before yield
;; - args:
;;   * Scale?
;;   * starting pitch/c
;;   * pairs of weights and abstract motifs: (listof (cons/c relative-weight/c abstract-motif/c))
;;   * done? as (-> (listof (listof (or/c Note? Rest?))) boolean?)
;; - each call
;;   * test if the state with done? arg val
;;     - if (done? state) then return (void) without yielding
;;     - else generate new motif to state and (yield motif)
;; - to generate new motif:
;;   * transpose/successive ...
;;     ... abstract-motif-element/c includes maybe-interval/c so I need a
;;     ... new routine that accepts (listof maybe-interval/c) or to make
;;     ... the internal adjustments to transpose/successive, transpose/abolute,
;;     ... and xpose/internal
;;   * update pitch/c in state with final pitch in new motif
;;   * map motif to (or/c Note Rest?) based on maybe-interval/c in motif
;;   * append motif to list of motifs in state
;;   * yield motif
;; (-> Scale? pitch/c (listof (cons/c relative-weight/c abstract-motif/c)) (-> (listof (listof (or/c Note? Rest?))) boolean?) generator?)
(define (motif-generator scale starting-pitch weighted-abstract-motifs done?)
  (let* (;; mutable
         [begin-pitch      starting-pitch] 
         [all-motifs       '()]
         ;; const
         [pitch-range-pair (scale->pitch-range-pair scale)]
         [weights          (map car weighted-abstract-motifs)]
         [abstract-motifs  (map cdr weighted-abstract-motifs)]
         [buckets          (gen-buckets weights)])
    (generator ()
      (let loop ()
        (if (done? all-motifs)
            (void)
            (let* ([rand            (random)]
                   [rand-ix         (list-index (lambda (bucket) (<= rand bucket)) buckets)]
                   [abstract-motif  (list-ref abstract-motifs rand-ix)]
                   [maybe-intervals (map car  abstract-motif)]
                   [controlss       (map cadr abstract-motif)]
                   [durationss      (map cddr abstract-motif)]
                   [maybe-pitches   (transpose/successive scale pitch-range-pair begin-pitch maybe-intervals)]
                   [motif           (flatten (map gen-notes-or-rests maybe-pitches controlss durationss))])
              (set! all-motifs (append all-motifs (list motif)))
              (let ([maybe-pitch (find identity (reverse maybe-pitches))])
                (when maybe-pitch
                  (set! begin-pitch maybe-pitch)))
              (yield motif)
              (loop)))))))

(define/contract (gen-notes-or-rests maybe-pitch controls durations)
  (-> maybe-pitch/c (listof control/c) (listof duration?) (or/c (listof Rest?) (listof Note?)))
  (if maybe-pitch
      (let ([pitch (car maybe-pitch)]
            [octave (cdr maybe-pitch)])
        (if (= 1 (length durations))
            ;; list of one dur has dur, controls, tie #f
            (list (Note pitch octave (car durations) controls #f))
            ;; list of durs has first note with controls and tie #t, middle notes with '(), tie #t, final note with '(), tie #f
            (let ([first (Note pitch octave (car durations) controls #t)]
                  [middle (map (lambda (duration) (Note pitch octave duration '() #t)) (drop (inits durations) 1))]
                  [last  (Note pitch octave (last durations) '() #f)])
              (cons first (append middle (list last))))))
      (map (lambda (duration) (Rest duration)) durations)))
      
