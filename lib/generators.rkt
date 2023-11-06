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
  maybe-interval-motif-element/c

  ;; non-empty-listof maybe-interval-motif-element/c
  maybe-interval-motif/c

 ;; non-empty-listof (list/c relative-weight/c interval/c)
 weight&intervalss/c
 
 (contract-out
  ;; proof of concept for nested generators, yield a Note or a Rest until a 
  ;; nested generator is done, then answers void without yielding so state
  ;; state becomes 'done, nested generators are pitch-or-f, duration, accent-or-f
  [note-or-rest/generator (-> generator? generator? generator? generator?)]
 
  ;; inifinite generate of num-denom pair(s) from time-signature
  [time-signature->num-denom/generator (-> time-signature/c generator?)]

  ;; infinite generator of random pick from listof any/c based on weights,
  ;; lengths of weights and lengths of elements must be the same
  [weighted-list-element/generator (-> (non-empty-listof (list/c relative-weight/c any/c)) generator?)]

  ;; generate intervals from start to stop in pitch-range-pair/c in scale given
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

  ;; TBD: every other routine answers a generator and it's up to the caller to decide
  ;; how to use it.  This feels out of place answers a list instead of a generator.
  ;;
  ;; See (in/generator gen (void)) which answers a sequence, so you could say sequence->list
  ;; with the result to get a list.
  ;;
  ;; Similarly, see sequence-map as a way to implement map/generator trivially
  ;;
  ;; generate until predicate fails or generator-state is done,
  ;; supply (const #t) as predicate to collect all output as a list
  [generate-while (-> predicate/c generator? (listof any/c))]

  ;; (yield (op (generator1) (generator2)) unless either of generator1 or generator2 are 'done
  [combine-generators/generator (-> (-> any/c any/c any/c) generator? generator? generator?)]

  ;; answer generator that applies function to all generator results, arities must match
  [map/generator (-> (-> any/c any/c) generator? generator?)]

  ;; incrementally accumulate left-to-right using fun from init over generator?
  [foldl/generator (-> (-> any/c any/c any/c) any/c generator? generator?)]

  ;; generates a list of list of (Note or Rest) randomly by weights with last arg as done?
  ;; done? function input is most recent (list of (Note or Rest))
  [generate-weighted-motifs
   (-> Scale?
       ;; starting-pitch
       pitch/c
       ;; weighted-list of maybe-interval-motif, transform into list of Note or Rest 
       (non-empty-listof (list/c relative-weight/c maybe-interval-motif/c))
       ;; done? terminating routine that looks at most recent list of (Note or Rest) only, accumulates itself if necessary
       (-> (non-empty-listof (or/c Note? Rest?)) boolean?)
       ;; -> list of list of Note or Rest
       (non-empty-listof (non-empty-listof (or/c Note? Rest?))))]
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

(define weight&intervalss/c
  (make-flat-contract #:name 'weight&intervalss/c #:first-order (non-empty-listof (list/c relative-weight/c interval/c))))

(define maybe-interval-motif-element/c
  (make-flat-contract #:name 'maybe-interval-motif-element/c #:first-order (list/c maybe-interval/c (listof control/c) (non-empty-listof duration?))))

(define maybe-interval-motif/c
  (make-flat-contract #:name 'maybe-interval-motif/c #:first-order (non-empty-listof maybe-interval-motif-element/c)))

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
(define (weighted-list-element/generator weights&elements)
  (match (sequence->list (unzip weights&elements))
    [(list weights elements)
     (let ([buckets (gen-buckets weights)])
       (infinite-generator
        (let* ([r (random)]
               [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
          (yield (list-ref elements ix)))))]))

;; (-> Scale? pitch-range-pair/c pitch/c weight&intervalss/c generator?)
(define (weighted-intervals->pitches/generator scale pitch-range-pair start-pitch weight&intervalss)
  (foldl/generator
   (lambda (interval prev-pitch)
     (transpose/unguarded scale prev-pitch pitch-range-pair interval))
   start-pitch
   (weighted-list-element/generator weight&intervalss)))

(module+ test
  (require rackunit)
  (define (done? cnt)
    (let ([i 0])
      (lambda (_)
        (set! i (add1 i))
        (> i cnt))))
  (let* ([scale C-major]
         [start-pitch (cons 'C '0va)]
         [pitch-range-pr (scale->pitch-range-pair scale)]
         [weight&intervalss (list (list 1 1) (list 1 -2) (list 1 -3))]
         [pitches-gen (weighted-intervals->pitches/generator C-major pitch-range-pr start-pitch weight&intervalss)])
    (let ([pitches (generate-while (compose not (done? 10)) pitches-gen)])
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
(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (cond [(eq? 'done (generator-state gen)) '()]
            [(pred next) (cons next (loop))]
            [else '()]))))

;; (-> (-> any/c any/c) generator? generator?)
(define (map/generator fun gen)
  (generator ()
    (let loop ()
      (let ([val (gen)])
        (if (symbol=? 'done (generator-state gen))
            (void)
            (begin
              (yield (fun val))
              (loop)))))))

;; e.g. (gen) tells next maybe-interval, init tells start-pitch
;; and acc is transposition of prev-pitch with interval to
;; carry prev-pitch through list of intervals from start-pitch
;; terminates at 'done generator-state for gen or #f for (gen)
;; 
;; (-> (-> any/c any/c any/c) any/c generator? generator?)
(define (foldl/generator fun init gen)
  (generator ()
    (let loop ([g (gen)]
               [a init]) 
      (if (or (symbol=? 'done (generator-state gen))
              (not a)
              (not g))
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
    (check-equal? (generate-while (length<=? 3) (time-signature->num-denom/generator three-four-time-signature))
                  '((3 . Q) (3 . Q) (3 . Q)))
    (check-equal? (generate-while (length<=? 6) (time-signature->num-denom/generator seven-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (3 . E)))
    (check-equal? (generate-while (length<=? 9) (time-signature->num-denom/generator seven-eight-six-eight-time-signature))
                  '((2 . E) (2 . E) (3 . E) (2 . E) (2 . E) (2 . E) (2 . E) (2 . E) (3 . E))))
  (let* ([elements-list '(a b c d)]
         [weighted-list-gen (weighted-list-element/generator (sequence->list (zip '(1 1 1 1) elements-list)))])
    (for ((_ (in-range 100))) (check member (weighted-list-gen) elements-list)))
  (check-equal? (generate-while identity (scale-steps/generator C-major '(C . 0va) 7))
                '((C . 0va) (D . 0va) (E . 0va) (F . 0va) (G . 0va) (A . 0va) (B . 0va)))
  (check-equal? (generate-while identity (scale-steps/generator C-major (cons 'C '0va) -7))
                '((C . 0va) (B . 8vb) (A  . 8vb) (G . 8vb) (F . 8vb) (E . 8vb) (D . 8vb)))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) 3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (D . 8vb) (E . 8vb) (F . 8vb) (E . 8vb) (F . 8vb) (G . 8vb))))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (D . 8vb) (E . 8vb) (B . 15vb) (C . 8vb) (D . 8vb) (A . 15vb) (B . 15vb) (C . 8vb))))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8vb) -3)]
         [gen-inner (lambda (pitch) (scale-steps/generator C-major pitch -3))]
         [gen-gen (generator/generator gen-inner outer)])
    (check-equal? (generate-while identity gen-gen)
                  '((C . 8vb) (B . 15vb) (A . 15vb) (B . 15vb) (A . 15vb) (G . 15vb) (A . 15vb) (G . 15vb) (F . 15vb))))
  (check-equal? (generate-while identity (scale-xpose/generator C-major '(C . 0va) '(0 6 0 6)))
                '((C . 0va) (B . 0va) (C . 0va) (B . 0va)))
  ;; compose: (generator/generator (generator/generator ..))
  (let* ([outer (scale-steps/generator C-major (cons 'C '8va) -3)]
         [gen-inner-outer (lambda (pitch) (scale-steps/generator C-major pitch 3))]
         [gen-gen (generator/generator gen-inner-outer outer)]
         [gen-inner-inner (lambda (pitch) (scale-xpose/generator C-major pitch '(0 1 0 -1)))]
         [gen-gen-gen (generator/generator gen-inner-inner gen-gen)])
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
  (let* ([inner (scale-steps/generator C-major (cons 'C '8va) -3)]
         [outer (weighted-maybe/generator (cons 1 1) inner)])
    (check-equal? (length (generate-while (const #t) outer)) 3))
  ;; map/generator works with generator that answers a list as well as a val
  (let* ([inner (sequence->generator '((1 2 3) (4 5 6)))]
         [outer (map/generator (lambda (l) (cdr l)) inner)])
    (check-equal? (generate-while (const #t) outer) '((2 3) (5 6))))
  )

;;   (-> (list/c maybe-pitch/c (listof control/c) (listof duration?)) (non-empty-listof (or/c Note? Rest?)))
(define (maybe-pitch-motif->motif maybe-pitch-motif)
  (match maybe-pitch-motif
    [(list maybe-pitch controls durations)
     (match maybe-pitch
       [#f
        (map Rest durations)]
       [(cons pitch octave)
        (let ([count-durs (length durations)])
          (foldr (lambda (duration acc)
                   (let ([last  (null? acc)]
                         [first (= (sub1 count-durs) (length acc))])
                     (cons (Note pitch octave duration (if first controls '()) (not last))  acc)))
                 '() durations))])]))

;; (compose (compose sequence->list zip) ... (compose sequence->list unzip)) is noisy
;; but incrementally applying (scanl (op-maybe +) _) to maybe-interval/c is noisier
;; (-> Scale? pitch/c generator? generator?)
(define (motif-gen scale starting-pitch maybe-interval-motif-gen)
  (define (accum-motifs maybe-pitch-motif motifs)
    (cons (maybe-pitch-motif->motif maybe-pitch-motif) motifs))
  (let ([max-pitch-range-pair (scale->pitch-range-pair scale)]) ;; const
    (generator ()
       (let loop ([begin-pitch starting-pitch])
         (match (sequence->list (unzip (maybe-interval-motif-gen)))
           [(list maybe-intervals controlss durationss)
            (let* ([maybe-pitches (transpose/successive scale max-pitch-range-pair begin-pitch maybe-intervals)]
                   [motifss (foldr accum-motifs '() (sequence->list (zip maybe-pitches controlss durationss)))]
                   [maybe-pitch (find identity (reverse maybe-pitches))])
              (yield (flatten motifss))
              (loop (if maybe-pitch maybe-pitch begin-pitch)))])))))

(define (generate-weighted-motifs scale starting-pitch weights&maybe-interval-motifs done?)
  (let ([maybe-interval-motif-gen (weighted-list-element/generator weights&maybe-interval-motifs)])
    (generate-while (compose not done?) (motif-gen scale starting-pitch maybe-interval-motif-gen))))

;; FSM for parsing a motif into an maybe-interval motif to check results from generate-weighted-motifs.
;; Would would work as a grammar?  Took a lot of work to implement from scratch and more to debug.
;; TBD: use logging instead of printf so messages can be enabled from REPL?
;; Note there's a racket logger running a logger integrated with emacs Racket mode.
(module+ test
  (require rackunit)
  (require (only-in lily-gen/lib/utils sum<=?))
  (define/contract (motif->maybe-interval-motif scale starting-pitch motif)
    (-> Scale? pitch/c (non-empty-listof (or/c Note? Rest?)) (list/c pitch/c maybe-interval-motif/c))
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
  (let* ([start-pitch (cons 'C '0va)]
         [abs-motif-1 (list (list 1 '(Accent) '(E Q)) (list -1 '() '(W)))]
         [abs-motif-2 (list (list 3 '(Accent) '(S)) (list -1 '() '(S)))]
         [abs-motif-3 (list (list #f '() '(E)))]
         [abs-motif-4 (list (list #f '() '(Q W)))]
         [abs-motifs  (list abs-motif-1 abs-motif-2 abs-motif-3 abs-motif-4)]
         [motifs      (generate-weighted-motifs C-major start-pitch (map (lambda (motif) (list 1 motif)) abs-motifs) (done? 10))])
    #;(printf "motifs: ~v\n" motifs)
    (void (foldl (lambda (motif this-start-pitch)
             (match (motif->maybe-interval-motif C-major this-start-pitch motif)
               [(list next-start-pitch maybe-interval-motif)
                (let ([is-member? (if (member maybe-interval-motif abs-motifs) #t #f)])
                  #;(printf "checking for motif: ~v\n as abs-motif: ~v\n in abs-motifs: ~v\n is-member? ~v\n" motif abs-motif abs-motifs is-member?)
                  (check-true is-member?)
                  next-start-pitch)]))
           start-pitch motifs))))

