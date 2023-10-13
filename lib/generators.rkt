#lang racket

;; generators:
;; - note or rest for as long as the shortest generator
;; - num-denom pairs a time-signature/c
;; 
;; Utilities:
;; - generate while predicate succeeds e.g. up to some max total length

(provide
 (contract-out
  ;; proof of concept for nested generators, yield a Note or a Rest until a 
  ;; nested generator is done, then answers void without yielding so state
  ;; state becomes 'done, nested generators are pitch-or-f, duration, accent-or-f
  [note-or-rest-generator (-> generator? generator? generator? generator?)]
 
  ;; generate until predicate fails or generator-state is done
  [generate-while (-> predicate/c generator? (listof any/c))]

  ;; generate num-denom pair(s) from time-signature
  [time-signature->num-denom-generator (-> time-signature/c generator?)]

  ;; generate random pick from listof any/c based on weights,
  ;; e.g. weights (1 1 1) -> (1/3 1/3 1/3), (1 1 2) -> (1/4 1/4 1/2), etc.
  [weighted-list-element-generator (-> (non-empty-listof exact-positive-integer?) (non-empty-listof any/c) generator?)]

  ;; generate step-wise list of pitch/c given Scale?
  ;; exact-integer? as interval as count of steps, with start on start-pitch
  ;; and end as interval -/+ 1 (up/down) to generate interval count of steps
  [scale-steps-generator (-> Scale? pitch/c exact-integer? generator?)]

  ;; (-> Scale? pitch/c (listof exact-integer?) generator?)
  [scale-xpose-generator (-> Scale? pitch/c (listof exact-integer?) generator?)]

  ;; given a way to initialize an inner generator from the output of an
  ;; outer generator and an outer generator, return a generator that 
  ;; generates n * m elements where n is the count elements from the
  ;; outer generator and n is the count of elements from the inner
  ;; generator
  [generator-generator (-> (-> any/c generator?) generator? generator?)]

  ;; Wrap inner generator with random selection of elements including #f
  ;; based on input weights.  Length of weights must equal length of elements
  ;; answered by (inner-gen) + 1 for #f as last weight in list is weight for #f.
  ;; That is, weights (1 1 2) with inner generator that answers either 'a or 'b
  ;; means 25% chance for 'a, 25% for 'b, and 50% for #f.
  [weighted-list-or-f-generator-generator (-> (non-empty-listof exact-integer?) generator? generator?)]

  [gen-buckets (-> (listof positive?) (listof positive?))]
  ))

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in algorithms scanl))

(require (only-in srfi/1 list-index))

(require lily-gen/lib/score)

(require lily-gen/lib/scale)

(require (only-in lily-gen/lib/utils rotate-list-by sum<=?))

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

;;generators.rkt> (ez-weighted-random-list-element '(1 1 1))
;; '(1/3 1/3 1/3)
;; generators.rkt> (ez-weighted-random-list-element '(1 1 4))
;; '(1/6 1/6 2/3)
(define (gen-fractions ws) ;; (w)eight(s) 
  (let ([tot (apply + ws)])
    (map (lambda (w) (/ w tot)) ws)))

;; generators.rkt> (gen-buckets (gen-fractions '(1 1 4)))
;; '(1/6 1/3 1)
(define (gen-buckets ws) ;; (w)eight(s)
  (scanl + (gen-fractions ws)))

;; (-> (non-empty-listof exact-positive-integer?) (non-empty-listof any/c) generator?)
(define (weighted-list-element-generator weights elements)
  (unless (= (length weights) (length elements))
    (error 'weighted-random-list-element-generator "unequal lengths of weights ~v and elements ~v"
           (length weights) (length weights)))
  (let ([buckets (gen-buckets weights)])
    (infinite-generator
     (let* ([r (random)]
            [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
       (yield (list-ref elements ix))))))

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

;; chooses one of (append (inner-gen) '(#f)) based on weights
;; alternative: choose either all of (inner-gen) or #f, with
;; only two values in weights
;; (-> (non-empty-listof exact-integer?) generator? generator?)
(define (weighted-list-or-f-generator-generator weights inner-gen)
  (let ([buckets (gen-buckets weights)])
    (generator ()
      (let loop ()
        (let ([elements (append (list (inner-gen) (list #f)))])
          (cond [(symbol=? 'done (generator-state inner-gen))
                 (void)]
                [else
                 ;; cannot know ahead of time if length of weights 
                 ;; matches 1 + (length (inner-gen))
                 (unless (eq? (length elements) (length weights))
                   (error 'weighted-list-or-f-generator-generator
                          "length elements ~v is different from length weights ~v"
                          (length elements) (length weights)))
                 (let* ([r (random)]
                        [ix (list-index (lambda (bucket) (<= r bucket)) buckets)])
                   (yield (list-ref elements ix))
                   (loop))]))))))

;; (-> predicate/c generator? (listof any/c))
(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (cond [(eq? 'done (generator-state gen)) '()]
            [(pred next) (cons next (loop))]
            [else '()]))))

(module+ test
  (require rackunit)
  (require rackcheck)
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
         [outer (weighted-list-or-f-generator-generator '(1 1) inner)])
    (check-equal? (length (generate-while identity outer)) 3))
  (check-property
   (property ([nats (gen:list gen:natural)])
             ;; can't ask gen:list for a minimum number of elements, need at least two
             (let* ([weights (if (null? nats)
                                 '(1 1)
                                 (if (= 1 (length nats))
                                     (cons 1 nats)
                                     nats))]
                    [ix (list-index (lambda (bucket) (<= (random) bucket)) (gen-buckets weights))])
               (and (check >= ix 0) (check <= ix (sub1 (length weights)))))))
  )
