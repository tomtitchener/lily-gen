#lang racket

;; Instances of generator:
;; 1) endlessly repeat elements of a list in order 
;; 2) generate note or rest for as long as the shortest generator
;; 
;; Generator tilities:
;; 1) generate while predicate succeeds

(provide
 ;; endlessly repeat elements of a list
 (contract-out
  [cycle-generator (->* ((listof any/c)) (exact-nonnegative-integer?) generator?)])

 ;; synthesize Note or Rest from three input generators, else void when any generator is done
 (contract-out
  [note-or-rest-generator (-> (-> (or/c pitch/c #f)) (-> duration?) (-> (or/c accent? #f)) (-> (or/c Note? Rest?)))])
 
 ;; generate while predicate succeeds
 (contract-out
  [generate-while (-> predicate/c generator? (listof any/c))])
 )

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in "score.rkt" Note Rest Note? Rest? pitch/c accent? duration?))

(define (cycle-generator lst [start 0])
  (let ([i start]
        [l (length lst)])
    (infinite-generator
     (yield (list-ref lst (remainder i l)))
     (set! i (add1 i)))))

(define (note-or-rest-generator pitch-or-f-gen durations-gen accent-or-f-gen)
  (define (generator-done? gen)
    (symbol=? 'done (generator-state gen)))
  (infinite-generator
   (let ([pitch-or-f  (pitch-or-f-gen)]
         [duration    (durations-gen)]
         [accent-or-f (accent-or-f-gen)])
     (when (ormap generator-done? (list pitch-or-f-gen durations-gen accent-or-f-gen))
       void)
     (if pitch-or-f
         (let ([pitch    (car pitch-or-f)]
               [octave   (cdr pitch-or-f)]
               [controls (if accent-or-f (list accent-or-f) '())])
           (yield (Note pitch octave duration controls #f)))
         (yield (Rest duration))))))

(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (if (pred next)
          (cons next (loop))
          '()))))

#|
(define pitch-or-f-gen  (cycle-generator (list (cons 'C '0va) #f (cons 'E '0va))))
(define duration-gen    (cycle-generator '(E S S E S S)))
(define accent-or-f-gen (cycle-generator '(Accent #f #f)))
(define note-or-rest-gen (note-or-rest-gen pitch-or-f-gen duration-gen accent-or-f-gen))
(define sum-note-or-rest-durations<=? (sum<=? voice-event->duration-int note-or-rest-gen))
(define note-or-rests (generate-while note-or-rest-gen 
|#

#|
Re: random subsystem in Racket.  Want to use (random-seed (current-milliseconds)) to get started.
For recreating same number stream in a new score, save (current-milliseconds) via number->string
to seed field in score.  There's better ways in Racket to initialize the pseudo-random number
generator but I'm not sure they're worth the effort of recording a vector in the seed.

Remember also progression by increasing intensity, e.g. for period use series like fibonacci, 
e.g. 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, so the rate of change accelerates.  

|#

