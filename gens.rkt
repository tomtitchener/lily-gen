#lang racket

;; Generators

(require racket/generator)
(require "score.rkt")

(provide (all-defined-out))

(define/contract (cycle-generator lst [start 0])
  (->* ((listof any/c)) (exact-nonnegative-integer?) generator?)
  (let ([i start]
        [l (length lst)])
    (infinite-generator
     (yield (list-ref lst (remainder i l)))
     (set! i (add1 i)))))

(define/contract (generator-done? gen)
  (-> generator? boolean?)
  (symbol=? 'done (generator-state gen)))

(define/contract (generate-while pred gen)
  (-> predicate/c generator? (listof any/c))
  (let loop ()
    (let ([next (gen)])
      (if (pred next)
          (cons next (loop))
          '()))))

;; answers Note? or Rest? for length of shortest generator, then void to mark generator state 'done,
;; infinite if all generators are infinite
(define/contract (note-or-rest-gen pitch-or-f-gen durations-gen accent-or-f-gen)
  (-> generator? generator? generator? generator?)
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

