#lang racket

;; generators:
;; 1) endlessly repeat elements of a list in order 
;; 2) note or rest for as long as the shortest generator
;; 
;; Utilities:
;; 1) generate while predicate succeeds

(provide
 ;; endlessly repeat elements of a list
 (contract-out
  [cycle-generator (->* ((listof any/c)) (natural-number/c) generator?)])

 ;; synthesize Note or Rest from three input generators, else void when any generator is done
 (contract-out
  [note-or-rest-generator (-> (-> (or/c pitch/c false/c)) (-> duration?) (-> (or/c accent? false/c)) (-> (or/c Note? Rest?)))])
 
 ;; generate until predicate fails or generator-state is done
 (contract-out
  [generate-while (-> predicate/c generator? (listof any/c))])
 )

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require (only-in "score.rkt" Note Rest Note? Rest? pitch/c accent? duration?))

;; (->* ((listof any/c)) (natural-number/c) generator?)
(define (cycle-generator lst [start 0])
  (let ([i start]
        [l (length lst)])
    (infinite-generator
     (yield (list-ref lst (remainder i l)))
     (set! i (add1 i)))))

;; (-> (-> (or/c pitch/c false/c)) (-> duration?) (-> (or/c accent? false/c)) (-> (or/c Note? Rest?)))
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

;; generate-while (-> predicate/c generator? (listof any/c))
(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (cond [(eq? 'done (generator-state gen)) '()]
            [(pred next) (cons next (loop))]
            [else '()]))))

(module+ test
  (require rackunit)
  (require (only-in "utils.rkt" sum<=?))
  (require (only-in "score-utils.rkt" voice-event->duration-int))
  (let* ([pitch-or-f-gen  (cycle-generator (list (cons 'C '0va) #f (cons 'E '0va)))]
         [duration-gen    (cycle-generator '(E S S E S S))]
         [accent-or-f-gen (cycle-generator '(Accent #f #f))]
         [note-or-rest-gen (note-or-rest-generator pitch-or-f-gen duration-gen accent-or-f-gen)]
         [sum-note-or-rest-durations<=? (sum<=? voice-event->duration-int 100)]
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
    (check <= (apply + (map voice-event->duration-int note-or-rests)) 100)))


