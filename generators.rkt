#lang racket

;; generators:
;; - note or rest for as long as the shortest generator
;; 
;; Utilities:
;; - generate while predicate succeeds e.g. up to some max total length
;; - repeated generator for num-denom pairs a time-signature/c

(provide
 (contract-out
  ;; synthesize Note or Rest from three input generators, else void when any input generator is done
  [note-or-rest-generator (-> (-> (or/c pitch/c false/c)) (-> duration?) (-> (or/c accent? false/c)) generator?)]
  
  ;; generate until predicate fails or generator-state is done
  [generate-while (-> predicate/c generator? (listof any/c))]

  ;; endlessly generate num-denom pair(s) from time-signature
  [time-signature->num-denom-generator (-> time-signature/c generator?)]))

;; - - - - - - - - -
;; implementation
(require racket/generator)

(require "score.rkt")

(require (only-in "utils.rkt" rotate-list-by))

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

;; (-> predicate/c generator? (listof any/c))
(define (generate-while pred gen)
  (let loop ()
    (let ([next (gen)])
      (cond [(eq? 'done (generator-state gen)) '()]
            [(pred next) (cons next (loop))]
            [else '()]))))

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

(module+ test
  (require rackunit)
  (require (only-in "utils.rkt" sum<=?))
  (require (only-in "score.rkt" voice-event->duration-int))
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
                  (list (cons 3 'Q) (cons 3 'Q) (cons 3 'Q)))
    (check-equal? (generate-while (length<=? 6) (time-signature->num-denom-generator seven-eight-time-signature))
                  (list (cons 2 'E) (cons 2 'E) (cons 3 'E) (cons 2 'E) (cons 2 'E) (cons 3 'E)))
    (check-equal? (generate-while (length<=? 9) (time-signature->num-denom-generator seven-eight-six-eight-time-signature))
                  (list (cons 2 'E) (cons 2 'E) (cons 3 'E)
                        (cons 2 'E) (cons 2 'E) (cons 2 'E)
                        (cons 2 'E) (cons 2 'E) (cons 3 'E))))
  )


