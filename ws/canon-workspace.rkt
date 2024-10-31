#lang racket

;; canon-workspace.rkt:  ressurrect canon texture
;;

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)

#|
(define pitch&offset-list/c
  (make-flat-contract #:name 'pitch&offset-list/c #:first-order (list/c pitch/c exact-nonnegative-integer?)))
  
(define pitch&offset-lists/c
  (make-flat-contract #:name 'pitch&offset-lists/c #:first-order (non-empty-listof pitch&offset-list/c)))

(define/contract (gen-rotated-canons scale start-pitch&rotate-offsets motif)
  (-> Scale? pitch&offset-lists/c maybe-intervalss-motif/c notes-motifs/c)
  (for/list ([start-pitch&offset start-pitch&rotate-offsets])
    (match start-pitch&offset
      [(list start-pitch offset)
       (rotate-list-by (second (render-maybe-intervalss-motifs scale start-pitch motif)) offset)])))

(module+ test
  (define ex-mot (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E))))
  (require rackunit)
  (check-equal? (gen-rotated-canons C-major (list (list (cons 'C '0va) 0) (list (cons 'C '8vb) 1) (list (cons 'C '15va) 2)) ex-mot)
                (list
                 (list (Note 'D '0va 'E '() #f) (Note 'E '0va 'E '() #f) (Note 'F '0va 'E '() #f))
                 (list (Note 'E '8vb 'E '() #f) (Note 'F '8vb 'E '() #f) (Note 'D '8vb 'E '() #f))
                 (list (Note 'F '15va 'E '() #f) (Note 'D '15va 'E '() #f) (Note 'E '15va 'E '() #f)))))
|#

;; morpher callback

(define morph-maybe-intervalss-motif/c (-> pitch/c notes-motif/c))

;; start-pitches tells number of voices, start-pitch for each
;; morpher closes over state: scale, maybe-intervalss-motif/c, index, etc.
(define/contract (morph-rotated-canons start-pitches morpher)
  (-> (non-empty-listof pitch/c) morph-maybe-intervalss-motif/c notes-motifs/c)
    (for/list ([start-pitch start-pitches])
      (morpher start-pitch)))

(module+ test
  (define ex-mot (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E))))
  (require rackunit)
  ;; closure captures initial state, possible to mutate all index-by-index, just index for now
  ;; for arbitrary (not-per-index) control, have first arg to morph-maybe-intervalss-motif/c be a list
  (define (make-morph-maybe-intervalls-motif scale maybe-intervalss-motif index)
    (lambda (start-pitch)
      (let* ([notes-motif (second (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif))]
             [morphed-notes-motif (rotate-list-by notes-motif index)])
        (set! index (add1 index))
        morphed-notes-motif)))
  (let ([morpher (make-morph-maybe-intervalls-motif C-major ex-mot 0)])
    (check-equal? (morph-rotated-canons (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'E '0va 'E '() #f) (Note 'F '0va 'E '() #f))
                   (list (Note 'E '8vb 'E '() #f) (Note 'F '8vb 'E '() #f) (Note 'D '8vb 'E '() #f))
                   (list (Note 'F '15va 'E '() #f) (Note 'D '15va 'E '() #f) (Note 'E '15va 'E '() #f))))))

;; generalize start-pitch to (non-empty-listof any/c) as enumerator, e.g.
;;   to add per-voice params where any/c could be (cons/c pitch/c integer?)
;;   where second element is rotate value for that voice
;; let's me parameterize scale in render-maybe-intervalss-motifs
;; also possible to parameterize morpher with arbitrary list, updated on each call,
;; or maybe index and arbitrary list, possibly empty?
;; for now, wait and see what pops out with further practice

;; something trivial to get framework running
(define maybe-intervalss-motif/param (make-parameter (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E)))))

(define start-pitches/param (make-parameter (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va))))

(define (make-morph-maybe-intervalss-motif scale maybe-intervalss-motif)
  (let ([index 0])
    (lambda (start-pitch)
      (match (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif)
        [(list end-pitch notes-motif)
         (let ([morphed-notes-motif (rotate-list-by notes-motif index)])
           (set! index (add1 index))
           morphed-notes-motif)]))))

;; next, this needs to call back to parameters for config
(define/contract (render-canon-voices)
  (-> (non-empty-listof voice/c))
  (let* ([morpher       (make-morph-maybe-intervalss-motif (scale/param) (maybe-intervalss-motif/param))]
         [voice-eventss (morph-rotated-canons (start-pitches/param) morpher)]
         [voice-pans    (voice-cnt->pan-distrib (length voice-eventss))])
    (for/list ([voice-pan    voice-pans]
               [voice-events voice-eventss])
      (SplitStaffVoice 'AcousticGrand voice-pan voice-events))))

#;(parameterize ((tempo/param (TempoDur 'Q 120)))
    (gen-score-file (score/parameterized (render-canon-voices))))

;; something to do chords 10ths + 3rds, 6ths + 3rds, octaves?
(define chord-intervalss-motif
  '(
   ((0 9) (Accent) (S)) ;; 10th
   ((0 9) () (S))
   ((0 9) () (E))
   (#f () (Q))
   ((-2 2) (Accent) (S)) ;; 11th?
   ((0 2) () (S))
   ((0 2) () (E))
   (#f () (Q))
   )
  )

(define start-chord-pitches
  `(
    ,(cons 'C '0va)
    ,(cons 'C '0va)
    )
  )

(parameterize ((maybe-intervalss-motif/param chord-intervalss-motif) (start-pitches/param start-chord-pitches))
    (gen-score-file (score/parameterized (render-canon-voices))))

;; rotating just by elements in motif seems arbitrary, better would be to rotate by a duration
;; so for example, rotating above by an eighth note, though I need to deal with forward or
;;   backward, maybe with just two APIs, rotate-motif-forward/rotate-motif-backward
;; trick is to consume moif elements by duration, so e.g. rotating above by E backward means
;;   taking half of final (#f () (Q)) and starting with (#f () (S)) and ending with the same
;;   whereas rotating forward would stick initial two 'S at end of motif with total count
;;   unchanged
;; simplification would be to assume rhythmic duration would never split a note and to err if
;;   that happened?  otherwise you'd get an additional attack
;; for implementation for positive direction, consume from the head of the list until either
;;   - exactly exhausting reserve with duration of motif element at head of list
;;   or
;;   - encounter rest that exceeds duration and can be split with remainder at beginning and
;;     rhythmic unit of duration at the tail
;; and the same for backward except taking from the end of the list and adding to the head
;; use duration->int to perform arithmetic on duration values

;; nb:  maybe-intervalss-motif/c, so this would be low-level handler for higher-level
;;      routine maybe-intervalss-motifs/c that extracted maybe-intervalss-motif/c 
(define/contract (rotate-motif-forward-by-duration motif dur)
  (->  maybe-intervalss-motif/c duration? (list/c maybe-intervalss-motif/c boolean?))
  ;; until zero? durval, pull next element off head, put on tail, and recur with remaining durval
  (define (rotate-motif-forward-by-durval motif remaining-durval split-note-flag)
    (let* ([first-motif (car motif)]
           [first-durval (apply + (map duration->int (third first-motif)))])
      (cond
        [(zero? remaining-durval)
         (list motif split-note-flag)]
        [(equal? remaining-durval first-durval)
         (rotate-motif-forward-by-durval (rotate-list-by motif 1) 0 split-note-flag)]
        ;; first motif element is longer than durval that remains,
        ;; split it between first and last elements
        ;; if it's not a rest, set split-note-flag
        [(> first-durval remaining-durval)
         (let* ([ints-or-rest (first first-motif)]
                [new-split-note-flag (if ints-or-rest #t #f)] ;; only #t for splitting a note, not a rest
                [start-elem (list ints-or-rest '() (int->durations (- first-durval remaining-durval)))]
                [end-elem (list ints-or-rest '() (int->durations remaining-durval))]
                [rotated-motif (cons start-elem (append (cdr motif) (list end-elem)))])
           (rotate-motif-forward-by-durval rotated-motif 0 new-split-note-flag))]
        ;; durval that remains is longer than first motif element
        ;; append first element and recur with difference in durvals
        [else
         (rotate-motif-forward-by-durval (rotate-list-by motif 1) (- remaining-durval first-durval) split-note-flag)])))
  (rotate-motif-forward-by-durval motif (duration->int dur) #f))

(module+ test
  (check-equal? (rotate-motif-forward-by-duration '((0 () (Q)) (#f () (E))) 'S)
                '(((0 () (E.)) (#f () (E)) (0 () (S))) #t))
  (check-equal? (rotate-motif-forward-by-duration '(((0 2) () (Q)) (#f () (E))) 'S)
                '((((0 2) () (E.)) (#f () (E)) ((0 2) () (S))) #t))
  (check-equal? (rotate-motif-forward-by-duration '((#f () (Q)) (#f () (E))) 'S)
                '(((#f () (E.)) (#f () (E)) (#f () (S))) #f))
  (check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (E))) 'S)
                '((((0 2) () (Q)) (#f () (E)) ((1 3) () (S))) #f))
  (check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (E))) 'E)
                '((((0 2) () (E.)) (#f () (E)) ((1 3) () (S)) ((0 2) () (S))) #t))
  (check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (Q))) 'Q.)
                '(((#f () (E.)) ((1 3) () (S)) ((0 2) () (Q)) (#f () (S))) #f))
)

