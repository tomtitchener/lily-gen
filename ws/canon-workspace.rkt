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
  (define (make-list-morph-maybe-intervalls-motif scale maybe-intervalss-motif index)
    (lambda (start-pitch)
      (let* ([notes-motif (second (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif))]
             [morphed-notes-motif (rotate-list-by notes-motif index)])
        (set! index (add1 index))
        morphed-notes-motif)))
  (let ([morpher (make-list-morph-maybe-intervalls-motif C-major ex-mot 0)])
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

;; proof of concept:  rotates entire list incrementally by 0-based index of voice in list, so
;; each voice is rotated forward by one element vs. previous voice
;; note: calls list-rotate on *rendered* maybe-intervalss-motif,
;;  i.e. list of Note, Rest, Chord, or Tuplet, not maybe-intervalss-motif
(define (make-list-morph-maybe-intervalss-motif scale maybe-intervalss-motif)
  (let ([index 0])
    (lambda (start-pitch)
      (match (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif)
        [(list _ notes-motif)
         (let ([morphed-notes-motif (rotate-list-by notes-motif index)])
           (set! index (add1 index))
           morphed-notes-motif)]))))

;; next, this needs to call back to parameters for config
(define/contract (render-canon-voices)
  (-> (non-empty-listof voice/c))
  (let* ([morpher       (make-list-morph-maybe-intervalss-motif (scale/param) (maybe-intervalss-motif/param))]
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
(define/contract (rotate-motif-forward-by-duration motif durs)
  (->  maybe-intervalss-motif/c (listof duration?) (list/c maybe-intervalss-motif/c boolean?))
  ;; pluck (or/c integer? (listof integer?) false/c) as single integer? from motif
  (define (motif->int motif) (match (car motif) [#f 0] [(list ints) (car ints)] [i i]))
  ;; until zero? durval, pull next element off head, put on tail, and recur with remaining durval
  (define (rotate-motif-forward-by-durval motif remaining-durval split-note-flag)
    (let* ([first-motif (car motif)]
           [first-durval (apply + (map duration->int (third first-motif)))])
      (cond
        [(zero? remaining-durval)
         (if split-note-flag
             (let* ([last-motif (last motif)]
                    [intervals (map motif->int (cdr (drop-right motif 1)))]
                    [new-interval (- 0 (apply + intervals))]
                    [last-interval (if (list? (first last-motif)) (cons new-interval (cdr (first last-motif))) new-interval)]
                    [new-last-motif (list last-interval (second last-motif) (third last-motif))])
               (list (append (drop-right motif 1) (list new-last-motif)) split-note-flag))
             (list motif split-note-flag))]
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
  (rotate-motif-forward-by-durval motif (apply + (map duration->int durs)) #f))

;; can't split pitch and maintain interval representation and have return to same pitch as start,
;; much less with tie to connect it to next repetition

;; is it possible to fix up the last interval by undoing the intervals since the first pitch when
;; there's a fixup?

;; say the interval sequence is 1 2 3 and the first 1 gets split so the raw rotation looks like
;; 1 2 3 1

;; at the end of rotate-motif-forward-by-duration when (zero? remaining-durval) succeeds, I can look
;; at the split-note-flag and when it's #t then I can take the intervals *after* the first and *before*
;; the last e.g. (2 3) above and sum and negate them -5 and swap that in for the last interval e.g.
;; 1 2 3 -5, so instead of pitches (D . 8vb) (F . 8vb) (B . 8vb) (C . 0va) I'd get
;; (D . 8vb) (F . 8vb) (B . 8vb) (D . 8vb) though it'd have to be later on where I remember
;; the split flag and tag the final note with a tie if I'm going to be repeating

(module+ test
  ;; splits for rest and note
  (check-equal? (rotate-motif-forward-by-duration '((0 () (Q)) (#f () (E))) '(S))
                '(((0 () (E.)) (#f () (E)) (0 () (S))) #t))
  (check-equal? (rotate-motif-forward-by-duration '(((0 2) () (Q)) (#f () (E))) '(S))
                '((((0 2) () (E.)) (#f () (E)) ((0 2) () (S))) #t))
  (check-equal? (rotate-motif-forward-by-duration '((#f () (Q)) (#f () (E))) '(S))
                '(((#f () (E.)) (#f () (E)) (#f () (S))) #f))
  (check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (E))) '(S))
                '((((0 2) () (Q)) (#f () (E)) ((1 3) () (S))) #f))

  ;; does this example break my design?
  #;(check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (E))) '(E))
                '((((0 2) () (E.)) (#f () (E)) ((1 3) () (S)) ((0 2) () (S))) #t))
  
  (check-equal? (rotate-motif-forward-by-duration '(((1 3) () (S)) ((0 2) () (Q)) (#f () (Q))) '(Q.))
                '(((#f () (E.)) ((1 3) () (S)) ((0 2) () (Q)) (#f () (S))) #f))
  ;; need something other 1 1 1 for intervals or else rotating intervals list makes no difference
  (define ex-mot-2 (list (list 1 '() (list 'E)) (list 2 '() (list 'E)) (list 3 '() (list 'E))))
  (define (make-dur-morph-maybe-intervalls-motif scale dur maybe-intervalss-motif)
    (let ([index 0])
      (lambda (start-pitch)
        ;; here is where I lose flag to tell if a pitch was split during the rotation
        (let* ([morphed-intervalss-motif (first (rotate-motif-forward-by-duration maybe-intervalss-motif (make-list index dur)))]
               [notes-motif (second (render-maybe-intervalss-motifs scale start-pitch morphed-intervalss-motif))])
          (set! index (add1 index))
          notes-motif))))
  ;; no splits with 'Q for rotate dur 
  (let ([morpher (make-dur-morph-maybe-intervalls-motif C-major 'Q ex-mot-2)])
    (check-equal? (morph-rotated-canons (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'F '0va 'E '() #f) (Note 'B '0va 'E '() #f))       ;; 1 2 3
                   (list (Note 'F '8vb 'E '() #f) (Note 'G '8vb 'E '() #f) (Note 'B '8vb 'E '() #f))       ;; 3 1 2
                   (list (Note 'E '15va 'E '() #f) (Note 'A '15va 'E '() #f) (Note 'B '15va 'E '() #f))))) ;; 2 3 1
  ;; splits with 'S for rotate dur, with last pitch *not* same as first pitch 
  (let ([morpher (make-dur-morph-maybe-intervalls-motif C-major 'S ex-mot-2)])
    (check-equal? (morph-rotated-canons (list (cons 'C '0va) (cons 'C '8vb)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'F '0va 'E '() #f) (Note 'B '0va 'E '() #f))
                   (list (Note 'D '8vb 'S '() #f) (Note 'F '8vb 'E '() #f) (Note 'B '8vb 'E '() #f) (Note 'D '8vb 'S '() #f))))))

;; when rotating an interval list, the rendered notes will start in the middle of the pattern
;; so the new first note will be e.g. here, a repetition (0) of the starting pitch, and the
;; pitch pattern will *not* be preserved

;; so maybe this whole business of rotating intervals is bogus and to preserve the canon, I
;; need to do my rotating after rendering

;; brings into focus the larger context of manipulation, do I have separate layers to generate
;; canon "kernels" vs. to execute the voice rotations

;; goals:
;;
;; * regular permutation/modifications to kernel using intervals motif,
;    e.g. successive wave transforms passing through kernel, half time /
;;   double-time, insertion / extraction (elaboration / simplification)
;;   repetition ...
;;
;; * arrangement of multiple voices in a canon using notes motif based
;;   on duration
;;
;; how does rendering work, apply series of morphs to the kernel, then
;; render each with different starting pitches, then rotate per voice
;;
;; from a just get something done perspective, first step would be routine
;; to rotate a notes-motif by a given duration

;; doesn't work:  in the event int->durations answers a list with more than
;; one value, have to make tied values

;; (-> voice-event/c natural-number/c voice-event/c)(
(define (swap-voice-event-duration voice-event durint is-last)
  (define (set-tie note-or-chord tie)
    (match note-or-chord
      [(Note p o dur c _) (Note p o dur c tie)]
      [(Chord ps dur c _) (Chord ps dur c tie)]))
  (let ([durs (int->durations durint)])
    (match voice-event
      [(Note p o _ c t)  (let* ([durs (int->durations durint)]
                                [notes (map (lambda (dur) (Note p o dur c #t)) durs)])
                             (if (= 1 (length notes))
                                 (list (set-tie (last notes) (if is-last #f t)))
                                 (append (drop-right notes 1) (list (set-tie (last notes) #t)))))]
      [(Rest dur)          (map Rest (int->durations durint))]
      [(Chord ps _ c t)  (let* ([durs (int->durations durint)]
                                [chords (map (lambda (dur) (Chord ps dur c #t)) durs)])
                             (if (= 1 (length chords))
                                 (list (set-tie (last chords) (if is-last #f t)))
                                 (append (drop-right chords 1) (list (set-tie (last chords) #t)))))]
      [voice-event         (error 'swap-voice-event-duration "unexpected voice-event: ~v" voice-event)])))

;; answers list of:
;; - rotated notes
;; - flag to say if initial Note or Chord got split
;;   so if repeating, add a tie to the final note to
;;   carry over to the start of the next repetition
(define/contract (rotate-notes-motif-forward-by-duration notes-motif-in durs)
  (-> notes-motif/c (listof duration?) (list/c notes-motif/c boolean?))
  (define (split-first-elem first-durval remaining-durval element)
    (let ([firsts (swap-voice-event-duration element (- first-durval remaining-durval) #f)]
          [lasts  (swap-voice-event-duration element remaining-durval #t)])
      (values firsts lasts))) 
  (define (rotate-notes-motif-forward-by-durval notes-motif-out remaining-durval split-note-flag)
    (let ([first-durval (voice-event->duration-int (car notes-motif-out))])
      (cond
        [(zero? remaining-durval)
         (list notes-motif-out split-note-flag)]
        [(equal? remaining-durval first-durval)
         (rotate-notes-motif-forward-by-durval (rotate-list-by notes-motif-out 1) 0 split-note-flag)]
        [(> first-durval remaining-durval)
         (let-values ([(first-elems last-elems) (split-first-elem first-durval remaining-durval (car notes-motif-out))])
           (rotate-notes-motif-forward-by-durval (append first-elems (cdr notes-motif-out) last-elems) 0 #t))]
        [else
         (rotate-notes-motif-forward-by-durval (rotate-list-by notes-motif-out 1) (- remaining-durval first-durval) split-note-flag)])))
  (rotate-notes-motif-forward-by-durval notes-motif-in (apply + (map duration->int durs)) #f))

(module+ test
  (check-equal? (rotate-notes-motif-forward-by-duration
                 `(,(Note 'C '0va 'Q '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f))
                 '(S))
                `((,(Note 'C '0va 'E. '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f)
                   ,(Note 'C '0va 'S '() #f)),
                  #t))
  (check-equal? (rotate-notes-motif-forward-by-duration
                 `(,(Note 'C '0va 'Q '() #t)
                   ,(Note 'C '0va 'Q '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f))
                 '(Q E))
                `((,(Note 'C '0va 'E '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f)
                   ,(Note 'C '0va 'Q '() #t)
                   ,(Note 'C '0va 'E '() #f))
                   #t))
  (check-equal? (rotate-notes-motif-forward-by-duration
                 `(,(Note 'C '0va 'Q '() #t)
                   ,(Note 'C '0va 'Q '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f))
                 '(E.))
                `((,(Note 'C '0va 'S '() #t)
                   ,(Note 'C '0va 'Q '() #f)
                   ,(Rest 'E)
                   ,(Note 'D '0va 'S '() #f)
                   ,(Note 'C '0va 'E. '() #f))
                   #t))
  )

;; Note:  at the motif level, an element can contain a list of durations, which render as a list of tied notes because the Note
;; and Chord types allow for a single duration only.
;; That means rotating post rendering may pull a tied Note or Chord from the beginning of the motif into a shorter, tied note at
;; the start and one or more tied notes at the end, with the implication that upon repetition, those should be tied across the
;; repetition to preserve the canon effect without repeating the Note or Chord.

;; This is *awfully* tweaky, with having to be careful about ties when rotating Note or Chord around start and stop.
;; Is it really necessary to cross domains and operate on the rendered motif?
;; Would it be easier to figure out how to rotate an intervals motif for a during instead?
;; Why did I abandon that approach?


;; Simplify:
;;   start back with morph-rotated-canons via make-list-morph-maybe-intervalls-motif,
;;   then make a new morpher to pass offset per voice as a pair with start pitch,
;;   with samples:  pitches no rests, pitches and rests, pitches chords and rests
;;
;; actually means modifying morph-rotated-canons to take an arbitrary list tied to morpher,
;; then making a new morpher that takes motif, start pitch and rotate count

;; generic morpher callback, i.e. not just (-> pitch/c notes-motif/c)
(define gen-morph-maybe-intervalss-motif/c (-> any/c notes-motif/c))

;; just map with contract
(define/contract (morph-to-motifs morph-data morpher)
  (-> (non-empty-listof any/c) gen-morph-maybe-intervalss-motif/c notes-motifs/c)
  (map morpher morph-data))

;; remembers maybe-intervals-motif, applies morph based on per-voice data, 
;; here, morph data includes (listof Scale? pitch/c exact-integer?)

(define (make-morph-maybe-intervalss-motif maybe-intervalss-motif)
  (lambda (morph-data)
    (match morph-data
      [(list scale start-pitch rotate-val)
       (match (render-maybe-intervalss-motifs scale start-pitch (rotate-list-by maybe-intervalss-motif rotate-val))
         [(list _ notes-motif)
          notes-motif])])))

(module+ test
  (define ex-mot (list (list 1 '() (list 'E)) (list 2 '() (list 'E)) (list 3 '() (list 'E))))
  (require rackunit)
  (let ([morpher (make-morph-maybe-intervalss-motif ex-mot)])
    (check-equal? (morph-to-motifs (list (list C-major (cons 'C '0va) 0) (list C-major (cons 'C '8vb) 1)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'F '0va 'E '() #f) (Note 'B '0va 'E '() #f))
                   (list (Note 'E '8vb 'E '() #f) (Note 'A '8vb 'E '() #f) (Note 'B '8vb 'E '() #f))))))
|#

;; And here I am, right back at the start because rotating the motif:
;; - is invariant for e.g. repeated intervals '(1 1 1), whereas if
;;   I render first, D E F becomes E F D, which is what I expected
;; - but for e.g. '(1 2 3) yields ('2 3 1), you get what you ask for,
;;   rotation *of intervals*, NOT rotation of rendered pitches, so
;;   from ascending intervals second, third, fourth you get ascending
;;   intervals third, fourth, second

;; So this diverges from my model.  Choose a middle ground: render first,
;; then have a special rotate that recognizes a tied note as a single entity

;; A tuplet is tied if the last tuplet-note/c is tied
(define/contract (tie? elem)
  (-> notes-motif-element/c boolean?)
  (match elem
    [(Note _ _ _ _ tie)
     tie]
    [(Chord _ _ _ tie)
     tie]
    [(Rest _)
     #f]
    [(Tuplet _ _ _ notes)
     (tie? (last notes))]))

;; copy elements from source to destination until first element that is not a tie
(define/contract (accum-tied-elements mot)
  (-> notes-motif/c notes-motif/c)
  (let ([elem (first mot)])
    (if (tie? elem)
        (cons elem (accum-tied-elements (rest mot)))
        (cons elem '()))))

;; - call accum-tied-elements from head and save result
;; - answer two-part list with result and original list
;;   with (length result) dropped from beginning
(define (split-notes-motif mot)
  (let ([first (accum-tied-elements mot)])
    (list first (drop mot (length first)))))

(define/contract (group-notes-motif-by-ties mot)
  (-> notes-motif/c notes-motifs/c)
  (define (inner m acc)
    (if (null? m)
        acc
        (match (split-notes-motif m)
          [(cons first-notes last-notes)
           (inner (car last-notes) (append acc (list first-notes)))])))
  (inner mot '()))

;; group the list of notes-motif elements by tied notes, e.g. a note
;; that is *not* tied becomes a single-item list and notes that *are*
;; tied become multiple-item lists, then call rotate-list-by, flatten
(define/contract (rotate-notes-motif-by mot cnt)
  (-> notes-motif/c exact-integer? notes-motif/c)
  (flatten (rotate-list-by (group-notes-motif-by-ties mot) cnt)))


(define gen-morph-maybe-intervalss-motif/c (-> any/c notes-motif/c))

;; just map with contract
(define/contract (morph-to-motifs morph-data morpher)
  (-> (non-empty-listof any/c) gen-morph-maybe-intervalss-motif/c notes-motifs/c)
  (map morpher morph-data))

;; morph data here is list of scale? pitch/c exact-integer?
;; minimal morph given always render first, morph second
;; 1) render with scale? and pitch/c for start pitch
;; 2) rotate with exact-integer?
(define (make-rotate-maybe-intervalss-motif maybe-intervalss-motif)
  (lambda (morph-data)
    (match morph-data
      [(list scale start-pitch rotate-val)
       (match (render-maybe-intervalss-motifs scale start-pitch maybe-intervalss-motif)
         [(list _ notes-motif)
          (rotate-notes-motif-by notes-motif rotate-val)])])))


;; Reconsider:

;; 1) morph has three points for transform:
;;    - mods to motif itself: repetition, inversion, rhythm, accent, etc.
;;    - rendering that can have unique scale and start pitch
;;    - mods to rendered result e.g. rotation
;;
;;    add callbacks to above for intervalss-motif -> intervalss-motif
;;    and notes-motif -> notes-motif, only constant is rendering

;; 2) zoom out: morphing of *individual motif* still seems too low level,
;;    breaking of note around rotate works for Bittersweet with long, 15
;;    and 22 pitch motifs, but point of motifs is many short bits that
;;    are easily audible
;;    rotate should be list-based for non-empty-listof maybe-intervalss-motif/c,
;;    not individual maybe-intervalss-motif/c, which makes input list of
;;    maybe-intervalss-motif, up one level, and rendering yields notes-motifs/c
;;    *maybe* with rotate of elements in notes-motif/c
;;
;;    maybe even a list of routines to generate motifs that can be parameterized
;;    note: motifs.rkt tries to generalize but maybe that's early optimization,
;;    and motifs-workspace.rkt is adjacent too
;;
;;    another strategy: write a couple motif routines:
;;
;;    - ascend/descend in progressively larger/smaller steps, uniform vs.
;;      longer/shorter durations with optional pattern to repeat at each
;;      iteration vs. just a single note
;;
;;    - same without varying transpose steps, just interval pattern and
;;      duration pattern to repeat up or down, from min to max
;;
;;    - repetition of rhythm for count maybe varying dynamic or duration,
;;      with varying octave or other interval mirroring in crescendo or
;;      decrescendo, soft/loud/soft, loud/soft/loud
;;
;; 3) then see where morph at level of list of motifs works or even if canon
;;    still makes sense or if there's some better way to manipulate aggregate
;;    motifs collectively?

;; note this comes close to what's already in motifs-workspace, except maybe
;; it really belongs in lib/motifs.rkt
;;
;; work through it here, migrate it later

;; eventually belongs in scale.rkt as just successive transposes of
;; a pattern over a list of pitches ... or maybe motifs.rkt?
;; 
;; - ascend/descend in progressively larger/smaller steps, uniform vs.
;;   longer/shorter durations with optional pattern to repeat at each
;;   iteration vs. just a single note
;; using scale, map over steps from start-pitch via transpose/absolute to
;; get list of start-pitches, 
;; map over start-pitches using scale and pattern via render-maybe-intervalss-motifs
;; to get (listof notes-motif/c)
;; for output, flatten (listof notes-motif/c)
;; - tbd: how to adjust durations?
(define/contract (transpose-by-start-pitches scale start-pitch steps maybe-intervalss-motif)
  (-> Scale? pitch/c intervals/c maybe-intervalss-motifs/c notes-motif/c)
  (let ([start-pitches (cons start-pitch (transpose/successive scale (scale->pitch-range-pair scale) start-pitch steps))])
    (flatten (for/list ([inner-start-pitch start-pitches])
               (second (render-maybe-intervalss-motifs scale inner-start-pitch maybe-intervalss-motif))))))

(define/contract (add-ctrl-to-notes-motif-element elem ctrl)
  (-> notes-motif-element/c control/c notes-motif-element/c)
  (match elem
    [(Note pc oct dur ctrls tie)
     (Note pc oct dur (cons ctrl ctrls) tie)]
    [(Rest dur)
     (Rest dur)]
    [(Chord pitches dur ctrls tie)
     (Chord pitches dur (cons ctrl ctrls) tie)]
    [(Tuplet num denom dur notes)
     (Tuplet num denom dur (cons (add-ctrl-to-notes-motif-element (car notes) ctrl) (cdr notes)))]))

(define (sustain-all notes-motif)
  (mod-all notes-motif 'SustainOn 'SustainOff))

;; e.g. on/off controls:  Sustain, Slur, Sostenuto
(define/contract (mod-all notes-motif on off)
  (-> notes-motif/c symbol? symbol? notes-motif/c)
  (let*-values ([(start  rest) (split-at notes-motif 1)]
                [(middle last) (split-at rest (- (length rest) 1))])
    (append (cons (add-ctrl-to-notes-motif-element (car start) on) middle)
            (list (add-ctrl-to-notes-motif-element (car last) off)))))

;; a motif with three-note ascending pattern over five octave arc stated five times:
;; - as is
;; - with overlapping segments
;; - with skipped steps repeated three times
(parameterize ((tempo/param (TempoDur 'Q 100)))
  (let* ([start-pitch (cons 'C '15vb)]
         [pattern     '((0 (Accent) (S)) (2 () (T)) (2 () (T)) (-1 () (T)) (1 () (T)) ((1 2 1) () (S)))]
         [intervals   '(2 2 2 3 3 4 4 5 1 1 1 -1 -1 -1 -5 -4 -4 -3 -3 -2 -4)]
         [base        (transpose-by-start-pitches C-whole-tone start-pitch intervals pattern)]
         [overlapped  (repeat-segments 3 9 base)]
         [skipped     (repeat-segments 9 3 base)]
         [all-motifs  (sustain-all (append base overlapped skipped skipped skipped))]
         [voice       (SplitStaffVoice 'AcousticGrand 'PanCenter all-motifs)])
    (gen-score-file (score/parameterized (list voice)))))

;; note that pattern embeds rhythm and accents, so if I wanted e.g. a rhythm with
;; a different length than the count of pitches, I'd have to rewrite the motif
;; the question is does match/mismatch of counts of rhythmic motif vs. pitch motif
;; have to be part of generative process or can it be a post-processing step?
;; the point is to have them out of sync
;; so instead of a constant pattern, I need something that generates a pattern
;; one element at a time or that takes arguments reflecting different periods
;; like lists of intervals and durations + accents
;; then the question is don't I want to pick up the next transposition from where
;; I left off instead of repeating the same pattern?
;; 
;; so that takes the motif generation into transpose-by-start-pitches so that
;; instead of a fixed pattern I provide a generator and the implementation
;; changes from a simple map, but note I need to know some sort of chunking
;; so maybe it's a matter of generating a motif with a list of elements per
;; generation and then the next time the generator is called it cycles the
;; components as it sees fit, with the reference example being to use one
;; list to govern length and another to cycle within or across that length
;; in that contact, repeat-segments could be another way to modify input to
;; transpose-by-start-pitches, though it gives you an output list with the
;; repeats concatenated


;; to move progressively small -> large/large -> small create interval generator
;; and call gen-up/down-notes-motif (rename to transpose-by-start-pitches?)

;; and another variant to modify durations over period of transpose


;; next: workspace framework

#|
;; next, this needs to call back to parameters for config

(define maybe-intervalss-motif/param (make-parameter (list (list 1 '() (list 'E)) (list 1 '() (list 'E)) (list 1 '() (list 'E)))))

(define start-pitches/param (make-parameter (list (cons 'C '0va) (cons 'C '8vb) (cons 'C '15va))))

(define/contract (render-canon-voices)
  (-> (non-empty-listof voice/c))
  (let* ([morpher       (make-list-morph-maybe-intervalss-motif (scale/param) (maybe-intervalss-motif/param))]
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
|#


(module+ test
  (require rackunit)
  (check-equal? (split-notes-motif `(,(Note 'C '8va 'E '() #t)
                                     ,(Note 'C '8va 'E '() #t)
                                     ,(Note 'C '8va 'E '() #f)
                                     ,(Rest 'E) ,(Rest 'E)))
                (list (list (Note 'C '8va 'E '() #t)
                            (Note 'C '8va 'E '() #t)
                            (Note 'C '8va 'E '() #f))
                      (list (Rest 'E) (Rest 'E))))
  (check-equal? (first (split-notes-motif `(,(Rest 'E)
                                            ,(Note 'C '8va 'E '() #t)
                                            ,(Note 'C '8va 'E '() #t)
                                            ,(Note 'C '8va 'E '() #f)
                                            ,(Rest 'E))))
                (list (Rest 'E)))
  (check-equal? (second (split-notes-motif `(,(Rest 'E)
                                             ,(Note 'C '8va 'E '() #t)
                                             ,(Note 'C '8va 'E '() #t)
                                             ,(Note 'C '8va 'E '() #f)
                                             ,(Rest 'E))))
                (list (Note 'C '8va 'E '() #t)
                      (Note 'C '8va 'E '() #t)
                      (Note 'C '8va 'E '() #f)
                      (Rest 'E)))
  (let ([mot `(,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Rest 'E))])
    (check-equal? (rotate-notes-motif-by mot 0)
                  mot)
    (check-equal? (rotate-notes-motif-by mot 1)
                  `(,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)
                    ,(Rest 'E)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)))
    (check-equal? (rotate-notes-motif-by mot 2)
                  `(,(Rest 'E)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)))
    (check-equal? (rotate-notes-motif-by mot 3) mot))
  (let ([mot `(,(Tuplet 3 2 'Q (list (Note 'C '8va 'E '() #f) (Note 'D '8va 'E '() #f) (Note 'E '8va 'E '() #f)))
               ,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Rest 'E))])
    (check-equal? (rotate-notes-motif-by mot 1)
                  `(,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)
                    ,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)
                    ,(Rest 'E)
                    ,(Tuplet 3 2 'Q (list (Note 'C '8va 'E '() #f) (Note 'D '8va 'E '() #f) (Note 'E '8va 'E '() #f))))))
  (let ([mot `(,(Tuplet 3 2 'Q (list (Note 'C '8va 'E '() #f) (Note 'D '8va 'E '() #f) (Note 'E '8va 'E '() #t)))
               ,(Note 'E '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Note 'C '8va 'E '() #t)
               ,(Note 'C '8va 'E '() #f)
               ,(Rest 'E))])
    (check-equal? (rotate-notes-motif-by mot 1)
                  `(,(Note 'C '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f)
                    ,(Rest 'E)
                    ,(Tuplet 3 2 'Q (list (Note 'C '8va 'E '() #f) (Note 'D '8va 'E '() #f) (Note 'E '8va 'E '() #t)))
                    ,(Note 'E '8va 'E '() #t)
                    ,(Note 'C '8va 'E '() #f))))
  )

(module+ test
  (define ex-mot (list (list 1 '() (list 'E)) (list 2 '() (list 'E)) (list 3 '() (list 'E))))
  (require rackunit)
  (let ([morpher (make-rotate-maybe-intervalss-motif ex-mot)])
    (check-equal? (morph-to-motifs (list (list C-major (cons 'C '0va) 0) (list C-major (cons 'C '8vb) 1)) morpher)
                  (list
                   (list (Note 'D '0va 'E '() #f) (Note 'F '0va 'E '() #f) (Note 'B '0va 'E '() #f))
                   (list (Note 'F '8vb 'E '() #f) (Note 'B '8vb 'E '() #f) (Note 'D '8vb 'E '() #f))))))

