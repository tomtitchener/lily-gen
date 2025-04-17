#lang racket

;; score-utils: utilities that depend only on score

(provide
 (contract-out

  ;; map voice-event->duration-int across all (listof voice-event/c)
  ;; per voice, answer max of (listof voice-event/c e.g. for Keyboard
  [voice->total-durs (-> voice/c (listof natural-number/c))]
  
  ;; multiply num with (duration->int denom) for simple
  ;; time signature
  ;; for grouping and compound, sum of same for all embedded
  ;; simple times signatures
  [time-signature->barlen (-> time-signature/c natural-number/c)]
  
  ;; multiply num  and duration->int for first and last elements of pair
  [num-denom-pr->barlen (-> num-denom/c natural-number/c)]

  [ctrls-durs&pit->notes (-> (listof control/c) (non-empty-listof duration?) pitch/c boolean? (listof Note?))]
  
  [ctrls-durs&pits->chords (-> (listof control/c) (non-empty-listof duration?) (non-empty-listof pitch/c) boolean? (listof Chord?))]

  [ctrls-durs&mpit->notes-or-rests (-> (listof control/c) (listof duration?) maybe-pitch/c (or/c (listof Note?) (listof Rest?)))]

  [add-bass-or-treble-clefs-to-voice-events (-> (listof voice-event/c) clef? (listof voice-event/c))]))

;; - - - - - - - - -
;; implementation
(require lily-gen/lib/score)

(require (only-in lily-gen/lib/scale compare-pitches pitch->chromatic-index))

;; answer list of total durs because KeyboardVoice has treble and bass voices
;; (-> voice/c (listof natural-number/c))
(define (voice->total-durs voice)
  (match voice
    [(PitchedVoice _ _ voice-events)
     (list (apply + (map voice-event->duration-int voice-events)))]
    [(KeyboardVoice _ _ voice-events-pr)
     (list (apply + (map voice-event->duration-int (car voice-events-pr)))
           (apply + (map voice-event->duration-int (cdr voice-events-pr))))]
    [(SplitStaffVoice _ _ voice-events)
     (list (apply + (map voice-event->duration-int voice-events)))]))

;; (-> time-signature/c natural-number/c)
(define (time-signature->barlen time-sig)
  (match time-sig
    [(TimeSignatureSimple num denom)
     (* num (duration->int denom))]
    [(TimeSignatureGrouping _ num denom)
     (* num (duration->int denom))]
    [(TimeSignatureCompound groups)
     (apply + (map (lambda (grp)
                     (let ([nums  (take grp (- (length grp) 1))]
                           [denom (duration->int (last grp))])
                       (apply + (map ((curry *) denom) nums))))
                   groups))]))

;; (-> num-denom/c natural-number/c)
(define (num-denom-pr->barlen num-denom-pr)
  (* (car num-denom-pr) (duration->int (cdr num-denom-pr))))

;; create tied notes one for each duration, all except last are tied
;; (-> (listof control/c) (non-empty-listof duration?) pitch/c boolean? (listof Note?))
(define (ctrls-durs&pit->notes controls durations pitch tie)
  (let ([pitch-class (car pitch)]
        [octave      (cdr pitch)])
    (let loop ([ctrls controls]
               [durs  durations])
      (cond [(= 1 (length durs))
             (list (Note pitch-class octave (car durs) ctrls tie))]
            [else
             (cons (Note pitch-class octave (car durs) ctrls #t)
                   (loop '() (cdr durs)))]))))

(module+ test
  (require rackunit)
  (check-equal?
   (ctrls-durs&pit->notes '(Accent) '(E) (cons 'C '0va) #f)
   (list (Note 'C '0va 'E '(Accent) #f)))
  (check-equal?
   (ctrls-durs&pit->notes '(Accent) '(E Q) (cons 'C '0va) #f)
   (list (Note 'C '0va 'E '(Accent) #t) (Note 'C '0va 'Q '() #f)))
  (check-equal?
   (ctrls-durs&pit->notes '(Accent) '(E Q W) (cons 'C '0va) #f)
   (list (Note 'C '0va 'E '(Accent) #t) (Note 'C '0va 'Q '() #t) (Note 'C '0va 'W '() #f))))

(define (ctrls-durs&mpit->notes-or-rests ctrls durs mpitch)
  (match mpitch
    [#f         (map Rest durs)]
    [(cons _ _) (ctrls-durs&pit->notes ctrls durs mpitch)]))

;; create chords one for each duration, all except last are tied
;; (-> (listof control/c) (non-empty-listof duration?) (listof pitch/c) (listof Chord?))
(define (ctrls-durs&pits->chords controls durations pitches tie)
  (let loop ([ctrls controls]
             [durs durations])
    (cond [(= 1 (length durs))
           (list (Chord pitches (car durs) controls tie))]
          [else
           (cons (Chord pitches (car durs) controls #t)
                 (loop '() (cdr durs)))])))

;; (-> (listof voice-event/c) clef? (listof voice-event/c))
(define (add-bass-or-treble-clefs-to-voice-events voice-events starting-clef)
  (define max-count-window-pitches 5)
  (define (max-note-for-clef clef) (if (symbol=? 'Bass clef) (cons 'F '0va) (cons 'G '8vb)))
  (define (pitch-within-clef? clef pitch)
    (let ([max-or-min-note (max-note-for-clef clef)]
          [cmp-op (if (symbol=? 'Bass clef) <= >=)])
      (compare-pitches cmp-op pitch max-or-min-note)))
  (define (voice-events->count-pitches voice-events)
    (define (voice-event->count-pitches voice-event)
      (if (or (Note? voice-event) (Chord? voice-event))
          1
          (if (Tuplet? voice-event)
              (length (Tuplet-notes voice-event))
              0)))
    (apply + (map voice-event->count-pitches voice-events)))
  (define (fold-fun voice-event fold-state)
    (let ([current-clef   (first  fold-state)]
          [current-window (second fold-state)]
          [current-return (third  fold-state)])
      (define (fold-fun-inner pitch-class octave)
        (if (pitch-within-clef? current-clef (cons pitch-class octave))
            (let ([new-window '()]
                  [new-return (append current-return current-window (list voice-event))])
              (list current-clef new-window new-return))
            (if (< (add1 (voice-events->count-pitches current-window)) max-count-window-pitches)
                (let ([new-window (append current-window (list voice-event))])
                  (list current-clef new-window current-return))
                (let* ([new-clef   (if (symbol=? current-clef 'Bass) 'Treble 'Bass)]
                       [new-window '()]
                       [new-return (append current-return current-window (list new-clef) (list voice-event))])
                  (list new-clef new-window new-return)))))
      (match voice-event
        [(Note pitch-class octave _ _ _)
         (fold-fun-inner pitch-class octave)]
        [(Chord pitches _ _ _)
         (let ([pitch (if (symbol=? current-clef 'Bass) (last pitches) (first pitches))])
           (fold-fun-inner (car pitch) (cdr pitch)))]
        ;; not sure what to do here, find lowest or highest pitch the way I do with chord?
        [(Tuplet _ _ _ notes)
         (define (tuplet-note->pitches note)
           (match note
             [(Note pitch-class octave _ _ _)
              (list (cons pitch-class octave))]
             [(Chord pitches _ _ _)
              pitches]
             [_
              '()]))
         (let* ([pitches (append (tuplet-note->pitches notes))]
                [sorted-pitches (sort pitches < #:key pitch->chromatic-index)]
                [pitch (if (symbol=? current-clef 'Bass) (last sorted-pitches) (first sorted-pitches))])
           (fold-fun-inner (car pitch) (cdr pitch)))]
        [_
         (let ([new-window (append current-window (list voice-event))])
           (list current-clef new-window current-return))])))
  (let* ([initial-state (list starting-clef '() '())]
         [final-state (foldl fold-fun initial-state voice-events)]
         [final-window (second final-state)]
         [final-return (third final-state)])
    (append final-return final-window)))
