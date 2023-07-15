#lang racket

;; score-utils: utilities that depend only on score and lily-utils

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

  [ctrls-durs&pit->notes (-> (listof control/c) (listof duration?) pitch/c (listof Note?))]
  
  [ctrls-durs&pits->chords (-> (listof control/c) (listof duration?) pitch/c (listof Chord?))]

  [add-bass-or-treble-clefs-to-voice-events (-> (listof voice-event/c) clef? (listof voice-event/c))]))

;; - - - - - - - - -
;; implementation
(require "score.rkt")

(require (only-in "scale.rkt" pitch->chromatic-index))

(require (only-in "lily-utils.rkt" duration->int))

;; answer list of total durs because KeyboardVoice has treble and bass voices
;; (-> voice/c (listof natural-number/c))
(define (voice->total-durs voice)
  (match voice
    [(PitchedVoice _ voice-events)
     (list (apply + (map voice-event->duration-int voice-events)))]
    [(KeyboardVoice _ voice-events-pr)
     (list (apply + (map voice-event->duration-int (car voice-events-pr)))
           (apply + (map voice-event->duration-int (cdr voice-events-pr))))]
    [(SplitStaffVoice _ voice-events)
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

;; (-> (listof control/c) (listof duration?) pitch/c (listof Note?))
(define (ctrls-durs&pit->notes ctrls durations pitch)
  (cond [(null? durations) '()]
        [(= 1 (length durations))
         (cons (Note (car pitch) (cdr pitch) (car durations) ctrls #f)
               (ctrls-durs&pit->notes ctrls (cdr durations) pitch))]
        [else
         (cons (Note (car pitch) (cdr pitch) (car durations) ctrls #t)
               (ctrls-durs&pit->notes ctrls (cdr durations) pitch))]))

;; (-> (listof control/c) (listof duration?) (listof pitch/c) (listof Chord?))
(define (ctrls-durs&pits->chords ctrls durations pitches)
  (cond [(null? durations) '()]
        [(= 1 (length durations))
         (cons (Chord pitches (car durations) ctrls #f)
               (ctrls-durs&pits->chords ctrls (cdr durations) pitches))]
        [else
         (cons (Chord pitches (car durations) ctrls #t)
               (ctrls-durs&pits->chords ctrls (cdr durations) pitches))]))

;; (-> (listof voice-event/c) clef? (listof voice-event/c))
(define (add-bass-or-treble-clefs-to-voice-events voice-events starting-clef)
  (define max-count-window-pitches 5)
  (define (max-note-for-clef clef) (if (symbol=? 'Bass clef) (cons 'F '0va) (cons 'G '8vb)))
  (define (compare-pitches cmp pitch1 pitch2) (cmp (pitch->chromatic-index pitch1) (pitch->chromatic-index pitch2)))
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
                       [new-return (append (list new-clef) current-return current-window (list voice-event))])
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
