#lang racket

;; score-utils: utilities that depend only on score and lily-utils

(provide
 (contract-out
  ;; extract duration from voice-event, 0 if none
  [voice-event->duration-int (-> voice-event/c natural-number/c)]

  ;; map voice-event->duration-int across all (listof voice-event/c)
  ;; per voice, answer max of (listof voice-event/c e.g. for Keyboard
  [voice->total-durs (-> voice/c (listof natural-number/c))]
  
  ;; multiply num with (duration->int denom) for simple
  ;; time signature
  ;; for grouping and compound, sum of same for all embedded
  ;; simple times signatures
  [time-signature->barlen (-> time-signature/c natural-number/c)]
  
  ;; multiply num  and duration->int for first and last elements of pair
  [num-denom-pr->barlen (-> num-denom/c natural-number/c)]))

;; - - - - - - - - -
;; implementation
(require "score.rkt")

(require (only-in "lily-utils.rkt" duration->int))

;; (-> voice-event/c natural-number/c)
(define (voice-event->duration-int voice-event)
  (match voice-event
    [(Note _ _ dur _ _) (duration->int dur)]
    [(Rest dur)         (duration->int dur)]
    [(Spacer dur)       (duration->int dur)]
    [(Chord _ dur _ _)  (duration->int dur)]
    [(Tuplet _ _ dur _) (duration->int dur)]
    [(KeySignature _ _) 0]
    [(? clef?)          0]))

;; answser list of total durs because KeyboardVoice has treble and bass voices
;; (-> voice/c (listof natural-number/c))
(define (voice->total-durs voice)
  (match voice
    [(PitchedVoice _ voice-events)
     (list (apply + (map voice-event->duration-int voice-events)))]
    [(KeyboardVoice _ voice-events-pr)
     (list (apply + (map voice-event->duration-int car voice-events-pr))
           (apply + (map voice-event->duration-int cdr voice-events-pr)))]
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
