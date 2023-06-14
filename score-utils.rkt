#lang racket

(provide voice-event->duration-int)

(require (only-in "score.rkt" Note Rest Chord Tuplet KeySignature clef? voice-event/c))
(require (only-in "lily-utils.rkt" duration->int))

(define/contract (voice-event->duration-int voice-event)
  (-> voice-event/c exact-nonnegative-integer?)
  (match voice-event
    [(Note _ _ dur _ _) (duration->int dur)]
    [(Rest dur)         (duration->int dur)]
    [(Chord _ dur _ _)  (duration->int dur)]
    [(Tuplet _ _ dur _) (duration->int dur)]
    [(KeySignature _ _) 0]
    [(? clef?)          0]))
