#lang racket

;; Score and contained structs, contracts for pitch and sums, predicates and syms from score-syms.rkt
;; goal is to capture all data and data predicates contained in score

(provide
 ;; - - - - - - - - -
 ;; structs
 (struct-out Score)
 (struct-out PitchedVoice)
 (struct-out KeyboardVoice)
 (struct-out SplitStaffVoice)
 (struct-out VoicesGroup)
 (struct-out Note)
 (struct-out Rest)
 (struct-out Tuplet)
 (struct-out Chord)
 (struct-out TempoDur)
 (struct-out TempoLong)
 (struct-out TempoRange)
 (struct-out KeySignature)
 (struct-out TimeSignatureSimple)
 (struct-out TimeSignatureGrouping)
 (struct-out TimeSignatureCompound)
 ;; - - - - - - - - -
 ;; contracts
 pitch/c
 maybe-pitch/c
 control/c
 num-denom/c
 tempo/c
 time-signature/c
 voice-event/c
 voice/c
 ;; - - - - - - - - -
 ;; symbols and predicates
 (all-from-out "score-syms.rkt"))

;; - - - - - - - - -
;; implementation
(require "score-syms.rkt")

(define pitch/c
  (make-flat-contract #:name 'pitch/c #:first-order (cons/c pitch-class? octave?)))

(define maybe-pitch/c
  (make-flat-contract #:name 'maybe-pitch/c #:first-order (or/c pitch/c false/c)))

(define control/c
  (make-flat-contract #:name 'control/c #:first-order (or/c accent? dynamic? swell? sustain? sostenuto? slur? string?)))

(define num-denom/c
  (make-flat-contract #:name 'num-denom/c #:first-order (cons/c exact-positive-integer? duration?)))

(struct/contract Note ([pitch    pitch-class?]
                       [octave   octave?]
                       [dur      duration?]
                       [controls (listof control/c)]
                       [tie      boolean?])
                 #:transparent)

(struct/contract Rest ([dur duration?]) #:transparent)

(struct/contract Chord ([pitches  (listof pitch/c)]
                        [dur      duration?]
                        [controls (listof control/c)]
                        [tie      boolean?])
                 #:transparent)

(define tuplet-note/c
  (make-flat-contract #:name 'tuplet-note/c #:first-order (or/c Note? Rest? Chord?)))

(struct/contract Tuplet ([num   exact-positive-integer?]
                         [denom exact-positive-integer?]
                         [dur   duration?]
                         [notes (listof tuplet-note/c)])
                 #:transparent)

(struct/contract TempoDur ([dur   duration?]
                           [perMin exact-positive-integer?])
                 #:transparent)

(struct/contract TempoLong ([text   string?]
                            [dur    duration?]
                            [perMin exact-positive-integer?])
                 #:transparent)

(define/contract (tempo-range-ctor-guard dur perMinLo perMinHi type-name)
  (-> duration? exact-positive-integer? exact-positive-integer? symbol? (values duration? exact-positive-integer? exact-positive-integer?))
  (if (>= perMinLo perMinHi)
      (error type-name "for vals ~e ~e ~e, perMinLo ~e is not less than perMinHi ~e" dur perMinLo perMinHi perMinLo perMinHi)
      (values dur perMinLo perMinHi)))

(struct TempoRange (dur perMinLo perMinHi) #:guard tempo-range-ctor-guard #:transparent)

(define tempo/c
  (make-flat-contract #:name 'tempo/c #:first-order (or/c string? TempoDur? TempoLong? TempoRange?)))

(struct/contract KeySignature ([pitch pitch-class?]
                               [mode  mode?])
                 #:transparent)

(struct/contract TimeSignatureSimple ([num   exact-positive-integer?]
                                      [denom duration?])
                 #:transparent)

(define/contract (time-signature-grouping-ctor-guard groups num denom type-name)
  (-> (listof exact-positive-integer?) exact-positive-integer? duration? symbol? (values (listof exact-positive-integer?) exact-positive-integer? duration?))
  (if (not (eq? num (apply + groups)))
      (error type-name "sum of values in groups ~v does not equal to numerator in time signature ~v" groups num)
      (values groups num denom)))
                  
(struct TimeSignatureGrouping (groups num denom) #:guard time-signature-grouping-ctor-guard  #:transparent)

(struct/contract TimeSignatureCompound ([groups (listof (*list/c exact-positive-integer? duration?))])
                 #:transparent)

(define time-signature/c
  (make-flat-contract #:name 'time-signature/c #:first-order (or/c TimeSignatureSimple? TimeSignatureGrouping? TimeSignatureCompound?)))

(define voice-event/c
  (make-flat-contract #:name 'voice-event/c #:first-order (or/c Note? Rest? Tuplet? Chord? clef? KeySignature?)))

(struct/contract PitchedVoice ([instr       instr?]
                               [voiceevents (listof voice-event/c)])
                 #:transparent)

(define voice-events-pair/c
  (make-flat-contract #:name 'voice-events-pair/c #:first-order (cons/c (listof voice-event/c) (listof voice-event/c))))

(struct/contract KeyboardVoice ([instr           instr?]
                                [voiceeventspair voice-events-pair/c])
                 #:transparent)

(struct/contract SplitStaffVoice ([instr       instr?]
                                  [voiceevents (listof voice-event/c)])
                 #:transparent)
                 
(define voice/c
  (make-flat-contract #:name 'voice/c #:first-order (or/c PitchedVoice? KeyboardVoice? SplitStaffVoice?)))

(struct/contract VoicesGroup ([tempo          tempo/c]
                              [time-signature time-signature/c]
                              [voices         (listof voice/c)])
                 #:transparent)

(struct/contract Score ([title        string?]
                        [seed         string?]
                        [voice-groups (listof VoicesGroup?)]))
