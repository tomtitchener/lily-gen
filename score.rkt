#lang racket

;; score: capture all data and data predicates contained in Score,
;; forward all from score-sym.rkt

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
 (struct-out Spacer)
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

(require (only-in "scale.rkt" pitch->chromatic-idx))

(define control/c
  (make-flat-contract #:name 'control/c #:first-order (or/c accent? dynamic? swell? sustain? sostenuto? slur? string?)))

(define num-denom/c
  (make-flat-contract #:name 'num-denom/c #:first-order (cons/c natural-number/c duration?)))

(struct/contract Note ([pitch    pitch-class?]
                       [octave   octave?]
                       [dur      duration?]
                       [controls (listof control/c)]
                       [tie      boolean?])
                 #:transparent)

(struct/contract Rest ([dur duration?]) #:transparent)

(struct/contract Spacer ([dur duration?]) #:transparent)

(define/contract (chord-ctor-guard pitches dur controls tie type-name)
  (-> (listof pitch/c) duration? (listof control/c) boolean? symbol? (values (listof pitch/c) duration? (listof control/c) boolean?))
  (let ([sorted-pitches (sort pitches < #:key pitch->chromatic-idx)])
    (values sorted-pitches dur controls tie)))

(struct Chord (pitches dur controls tie) #:guard chord-ctor-guard #:transparent)

(define tuplet-note/c
  (make-flat-contract #:name 'tuplet-note/c #:first-order (or/c Note? Rest? Chord?)))

(struct/contract Tuplet ([num   natural-number/c]
                         [denom natural-number/c]
                         [dur   duration?]
                         [notes (listof tuplet-note/c)])
                 #:transparent)

(struct/contract TempoDur ([dur   duration?]
                           [perMin natural-number/c])
                 #:transparent)

(struct/contract TempoLong ([text   string?]
                            [dur    duration?]
                            [perMin natural-number/c])
                 #:transparent)

(define/contract (tempo-range-ctor-guard dur perMinLo perMinHi type-name)
  (-> duration? natural-number/c natural-number/c symbol? (values duration? natural-number/c natural-number/c))
  (if (>= perMinLo perMinHi)
      (error type-name "for vals ~e ~e ~e, perMinLo ~e is not less than perMinHi ~e" dur perMinLo perMinHi perMinLo perMinHi)
      (values dur perMinLo perMinHi)))

(struct TempoRange (dur perMinLo perMinHi) #:guard tempo-range-ctor-guard #:transparent)

(define tempo/c
  (make-flat-contract #:name 'tempo/c #:first-order (or/c string? TempoDur? TempoLong? TempoRange?)))

(struct/contract KeySignature ([pitch pitch-class?]
                               [mode  mode?])
                 #:transparent)

(struct/contract TimeSignatureSimple ([num   natural-number/c]
                                      [denom duration?])
                 #:transparent)

(define/contract (time-signature-grouping-ctor-guard groups num denom type-name)
  (-> (listof natural-number/c) natural-number/c duration? symbol? (values (listof natural-number/c) natural-number/c duration?))
  (if (not (eq? num (apply + groups)))
      (error type-name "sum of values in groups ~v does not equal to numerator in time signature ~v" groups num)
      (values groups num denom)))
                  
(struct TimeSignatureGrouping (groups num denom) #:guard time-signature-grouping-ctor-guard  #:transparent)

(struct/contract TimeSignatureCompound ([groups (listof (*list/c natural-number/c duration?))])
                 #:transparent)

(define time-signature/c
  (make-flat-contract #:name 'time-signature/c #:first-order (or/c TimeSignatureSimple? TimeSignatureGrouping? TimeSignatureCompound?)))

(define voice-event/c
  (make-flat-contract #:name 'voice-event/c #:first-order (or/c Note? Rest? Spacer? Tuplet? Chord? clef? KeySignature?)))

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

;; TBD: add PercussionVoice.  Note one instrument e.g. timbales might have two "notes" e.g. timh and timl
;; or for melodic tom-tom: tomfl tomfh toml tomh tomml tommh, and etc.
;; see https://lilypond.org/doc/v2.24/Documentation/notation/percussion-notes
;; and e.g. all tom-toms can all be rendered on a percussion-style or maybe just toml and tomh on a timbales-style staff
;; - need percussion pitch class analogous to a tuned voice pitch class though they're unique to the instrument
;; - otherwise notation for a percussion note follows lilypond convention e.g. toml4 tomh8->
;; see https://lilypond.org/doc/v2.24/Documentation/snippets/percussion

(struct/contract VoicesGroup ([tempo          tempo/c]
                              [time-signature time-signature/c]
                              [voices         (listof voice/c)])
                 #:transparent)

(define/contract (score-ctor-guard title seed voices-groups type-name)
  (-> string? string? (listof VoicesGroup?) symbol? (values string? string? (listof VoicesGroup?)))
  (if (= 1 (length voices-groups))
      (values title seed voices-groups)
      (let ([voices-group-lengths (map (lambda (voices-group) (length (VoicesGroup-voices voices-group))) voices-groups)])
        (when (not (= voices-group-lengths))
          (error 'score-ctor-guard "unequal counts of voices in VoicesGroups: ~v" voices-group-lengths))
        (values title seed voices-groups))))

(struct Score (title seed voices-groups) #:guard score-ctor-guard #:transparent)
