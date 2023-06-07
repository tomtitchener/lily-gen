#lang at-exp racket

(require "score.rkt")
(require "score-syms.rkt")
(require "lily-utils.rkt")
(require "rhythm-utils.rkt")
         
(provide score->lily)

(define/contract (control->lily control)
  (-> control/c string?)
  (cond [(accent?     control) (accent->lily    control)]
        [(dynamic?    control) (dynamic->lily   control)]
        [(swell?      control) (swell->lily     control)]
        [(sustain?    control) (sustain->lily   control)]
        [(sostenuto?  control) (sostenuto->lily control)]
        [(slur?       control) (slur->lily      control)]
        [(string?     control) (ann->lily       control)]
        [else (error 'control->lily "unexpected control ~e" control)]))

(define/contract (note->lily note)
  (-> Note? string?)
  (string-append
   (pitch->lily (cons (Note-pitch note) (Note-octave note)))
   (duration->lily (Note-dur note))
   (apply string-append (map control->lily (Note-controls note)))
   (if (Note-tie note) "~" "")))

(define/contract (rest->lily rest)
  (-> Rest? string?)
  (let ([duration (Rest-dur rest)])
    (string-append "r" (duration->lily duration))))

(define/contract (tuplet->lily tuplet)
  (-> Tuplet? string?)
  (let ([num   (Tuplet-num   tuplet)]
        [denom (Tuplet-denom tuplet)]
        [dur   (duration->lily (Tuplet-dur tuplet))]
        [notes (string-join (map voice-event->lily (Tuplet-notes tuplet)) " ")])
    (format "\\tuplet ~a/~a ~a { ~a }" num denom dur notes)))

(define/contract (pitch->lily pitch)
  (-> pitch/c string?)
  (let ([pit (car pitch)]
        [oct (cdr pitch)])
    (string-append (pitch-class->lily pit) (octave->lily oct))))

(define/contract (chord->lily chord)
  (-> Chord? string?)
  (let ([pitches  (Chord-pitches  chord)]
        [dur      (Chord-dur      chord)]
        [controls (Chord-controls chord)]
        [tie?     (Chord-tie      chord)])
    (format
     "< ~a >~a~a~a"
     (string-join (map pitch->lily pitches) " ")
     (duration->lily dur)
     (apply string-append (map control->lily controls))
     (if tie? "~" ""))))

;; NB: using \\markup { ~a } for the text parts of a TempoText
;; and TempoLong means you can specify e.g. "\\italic Adagio" 
;; as well as unmarked-up "Adagio"

(define/contract (tempotext->lily tempo)
  (-> string? string?)
  (format "\\tempo \\markup { ~a }" tempo))

(define/contract (tempodur->lily tempo)
  (-> TempoDur? string?)
  (let ([dur    (TempoDur-dur tempo)]
        [perMin (TempoDur-perMin tempo)])
    (format "\\tempo ~a = ~a" (duration->lily dur) perMin)))

(define/contract (tempolong->lily tempo)
  (-> TempoLong? string?)
  (let ([text   (TempoLong-text   tempo)]
        [dur    (TempoLong-dur    tempo)]
        [perMin (TempoLong-perMin tempo)])
    (format "\\tempo \\markup { ~a } ~a = ~a" text (duration->lily dur) perMin)))

(define/contract (temporange->lily tempo)
  (-> TempoRange? string?)
  (let ([dur (TempoRange-dur      tempo)]
        [lo  (TempoRange-perMinLo tempo)]
        [hi  (TempoRange-perMinHi tempo)])
    (format "\\tempo ~a = ~a - ~a" (duration->lily dur) lo hi)))

(define/contract (tempo->lily tempo)
  (-> tempo/c string?)
  (cond [(string?     tempo) (tempotext->lily  tempo)]
        [(TempoDur?   tempo) (tempodur->lily   tempo)]
        [(TempoLong?  tempo) (tempolong->lily  tempo)]
        [(TempoRange? tempo) (temporange->lily tempo)]
        [else (error "tempo->lily unexpected tempo")]))

(define/contract (keysignature->lily keysig)
  (-> KeySignature? string?)
  (let ([pitch (KeySignature-pitch keysig)]
        [mode  (KeySignature-mode  keysig)])
    (format "\\key ~a ~a" (pitch-class->lily pitch) (mode->lily mode))))

(define/contract (time-signature-simple->lily time-signature)
  (-> TimeSignatureSimple? string?)
  (let ([num   (TimeSignatureSimple-num   time-signature)]
        [denom (TimeSignatureSimple-denom time-signature)])
    (format "\\time  ~s/~a" num (duration->lily denom))))

(define/contract (time-signature-grouping->lily time-signature)
  (-> TimeSignatureGrouping? string?)
  (let ([groups (TimeSignatureGrouping-groups time-signature)]
        [num    (TimeSignatureGrouping-num     time-signature)]
        [denom  (TimeSignatureGrouping-denom   time-signature)])
    (format "\\time ~a ~a/~a" (string-join (map number->string groups) ",") num (duration->lily denom))))

(define num-denom/c
  (make-flat-contract #:name 'num-denom/c #:first-order (cons/c exact-positive-integer? duration?)))

(define/contract (numdenompr->lily pr)
  (-> num-denom/c string?)
  (let ([num (car pr)]
        [den (cdr pr)])
    (format "(~a ~a)" num (duration->lily den))))

(define/contract (time-signature-compound->lily time-signature)
  (-> TimeSignatureCompound? string?)
  (let* ([groups (TimeSignatureCompound-groups time-signature)])
      (format "\\compoundMeter #'~a" (map numdenompr->lily groups))))

(define/contract (time-signature->lily time-signature)
  (-> time-signature/c string?)
  (cond [(TimeSignatureSimple?   time-signature) (time-signature-simple->lily   time-signature)]
        [(TimeSignatureGrouping? time-signature) (time-signature-grouping->lily time-signature)]
        [(TimeSignatureCompound? time-signature) (time-signature-compound->lily time-signature)]
        [else (error "timesignature->lily unexpected time signature")]))

(define/contract (voice-event->lily voiceevent)
  (-> voice-event/c string?)
  (cond [(Note?            voiceevent) (note->lily          voiceevent)]
        [(Rest?            voiceevent) (rest->lily          voiceevent)]
        [(Tuplet?          voiceevent) (tuplet->lily        voiceevent)]
        [(Chord?           voiceevent) (chord->lily         voiceevent)]
        [(clef?            voiceevent) (clef->lily          voiceevent)]
        [(KeySignature?    voiceevent) (keysignature->lily  voiceevent)]
        [else (error "voice-event->lily unexpected voiceevent")]))

(define/contract (pitched-voice->lily tempo time-signature voice)
  (-> tempo/c time-signature/c PitchedVoice? string?)
  (let ([instr  (PitchedVoice-instr       voice)]
        [events (PitchedVoice-voiceevents voice)])
    @string-append{
    \new Voice
    {\set Staff.instrumentName = #"@(instr->short-lily instr)" \set Staff.midiInstrument = #"@(instr->lily instr)"
    @(string-join (cons (tempo->lily tempo) (cons (time-signature->lily time-signature) (map voice-event->lily events)))) \bar "|."
    }
    }))
  
(define/contract (keyboard-voice->lily tempo time-signature voice )
  (-> tempo/c time-signature/c KeyboardVoice? string?)
  (let* ([instr      (KeyboardVoice-instr           voice)]
         [eventspair (KeyboardVoice-voiceeventspair voice)]
         [treble     (car eventspair)]
         [bass       (cdr eventspair)])
    @string-append{
    \new PianoStaff {
    <<
    \set PianoStaff.instrumentName = #"@(instr->short-lily instr)"\set PianoStaff.midiInstrument = #"@(instr->lily instr)"
    \new Staff = "rh" {
    \new Voice {                       
    @(string-join (cons (tempo->lily tempo) (cons (time-signature->lily time-signature) (map voice-event->lily treble)))) \bar "|."
    }
    }
    \new Staff = "lh" {
    \new Voice {                       
    @(string-join (cons (time-signature->lily time-signature) (map voice-event->lily bass))) \bar "|."
    }
    }    
    >>
    }
    }
    ))

(define/contract (splitstaff-voice->lily tempo time-signature voice)
  (-> tempo/c time-signature/c SplitStaffVoice? string?)
  (let* ([instr  (SplitStaffVoice-instr       voice)]
         [events (SplitStaffVoice-voiceevents voice)])
    @string-append{
    \new PianoStaff {
    <<
    \set PianoStaff.instrumentName = #"@(instr->short-lily instr)"\set PianoStaff.midiInstrument = #"@(instr->lily instr)"
    \new Staff = "up" {
    \new Voice {                       
    \clef treble \autoChange { @(tempo->lily tempo) @(time-signature->lily time-signature) @(string-join (map voice-event->lily events))))) } \bar "|."
    }
    }
    \new Staff = "down" {
    \new Voice {                       
    \clef bass \autoChange { @(time-signature->lily time-signature) } | \bar "|."
    }
    }
    >>
    }
    }
    ))

(define/contract (voice->lily tempo time-signature voice)
  (-> tempo/c time-signature/c voice/c string?)
  (cond [(PitchedVoice?    voice) (pitched-voice->lily    tempo time-signature voice)]
        [(KeyboardVoice?   voice) (keyboard-voice->lily   tempo time-signature voice)]
        [(SplitStaffVoice? voice) (splitstaff-voice->lily tempo time-signature voice)]
        [else (error "voice->lily unexpected voice")]))

(define/contract (voice-group->lily voice-group)
  (-> VoicesGroup? string?)
  (let ([tempo          (VoicesGroup-tempo          voice-group)]
        [time-signature (VoicesGroup-time-signature voice-group)]
        [voices         (VoicesGroup-voices         voice-group)])
    (string-join (map (lambda (voice) (voice->lily tempo time-signature voice)) voices))))

(define/contract (score->lily score)
  (-> Score? string?)
  (let ([title        (Score-title        score)]
        [seed         (Score-seed         score)]
        [voice-groups (Score-voice-groups score)])
    (let ([aligned-voice-groups (map extend&align-voices-group-durations voice-groups)])
      @string-append{
      \include "articulate.ly"
      \version "2.24.1"
      \header { title = "@title" copyright = "@seed" }
      structure = {
      <<
      @(string-join (map voice-group->lily aligned-voice-groups))
      >>
      }
      \score { \removeWithTag #'midi \structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }
      \score { \unfoldRepeats \articulate \keepWithTag #'midi \structure \midi {  } }
      }
      )))

#|
(define treble-notes `(Treble ,(Note 'C '0va 'Q '() #f) ,(Note 'D '0va 'Q '() #f) ,(Note 'E '0va 'S '() #f)))
(define bass-notes `(Bass ,(Note 'E '8vb 'Q '() #f) ,(Note 'D '8vb 'Q '() #f) ,(Note 'E '8vb 'S '() #f)))
(define voice (KeyboardVoice 'AcousticGrand (cons treble-notes bass-notes)))
(define voice-group (VoicesGroup (TempoDur 'Q 120) (TimeSignatureSimple 3 'Q) (list voice)))
(define score (Score "test" "seed" (list voice-group)))
(display (score->lily score))
|#
