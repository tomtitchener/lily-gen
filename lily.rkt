#lang at-exp racket

;; lily: convert Score to Lilypond string by traversing Score 
;; converting components to a string, use with display to render
;; newlines the output file
;; note: only renders, need to prepare VoicesGroup voices
;; with extend&align-voices-group-durations first, if needed.

(provide
 (contract-out
  ;; convert from a Score to a lilypond-formatted string,
  ;; display to render newlines
  [score->lily (-> Score? string?)]))
         
;; - - - - - - - - -
;; implementation
(require "score.rkt")

(require "lily-utils.rkt")

;; - - - - - - - - - - - - -
;; support utilites (private)
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

(define/contract (spacer->lily spacer)
  (-> Spacer? string?)
  (let ([duration (Spacer-dur spacer)])
    (string-append "s" (duration->lily duration))))

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
    (format "\\key ~a \\~a" (pitch-class->lily pitch) (mode->lily mode))))

(define/contract (time-signature-simple->lily time-signature)
  (-> TimeSignatureSimple? string?)
  (let ([num   (TimeSignatureSimple-num   time-signature)]
        [denom (TimeSignatureSimple-denom time-signature)])
    (format "\\time  ~s/~a" num (duration->lily denom))))

(define/contract (time-signature-grouping->lily time-signature)
  (-> TimeSignatureGrouping? string?)
  (let ([groups (TimeSignatureGrouping-groups  time-signature)]
        [num    (TimeSignatureGrouping-num     time-signature)]
        [denom  (TimeSignatureGrouping-denom   time-signature)])
    (format "\\time ~a ~a/~a" (string-join (map number->string groups) ",") num (duration->lily denom))))

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
        [(Spacer?          voiceevent) (spacer->lily        voiceevent)]
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

;; (-> Score? string?)
(define (score->lily score)
  (let ([title        (Score-title        score)]
        [seed         (Score-seed         score)]
        [voice-groups (Score-voice-groups score)])
    @string-append{
    \include "articulate.ly"
    \version "@(getenv "LILYPOND_VERSION")"
    \header { title = "@title" copyright = "@seed" }
    structure = {
    <<
    @(string-join (map voice-group->lily voice-groups))
    >>
    }
    \score { \removeWithTag #'midi \structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }
    \score { \unfoldRepeats \articulate \keepWithTag #'midi \structure \midi {  } }
    }
    ))

(module+ test
  (require rackunit)
  (require "scale.rkt")
  (require "meter.rkt")

  (define pitched-voice
    (let* ([af-key-signature (KeySignature 'Af 'Major)]
           [pitch->note-voice-event (lambda (pitch) (Note (car pitch) (cdr pitch) 'E '() #f))]
           [ascending-pitch-range (cons (cons 'C '8va) (cons 'Ef '15va))]
           [ascending-pitches (scale->pitch-range Af-major ascending-pitch-range)]
           [descending-pitch-range (cons (cons 'Ef '15va) (cons 'C '8va))]
           [descending-pitches (scale->pitch-range Af-major descending-pitch-range)]
           [all-pitches (append ascending-pitches descending-pitches)]
           [all-voice-events (map pitch->note-voice-event all-pitches)]
           [all-voice-events-with-clef (add-bass-or-treble-clefs-to-voice-events all-voice-events 'Treble)]
           [all-voice-events-with-key-signature-and-clef (cons af-key-signature all-voice-events-with-clef)])
      (PitchedVoice 'AcousticGrand all-voice-events-with-key-signature-and-clef)))
  
  (define keyboard-voice
    (let* ([ef-key-signature (KeySignature 'Ef 'Major)]
           [pitch->note-voice-event (lambda (pitch) (Note (car pitch) (cdr pitch) 'S '() #f))]
           [ascending-treble-pitch-range (cons (cons 'Ef '0va) (cons 'G '15va))]
           [ascending-treble-pitches (scale->pitch-range Ef-major ascending-treble-pitch-range)]
           [ascending-treble-voice-events (map pitch->note-voice-event ascending-treble-pitches)]
           [ascending-treble-voice-events-with-clef (add-bass-or-treble-clefs-to-voice-events ascending-treble-voice-events 'Treble)]
           [ascending-treble-voice-events-with-key-signature-and-clef (cons ef-key-signature ascending-treble-voice-events-with-clef)]
           [ascending-bass-pitch-range (cons (cons 'G '15vb) (cons 'Bf '0va))]
           [ascending-bass-pitches (scale->pitch-range Ef-major ascending-bass-pitch-range)]
           [ascending-bass-voice-events (map pitch->note-voice-event ascending-bass-pitches)]
           [ascending-bass-voice-events-with-clef (add-bass-or-treble-clefs-to-voice-events ascending-bass-voice-events 'Bass)]
           [ascending-bass-voice-events-with-key-signature-and-clef (cons ef-key-signature ascending-bass-voice-events-with-clef)]
           [keyboard-voice-events-pair (cons ascending-treble-voice-events-with-key-signature-and-clef
                                                          ascending-bass-voice-events-with-key-signature-and-clef)])
      (KeyboardVoice 'AcousticGrand keyboard-voice-events-pair)))
  
  (define (make-simple-voices-group voices)
    (let ([simple-tempo (TempoDur 'Q 120)]
          [simple-time-signature (TimeSignatureSimple 4 'Q)])
      (extend&align-voices-group-durations (VoicesGroup simple-tempo simple-time-signature voices))))

  (define (make-score title voices-groups)
    (Score title "seed" voices-groups))

  (define (test-score file-name score)
    (let* ([output-file-name (string-append "test/" file-name ".ly")]
           [output-port (open-output-file output-file-name #:mode 'text #:exists 'replace)])
      (display (score->lily score) output-port)
      (close-output-port output-port)
      (check-true (system (format "lilypond -s -o test ~v" output-file-name)))))  

  (let* ([voices-group (make-simple-voices-group (list keyboard-voice))]
         [score (make-score "simple ascending keyboard voice" (list voices-group))])
    (test-score "simple-ascending-keyboard-voice" score))
  
  (let* ([voices-group (make-simple-voices-group (list pitched-voice))]
         [score (make-score "simple pitched voice" (list voices-group))])
    (test-score "simple-pitched-voice" score))
  
  (let* ([voices-group (make-simple-voices-group (list pitched-voice keyboard-voice))]
         [score (make-score "simple pitched and keyboard voices" (list voices-group))])
    (test-score "simple-pitched-and-keyboard-voices" score))
  )
  

