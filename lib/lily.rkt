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
(require lily-gen/lib/score)

(require lily-gen/lib/lily-utils)

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
        [(pan?        control) (pan->lily       control)]
        [(string?     control) (ann->lily       control)]
        [else (error 'control->lily "unexpected control ~e" control)]))

(define/contract (note->lily note)
  (-> Note? string?)
  (string-append
   (pitch->lily (cons (Note-pitch note) (Note-octave note)))
   (duration->lily (Note-dur note))
   (maybe-format-sempre-data (Note-tie note))
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
     "< ~a >~a~a~a~a"
     (string-join (map pitch->lily pitches) " ")
     (duration->lily dur)
     (maybe-format-sempre-data tie?)
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

;; to avoid stuffing repeated staccato, accent, sustain, etc. controls for
;; each note in a passage, use the Sempre and Orindale voice-events
;; Sempre writes one instance of "sempre <ctrl ...>" in italic at the start
;; and then in the midi data only, repeats <ctrl ...> for each voice-event
;; that follows until the Ordinale that ends the Sempre
;; gets dry/detache especially for percussion like piano

;; sempre data keeps state for voice being rendered to remember controls
;; for repeated notes, avoid controls for tied notes
(define sempre-data (make-hash '((ctrls . ())(mkup . #f)(ptie . #f))))

;; \markup \italic "sempre pppp e staccatissimo"
(define/contract (maybe-format-sempre-markup)
  (-> string?)
  (cond [(or (null? (hash-ref sempre-data 'ctrls)) (hash-ref sempre-data 'mkup))
         ""]
        [else
         (hash-set! sempre-data 'mkup #t)
         (let* ([controls (hash-ref sempre-data 'ctrls)]
                [markups  (map (compose string-downcase symbol->string) controls)])
           (format "-\\markup \\italic \"sempre ~a\"" (string-join markups " e ")))]))

;; \tag #'midi \pppp .. \tag #'midi -! ..
(define/contract (maybe-format-sempre-controls tie?)
  (-> boolean? string?)
  (let* ([controls (hash-ref sempre-data 'ctrls)]
         [markups  (map control->lily controls)])
    (if (null? markups)
        ""
        (let* ([ptie? (hash-ref sempre-data 'ptie)]
               [last-note? (and ptie? (not tie?))])
          (when last-note?
            (hash-set! sempre-data 'ptie #f))
          (when tie?
            (hash-set! sempre-data 'ptie #t))
          (if (or ptie? last-note?)
              ""
              (apply string-append (map (curry format "\\tag #'midi ~a") markups)))))))

(define/contract (maybe-format-sempre-data tie?)
  (-> boolean? string?)
  (string-append (maybe-format-sempre-markup) (maybe-format-sempre-controls tie?)))

(define/contract (init-sempre-data sempre)
  (-> Sempre? string?)
  (hash-set! sempre-data 'ctrls (Sempre-controls sempre))
  (hash-set! sempre-data 'mkup #f)
  (hash-set! sempre-data 'ptie #f)
  "")

(define/contract (reset-sempre-data)
  (-> string?)
  (hash-set! sempre-data 'ctrls '())
  (hash-set! sempre-data 'mkup #f)
  (hash-set! sempre-data 'ptie #f)
  "-\\markup \\italic \"ord.\"")

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
  (cond [(Note?         voiceevent) (note->lily          voiceevent)]
        [(Rest?         voiceevent) (rest->lily          voiceevent)]
        [(Spacer?       voiceevent) (spacer->lily        voiceevent)]
        [(Tuplet?       voiceevent) (tuplet->lily        voiceevent)]
        [(Chord?        voiceevent) (chord->lily         voiceevent)]
        [(clef?         voiceevent) (clef->lily          voiceevent)]
        [(KeySignature? voiceevent) (keysignature->lily  voiceevent)]
        [(Sempre?       voiceevent) (init-sempre-data    voiceevent)]
        [(Ordinale?     voiceevent) (reset-sempre-data)]
        [else (error "voice-event->lily unexpected voiceevent")]))

(define/contract (pitched-voice->lily tempo time-signature voice)
  (-> tempo/c time-signature/c PitchedVoice? string?)
  (let ([instr  (PitchedVoice-instr        voice)]
        [pan    (PitchedVoice-pan-position voice)]
        [events (PitchedVoice-voiceevents  voice)])
    @string-append{
    \new Voice
    {\set Staff.instrumentName = #"@(instr->short-lily instr)" \set Staff.midiInstrument = #"@(instr->lily instr)" @(pan->lily pan)
          @(string-join (cons (tempo->lily tempo) (cons (time-signature->lily time-signature) (cons (pan->markup pan) (map voice-event->lily events))))) \bar "|."
    }
    }))

(define/contract (keyboard-voice->lily tempo time-signature voice )
  (-> tempo/c time-signature/c KeyboardVoice? string?)
  (let* ([instr      (KeyboardVoice-instr           voice)]
         [pan        (KeyboardVoice-pan-position    voice)]
         [eventspair (KeyboardVoice-voiceeventspair voice)]
         [treble     (car eventspair)]
         [bass       (cdr eventspair)])
    @string-append{
    \new PianoStaff {
    <<
    \set PianoStaff.instrumentName = #"@(instr->short-lily instr)" \set PianoStaff.midiInstrument = #"@(instr->lily instr)"
    \new Staff = "rh" {
    @(pan->lily pan)
    \new Voice {                       
    @(string-join (cons (tempo->lily tempo) (cons (time-signature->lily time-signature) (cons (pan->markup pan) (map voice-event->lily treble))))) \bar "|."
    }
    }
    \new Staff = "lh" {
    @(pan->lily pan)
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
  (let* ([instr  (SplitStaffVoice-instr        voice)]
         [pan    (SplitStaffVoice-pan-position voice)]
         [events (SplitStaffVoice-voiceevents  voice)])
    @string-append{
    \new PianoStaff {
    <<
    \set PianoStaff.instrumentName = #"@(instr->short-lily instr)"\set PianoStaff.midiInstrument = #"@(instr->lily instr)"
    \new Staff = "up" {
    @(pan->lily pan)
    \new Voice {                       
    \clef treble \autoChange c' { @(tempo->lily tempo) @(time-signature->lily time-signature) @(pan->markup pan) @(string-join (map voice-event->lily events)) } \bar "|."
    }
    }
    \new Staff = "down" {
    @(pan->lily pan)
    \new Voice {                       
    \clef bass \autoChange c' { @(time-signature->lily time-signature) } | \bar "|."
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

(define/contract (split-staff->pitched-voice voice)
  (-> voice/c voice/c)
  (cond [(PitchedVoice?    voice) voice]
        [(KeyboardVoice?   voice) voice]
        [(SplitStaffVoice? voice) (PitchedVoice (SplitStaffVoice-instr voice)
                                                (SplitStaffVoice-pan-position voice)
                                                (SplitStaffVoice-voiceevents voice))]
        [else (error "split-staff->pitched-voice unexpected voice ~v" voice)]))

(define/contract (voices-group->midi-voices-group voices-group)
  (-> VoicesGroup? VoicesGroup?)
  (VoicesGroup (VoicesGroup-tempo voices-group)
               (VoicesGroup-time-signature voices-group)
               (map split-staff->pitched-voice (VoicesGroup-voices voices-group))))

(define/contract (voice-group->lily voice-group)
  (-> VoicesGroup? string?)
  (let ([tempo          (VoicesGroup-tempo          voice-group)]
        [time-signature (VoicesGroup-time-signature voice-group)]
        [voices         (VoicesGroup-voices         voice-group)])
    (string-join (map (lambda (voice) (voice->lily tempo time-signature voice)) voices))))

;; (-> Score? string?)
(define (score->lily score)
  (let ([title        (Score-title         score)]
        [copyright    (Score-copyright     score)]
        [voice-groups (Score-voices-groups score)]
        [lilypond-version (getenv "LILYPOND_VERSION")])
    (when (not lilypond-version)
      (error 'score->lily "no LILYPOND_VERSION environment variable"))
    @string-append{
    \include "articulate.ly"
    \version "@lilypond-version"
    \header { title = "@title" copyright = "@copyright" }
    layout-structure = {
    <<
    @(string-join (map voice-group->lily voice-groups))
    >>
    }
    midi-structure = {
    <<
    @(string-join (map voice-group->lily (map voices-group->midi-voices-group voice-groups)))
    >>
    }
    \score { \removeWithTag #'midi \layout-structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }
    \score { \unfoldRepeats \articulate \keepWithTag #'midi \midi-structure \midi {  } }
    }
    ))

(module+ test
  (require rackunit)
  (require lily-gen/lib/scale)
  (require lily-gen/lib/meter)
  (require lily-gen/lib/score-utils)

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
      (PitchedVoice 'AcousticGrand 'PanCenter all-voice-events-with-key-signature-and-clef)))

  (define split-staff-voice
    (let* ([bf-key-signature (KeySignature 'Bf 'Major)]
           [pitch->note-voice-event (lambda (pitch) (Note (car pitch) (cdr pitch) 'E '() #f))]
           [ascending-pitch-range (cons (cons 'Bf '15vb) (cons 'F '15va))]
           [ascending-pitches (scale->pitch-range Bf-major ascending-pitch-range 3)]
           [descending-pitch-range (cons (cons 'Bf '15va) (cons 'G '15vb))]
           [descending-pitches (scale->pitch-range Bf-major descending-pitch-range 3)]
           [all-pitches (append ascending-pitches descending-pitches ascending-pitches descending-pitches)]
           [all-voice-events (map pitch->note-voice-event all-pitches)]
           [all-voice-events-with-clef (add-bass-or-treble-clefs-to-voice-events all-voice-events 'Treble)]
           [all-voice-events-with-key-signature-and-clef (cons bf-key-signature all-voice-events-with-clef)])
      (SplitStaffVoice 'AcousticGrand 'PanCenter all-voice-events-with-key-signature-and-clef)))
  
  (define keyboard-voice
    [let* ([ef-key-signature (KeySignature 'Ef 'Major)]
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
      (KeyboardVoice 'AcousticGrand 'PanCenter keyboard-voice-events-pair)])
  
  (define (make-score title voices-groups)
    (Score title "copyright" voices-groups))

  (define (test-score file-name score)
    (let* ([output-file-name (string-append "test/" file-name ".ly")]
           [output-port (open-output-file output-file-name #:mode 'text #:exists 'replace)])
      (display (score->lily score) output-port)
      (close-output-port output-port)
      (check-true (system (format "lilypond -s -o test ~v" output-file-name)))))  

  (define (make-extended&aligned-simple-voices-group voices)
    (let ([simple-tempo (TempoDur 'Q 120)]
          [simple-time-signature (TimeSignatureSimple 4 'Q)])
      (extend&align-voices-group-durations (VoicesGroup simple-tempo simple-time-signature voices))))

  (let* ([voices-group (make-extended&aligned-simple-voices-group (list pitched-voice))]
         [score (make-score "exteded & aligned simple pitched voice" (list voices-group))])
    (test-score "extended&aligned-simple-pitched-voice" score))

  (let* ([voices-group (make-extended&aligned-simple-voices-group (list split-staff-voice))]
         [score (make-score "exteded & aligned split staff voice" (list voices-group))])
    (test-score "extended&aligned-split-staff-voice" score))

  (let* ([voices-group (make-extended&aligned-simple-voices-group (list keyboard-voice))]
         [score (make-score "exteded & aligned simple ascending keyboard voice" (list voices-group))])
    (test-score "extended&aligned-simple-ascending-keyboard-voice" score))
  
  (let* ([voices-group (make-extended&aligned-simple-voices-group (list pitched-voice keyboard-voice split-staff-voice))]
         [score (make-score "exteded & aligned split staff simple pitched and keyboard voices" (list voices-group))])
    (test-score "extended&aligned-split-staff-simple-pitched-and-keyboard-voices" score))

  (define (make-clipped&aligned-simple-voices-group voices)
    (let ([simple-tempo (TempoDur 'Q 120)]
          [simple-time-signature (TimeSignatureSimple 4 'Q)])
      (clip&align-voices-group-durations (VoicesGroup simple-tempo simple-time-signature voices))))

  (let* ([voices-group (make-clipped&aligned-simple-voices-group (list pitched-voice))]
         [score (make-score "exteded & aligned simple pitched voice" (list voices-group))])
    (test-score "clipped&aligned-simple-pitched-voice" score))

  (let* ([voices-group (make-clipped&aligned-simple-voices-group (list split-staff-voice))]
         [score (make-score "exteded & aligned split staff voice" (list voices-group))])
    (test-score "clipped&aligned-split-staff-voice" score))

  (let* ([voices-group (make-clipped&aligned-simple-voices-group (list keyboard-voice))]
         [score (make-score "exteded & aligned simple ascending keyboard voice" (list voices-group))])
    (test-score "clipped&aligned-simple-ascending-keyboard-voice" score))
  
  (let* ([voices-group (make-clipped&aligned-simple-voices-group (list pitched-voice keyboard-voice split-staff-voice))]
         [score (make-score "exteded & aligned split staff simple pitched and keyboard voices" (list voices-group))])
    (test-score "clipped&aligned-split-staff-simple-pitched-and-keyboard-voices" score))

  (define (ctrls-durs&pit->notes-no-tie controls durations pitch)
     (ctrls-durs&pit->notes controls durations pitch #f))

  ;; this causes the failure of test
  (let* ([gens                     4]
         [ef-key-signature         (KeySignature 'Ef 'Major)]
         [kernel-intervals         '(0 6 1 2)]
         [kernel-length            (length kernel-intervals)]
         [voices-durationss        (map (compose int->durations (curry expt kernel-length)) (reverse (range 1 (add1 gens))))]
         [durs&pits->notess        (lambda (durations pitches) (flatten (map (curry ctrls-durs&pit->notes-no-tie '() durations) pitches)))]
         [ef-major-pair            (scale->pitch-range-pair Ef-major)]
         [init-pitch               (cons 'Ef '8vb)]
         [init-intervals           '(3 0 5)]
         [self-sim-voices-pitchess (transpose/iterate gens Ef-major ef-major-pair init-pitch 0 kernel-intervals init-intervals)]
         [voices-notes             (map durs&pits->notess voices-durationss self-sim-voices-pitchess)]
         [voices-notes&clef        (map (lambda (voice-notes) (add-bass-or-treble-clefs-to-voice-events voice-notes 'Treble)) voices-notes)]
         [voices-notes&key&clef    (map (lambda (voice-events) (cons ef-key-signature voice-events)) voices-notes&clef)]
         [split-staff-voices       (map (lambda (voice-events) (SplitStaffVoice 'AcousticGrand 'PanCenter voice-events)) voices-notes&key&clef)]
         [simple-tempo             (TempoDur 'Q 120)]
         [simple-time-signature    (TimeSignatureSimple 4 'Q)]
         [voices-group             (VoicesGroup simple-tempo simple-time-signature split-staff-voices)]
         [score                    (make-score "selfsim voices" (list voices-group))])
      (test-score "self-sim-voices" score)))

;; Next steps:
;; * make routine for quick turnaround that outputs a self-similar score from args:
;;   - count generations
;;   - list of generations to extract for making voices
;;   - input routine to generate list of list of rhythms (including rests) per voice
;;     using list of pitches per voice, account for relative numbers of pitches per
;;     voice, which is actually a function of length of kernel and number of generation
;;     so e.g. per-generation multiplier is length of kernel * difference in generations
;;   - result should be creating and rendering score with lilypond ready to listen to
;;
;; Or maybe before trying to launch into integration of pitch and rhythm, just leave as is
;; and focus on interaction between kernel and init, also effect of 2x, 3x, 4x, and etc.
;; kernel lengths and length of inits
;;
;; Leverage parameters:  create parameters for key-signature, pair, kernel, inits,
;; tempo, time-signature, title, start pitch, list of generations to extract for voices, ...
;; Create a no-argument routine that initializes values from the parameters
;; and which produces e.g. a score I can pass to another routine to call
;; lilypond and answer if it succeeded or failed.
;;
;; Then explore interactively starting from very simple values for the kernel and inits lists.
