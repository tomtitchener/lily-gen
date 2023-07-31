#lang racket

;; workspace.rkt:  experiment with routines

;; nothing to provide, this is a stub

(require "score.rkt")
(require "score-utils.rkt")
(require "scale.rkt")
(require "lily.rkt")
(require "lily-utils.rkt")

;; first target:  transpose/iterate

;; transpose/iterate with default values as parameters

(define gens-param (make-parameter 2))

(define scale-param (make-parameter C-major))

(define key-signature-param (thunk (scale->KeySignature (scale-param))))

(define scale-range-min-max-pair (thunk (scale->PitchRangeMinMaxPair (scale-param))))

(define start-pitch-param (make-parameter (cons 'C '0va)))

(define offset-param (make-parameter 0))

(define kernel-param (make-parameter '(0 1 2 1)))

(define inits-param  (make-parameter '(2 1)))

(define instr-param  (make-parameter 'AcousticGrand))

(define file-name-param (make-parameter "test"))

(define tempo-param (make-parameter (TempoDur 'Q 60)))

(define time-signature-param (make-parameter (TimeSignatureSimple 4 'Q)))

(define score-title-param (make-parameter "workspace"))

(define score-copyright-param (make-parameter "copyright"))

;; wrap this with a let setting overrides to the default param vals above
(define/contract transpose/iterate/parameterized
  (-> (listof (listof pitch/c)))
  (thunk
   (transpose/iterate (gens-param)
                      (scale-param)
                      (scale-range-min-max-pair)
                      (start-pitch-param)
                      (offset-param)
                      (kernel-param)
                      (inits-param))))

;; given (length kernel) as multiplier between generations, compute uniform durations for pitches
;; for each generation as (expt kernel-length num-generation) starting from 1 e.g. for four gens,
;; the series would be 4 16 64 256 mapped by int->durations into a list of durations becomes
;; '((T) (E) (H) (W. H)) reversed as '((W. H) (H) (E) (T)) for list from lowest to highest count
;; of pitches e.g. generations '(0 1 2 3)
(define/contract simple-voices-durations/parameterized
  (-> (listof (listof duration?)))
  (thunk
   (let ([kernel-length (length (kernel-param))])
     (map (compose int->durations (curry expt kernel-length)) (reverse (range 1 (add1 (gens-param))))))))

(define/contract (durs&pits->notes durations pitches)
  (-> (listof duration?) (listof pitch/c) (listof Note?))
  (flatten (map (curry ctrls-durs&pit->notes '() durations) pitches)))

;; deprecated with choice of SplitStaff voice
(define/contract (add-clefs clef voice-events)
  (-> clef? (listof voice-event/c) (listof voice-event/c))
  (add-bass-or-treble-clefs-to-voice-events voice-events clef))

(define/contract (add-key-signature voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
  (cons (key-signature-param) voice-events))

(define/contract (add-clefs&key-signature voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
    (add-clefs 'Treble (add-key-signature voice-events)))

(define/contract simple-iterated-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([simple-pitchess   (transpose/iterate/parameterized)]
          [simple-durationss (simple-voices-durations/parameterized)]
          [notess            (map durs&pits->notes simple-durationss simple-pitchess)]
          [voice-eventss     (map add-key-signature notess)])
     (map (curry SplitStaffVoice (instr-param)) voice-eventss))))

(define/contract (voices-group/parameterized voices)
  (-> (listof voice/c) VoicesGroup?)
  (VoicesGroup (tempo-param) (time-signature-param) voices))

(define/contract (score/parameterized voices-groups)
  (-> (listof VoicesGroup?) Score?)
  (Score (score-title-param) (score-copyright-param) voices-groups))

(define/contract simple-iterated-score/parameterized
  (-> Score?)
  (thunk
   (let* ([voices (simple-iterated-voices/parameterized)]
          [voices-group (voices-group/parameterized voices)])
     (score/parameterized (list voices-group)))))

(define/contract gen-score-file/parameterized
  (-> boolean?)
  (thunk
   (let* ([output-file-name (string-append "test/" (file-name-param) ".ly")]
          [output-port (open-output-file output-file-name #:mode 'text #:exists 'replace)]
          [score (simple-iterated-score/parameterized)])
     (display (score->lily score) output-port)
     (close-output-port output-port)
     (system (format "lilypond -s -o test ~v" output-file-name)))))

