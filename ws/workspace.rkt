#lang racket

;; workspace.rkt:  experiment with enhanced REPL using parameterization

(provide (all-defined-out))

(require lily-gen/lib/lily)
(require lily-gen/lib/meter)
(require lily-gen/lib/score)
(require lily-gen/lib/score-utils)
(require lily-gen/lib/swell-utils)
(require lily-gen/lib/scale)

;; general-purpose params, common to specialized generators

(define scale/param (make-parameter C-major))

(define (scale->KeySignature scale)
  (let-values ([(tonic mode) (scale->key-signature-values scale)])
    (KeySignature tonic mode)))

(define key-signature/param (make-derived-parameter scale/param identity scale->KeySignature))

;; Hack!  Calling scale->pitch-range-pair in scale.rkt causes a memory access failure.  Something to do with contracts?
(define (loc-scale->pitch-range-pair scale)
  (-> Scale? pitch-range-pair/c)
  (cons (index->pitch scale 0) (index->pitch scale (scale->max-idx scale))))

(define scale-range-min-max-pair/param (make-derived-parameter scale/param identity loc-scale->pitch-range-pair))

(define count/param  (make-parameter 10))

(define instr/param  (make-parameter 'AcousticGrand))

(define piano/param (make-parameter 'AcousticGrand))

(define file-name/param (make-parameter "test"))

(define tempo/param (make-parameter (TempoDur 'Q 60)))

(define time-signature/param (make-parameter (TimeSignatureSimple 4 'Q)))

(define score-title/param (make-parameter "workspace"))

(define score-copyright/param (make-parameter "copyright"))

(define/contract (add-key-signature/parameterized voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
  (cons (key-signature/param) voice-events))

;; Not for use with SplitStaff voices
(define/contract (add-clefs clef voice-events)
  (-> clef? (listof voice-event/c) (listof voice-event/c))
  (add-bass-or-treble-clefs-to-voice-events voice-events clef))

;; to use this, provide list of voice/c,
;; consumes tempo/param, time-signature/param, score-title/param, score-copyright/param
(define/contract (score/parameterized voices)
  (-> (listof voice/c) Score?)
  (let* ([voices-group (VoicesGroup (tempo/param) (time-signature/param) voices)]
         [extended&aligned-voices-group (extend&align-voices-group-durations voices-group)]
         [extended&aligned-voices-group-with-swells (encode-voices-group-swells extended&aligned-voices-group)])
    (Score (score-title/param) (score-copyright/param) (list extended&aligned-voices-group-with-swells))))

;; consumes file-name/param, e.g.
;; (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized)))
;; parameterized:
;;  (parameterize ([gens/param 4] [kernel/param '(3 2 1 -3)] [inits/param '(0 2 4 -1 -3 2)])
;;    (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))
(define/contract (gen-score-file score)
  (-> Score? boolean?)
  (let* ([output-file-name (string-append "test/" (file-name/param) ".ly")]
         [output-port (open-output-file output-file-name #:mode 'text #:exists 'replace)])
    (display (score->lily score) output-port)
    (close-output-port output-port)
    (system (format "lilypond -s -o test ~v" output-file-name))))

