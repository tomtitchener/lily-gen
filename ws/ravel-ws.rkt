#lang racket

;; ravel-ws.rkt:  encode Ravel textures
;;

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)

;; Une barque sur l'ocean

(define arpeggio-pitches-1 (transpose/successive
                            Fs-minor
                            (scale->pitch-range-pair Fs-minor)
                            (cons 'Fs '8vb)
                            '(0 4 2 3 2  2 3 -3 -2 -2 -3 -2)))

(define arpeggio-pitches-2 (transpose/successive
                            Fs-minor
                            (scale->pitch-range-pair Fs-minor)
                            (cons 'Fs '8vb)
                            '(0 4 2 3 -3 -2 -4 4 2 3 -3 -2)))

(define arpeggio-controlss-1 '((SustainOn PPP SlurOn) () () (Accent) () ()
                               (Accent) () () (Accent) () (SlurOff)))

(define arpeggio-controlss-2 '((Accent PPP SlurOn) () () ()
                               (Accent) () () ()
                               (Accent) () () (SlurOff)))

(define arpeggio-notes-1 (map (lambda (p cs) (Note (car p) (cdr p) 'T cs #f)) arpeggio-pitches-1 arpeggio-controlss-1))

(define arpeggio-notes-2 (map (lambda (p cs) (Note (car p) (cdr p) 'T cs #f)) arpeggio-pitches-2 arpeggio-controlss-2))

(define arpeggio-tuplet-1 (Tuplet 12 8 'Q arpeggio-notes-1))

(define arpeggio-tuplet-2 (Tuplet 12 8 'Q arpeggio-notes-2))

(define arpeggios-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                      (list (KeySignature 'Fs 'Minor)
                                            arpeggio-tuplet-1
                                            arpeggio-tuplet-2)))

(define chords-pitches (transpose/successive
                        Fs-minor
                        (scale->pitch-range-pair Fs-minor)
                        (cons 'A '8va)
                        '((0 4) (-1 3) (0 3) (-4 4) (2 1))))

(define chords-controlss '((SustainOn PPP SlurOn) () () () (SlurOff)))

(define chords-ties '(#f #t #f #f #f))

(define chords (map (lambda (ps cs t) (Chord ps 'E cs t)) chords-pitches chords-controlss chords-ties))

(define chords-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                   (list (KeySignature 'Fs 'Minor)
                                         (first chords)
                                         (second chords)
                                         (Tuplet 3 2 'Q (list (third chords) (fourth chords) (fifth chords))))))

(define arpeggios-score (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                       (tempo/param (TempoDur 'E 40)))
                          (gen-score-file (score/parameterized (list arpeggios-voice chords-voice)))))
  
