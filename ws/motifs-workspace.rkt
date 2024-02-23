#lang racket

;; motifs-workspace.rkt:  sample motifs
;;
;; use with e.g. low clarinet, flute
;;
;; - murmurs:  soft, quickly oscillating patterns
;;   * parameterize by total length, rhythmic pattern, scale, pitch, interval (+/-)
;;   * grouped into multiple voices
;;   * core routine to emit list of notes
;;   * random duration rests with repeats, maybe variations widened intervals, shifted intervals?
;;   * arrange multiple-voice texture, start static

;; - arpeggios:  also soft, curtain-like
;;   * parameterize by scale, length, durations, start, intervals (not just uniform, +/-)
;;   * optional pattern on each step should be short, fast

;; aggregate to texture?

(require (only-in algorithms repeat))

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/generators)

;; start with murmurs

;; back-and-forth transposing starting from 0 of start pitch, then interval, then (- interval) back to startp pitch
;; continue for count elements, so even number ends with start pitch transposed by interval, odd with start pitch
;; 0 based transposition: 0 => unison, 1 => second, 2 => third, etc.
;; each note has the same duration, whole oscillation is slurred
;; list of list of accents is mapped one-by-one to note, so '((Accent) ()) repeats Accent, no control
;; note: generates "low" level motif, can be used raw or as input to FixedPitch.., FixedOctave.., or Tuplet.. motifs.
;; tbd: interval could be (or/c interval/c (non-empty-listof interval/c)) for alternating pitch and chord 
;;      or if start-pitch was chord, alternating chords
;; tbd: don't have to repeat dynamic with each motif, only need it for the first one
(define/contract (gen-oscillate-motif count interval duration controlss)
  (-> exact-positive-integer? interval/c duration? (non-empty-listof (listof control/c)) maybe-intervalss-motif/c)
  (define (add-control ctrl motif-element)
    (match motif-element
      [(list ints ctrls durs)
       (list ints (cons ctrl ctrls) durs)]))
  (let* ([intervals (take (cons 0 (apply append (repeat (quotient count 2) (list interval (- interval))))) count)]
         [durations (repeat count duration)]
         [controlss (take (apply append (repeat count controlss)) count)]
         [elements  (map (lambda (int ctrls dur) `(,int ,ctrls ,(list dur))) intervals controlss durations)]
         [init-elem (first elements)]
         [mid-elems (drop (drop-right elements 1) 1)]
         [end-elem  (last elements)])
    (append (cons (add-control 'SustainOn (add-control 'SlurOn init-elem)) mid-elems) (list (add-control 'SustainOff (add-control 'SlurOff end-elem))))))

(module+ test
  (require rackunit)
  
  (check-equal?
   (gen-oscillate-motif 6 1 'E '(() (Accent) ()))
   '((0 (SustainOn SlurOn) (E))
     (1 (Accent) (E))
     (-1 () (E))
     (1 () (E))
     (-1 (Accent) (E))
     (1 (SustainOff SlurOff) (E))))
  
  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 6 1 'E '(() (Accent) ())))
   (list '(D . 0va)
         (list (Note 'C '0va 'E '(SustainOn SlurOn) #f)
               (Note 'D '0va 'E '(Accent) #f)
               (Note 'C '0va 'E '() #f)
               (Note 'D '0va 'E '() #f)
               (Note 'C '0va 'E '(Accent) #f)
               (Note 'D '0va 'E '(SustainOff SlurOff) #f))))

  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 3 1 'E '(() (Accent) ())))
   (list '(C . 0va)
         (list (Note 'C '0va 'E '(SustainOn SlurOn) #f)
               (Note 'D '0va 'E '(Accent) #f)
               (Note 'C '0va 'E '(SustainOff SlurOff) #f))))

  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 3 -1 'E '(() (Accent) ())))
   (list '(C . 0va)
         (list (Note 'C '0va 'E '(SustainOn SlurOn) #f)
               (Note 'B '8vb 'E '(Accent) #f)
               (Note 'C '0va 'E '(SustainOff SlurOff) #f))))
  )
(define/contract (apply-dynamic-to-motif dynamic motif)
  (-> dynamic? maybe-intervalss-motifs/c maybe-intervalss-motifs/c)
  (match motif
    [(FixedPitchMaybeIntervalsMotif starting-pitch motif)
     (FixedPitchMaybeIntervalsMotif starting-pitch (apply-dynamic-to-motif dynamic motif))]
    [(FixedOctaveMaybeIntervalsMotif starting-octave motif)
     (FixedOctaveMaybeIntervalsMotif starting-octave (apply-dynamic-to-motif dynamic motif))]
    [(TupletMaybeIntervalsMotif num denom dur motif)
     (TupletMaybeIntervalsMotif num denom dur (apply-dynamic-to-motif dynamic motif))]
    [(? maybe-intervalss-motif/c maybe-intervalss-motif)
     (match (car maybe-intervalss-motif)
       [(list interval-or-intervals controls durations)
        (cons (list interval-or-intervals (cons dynamic controls) durations) (cdr maybe-intervalss-motif))])]))

;; repeat same motif interspersed with rests
(define/contract (render-repeated-motif count dynamic scale start-pitch rests-durations motif)
  (-> exact-positive-integer? dynamic? Scale? pitch/c (non-empty-listof duration?) maybe-intervalss-motifs/c (non-empty-listof (or/c Note? Chord? Rest? Tuplet?)))
  (let ([first-rendered-motif (second (render-maybe-intervalss-motifs scale start-pitch (apply-dynamic-to-motif dynamic motif)))]
        [rendered-motif (second (render-maybe-intervalss-motifs scale start-pitch motif))]
        [rests (map Rest rests-durations)])
    (let loop ([cnt count] [ret first-rendered-motif])
      (if (equal? 1 cnt)
          ret
          (loop (sub1 cnt) (append ret rests rendered-motif))))))

(module+ test
  (require rackunit)
  
  (check-equal?
   (render-repeated-motif 3 'Piano C-major (cons 'C '0va) (list 'Q 'E) (gen-oscillate-motif 6 1 'E  '(() (Accent) ())))
   (list
    (Note 'C '0va 'E '(Piano SustainOn SlurOn) #f)
    (Note 'D '0va 'E '(Accent) #f)
    (Note 'C '0va 'E '() #f)
    (Note 'D '0va 'E '() #f)
    (Note 'C '0va 'E '(Accent) #f)
    (Note 'D '0va 'E '(SustainOff SlurOff) #f)
    (Rest 'Q)
    (Rest 'E)
    (Note 'C '0va 'E '(SustainOn SlurOn) #f)
    (Note 'D '0va 'E '(Accent) #f)
    (Note 'C '0va 'E '() #f)
    (Note 'D '0va 'E '() #f)
    (Note 'C '0va 'E '(Accent) #f)
    (Note 'D '0va 'E '(SustainOff SlurOff) #f)
    (Rest 'Q)
    (Rest 'E)
    (Note 'C '0va 'E '(SustainOn SlurOn) #f)
    (Note 'D '0va 'E '(Accent) #f)
    (Note 'C '0va 'E '() #f)
    (Note 'D '0va 'E '() #f)
    (Note 'C '0va 'E '(Accent) #f)
    (Note 'D '0va 'E '(SustainOff SlurOff) #f)))
  )

;; wrap VoiceEvent from render-repeated-motif in a PitchedVoice
;; (gen-score-file (score/parameterized (list (render-repeated-motif-voice))))
(define/contract (render-simple-repeated-motif-voice)
  (-> voice/c)
  (let* ([osc-mot (gen-oscillate-motif 6 -2 'S '(() (Accent) ()))]
         [voice-events (render-repeated-motif 5 'PPPPP C-whole-tone (cons 'C '15va) (list 'H) osc-mot)])
    (PitchedVoice 'AcousticGrand voice-events)))

(define/contract (render-tuplet-repeated-motif-voice)
  (-> voice/c)
  (let* ([osc-mot (gen-oscillate-motif 9 -3 'S '((Tenuto)))]
         [tup-mot (TupletMaybeIntervalsMotif 3 2 'E osc-mot)]
         [voice-events (render-repeated-motif 5 'PPPPP C-whole-tone (cons 'Fs '0va) (list 'E 'E 'E) tup-mot)])
    (PitchedVoice 'Clarinet voice-events)))

;; experimented with several voices, slur is hardly audible even with legato clarinet,
;; also volume varies a lot, maybe balance is at mixing time, ugh

;; murmur texture has several voices with repeated oscillations, sempre ppp tenuto

