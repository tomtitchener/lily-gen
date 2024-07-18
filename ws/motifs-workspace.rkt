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

;; back-and-forth transposing starting from 0 of start pitch, then interval, then (- interval) back to start pitch
;; continue for count elements, so even number ends with start pitch transposed by interval, odd with start pitch
;; 0 based transposition: 0 => unison, 1 => second, 2 => third, etc.
;; each note has the same duration, whole oscillation is slurred
;; list of list of accents is mapped one-by-one to note, so '((Accent) ()) repeats Accent, no control
;; generates "low" level motif, can be used raw or as input to FixedPitch.., FixedOctave.., or Tuplet.. motifs.
;; tbd: interval could be (or/c interval/c (non-empty-listof interval/c)) for alternating pitch and chord 
;;      or if start-pitch was chord, alternating chords
;; tbd: don't have to repeat dynamic with each motif, only need it for the first one
;; tbd: interval could be list of intervals for arpeggio pattern, or just repeat of whatever, making this
;;      into gen-ostinato-motif
(define/contract (gen-oscillate-motif count interval duration cntrlss init-cntrls end-cntrls)
  (-> exact-positive-integer? interval/c duration? (non-empty-listof (listof control/c)) (listof control/c) (listof control/c) maybe-intervalss-motif/c)
  (define (add-cntrls new-ctrls motif-element)
    (match motif-element
      [(list ints ctrls durs)
       (list ints (append new-ctrls ctrls) durs)]))
  (let* ([intervals (take (cons 0 (apply append (repeat (quotient count 2) (list interval (- interval))))) count)]
         [durations (repeat count duration)]
         [cntrlss   (take (apply append (repeat count cntrlss)) count)]
         [elements  (map (lambda (int ctrls dur) `(,int ,ctrls ,(list dur))) intervals cntrlss durations)]
         [init-elem (first elements)]
         [mid-elems (drop (drop-right elements 1) 1)]
         [end-elem  (last elements)])
    (append (cons (add-cntrls init-cntrls init-elem) mid-elems) (list (add-cntrls end-cntrls end-elem)))))

(module+ test
  (require rackunit)
  
  (check-equal?
   (gen-oscillate-motif 6 1 'E '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff))
   '((0 (SustainOn SlurOn) (E))
     (1 (Accent) (E))
     (-1 () (E))
     (1 () (E))
     (-1 (Accent) (E))
     (1 (SustainOff SlurOff) (E))))
  
  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 6 1 'E '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff)))
   (list '(D . 0va)
         (list (Note 'C '0va 'E '(SustainOn SlurOn) #f)
               (Note 'D '0va 'E '(Accent) #f)
               (Note 'C '0va 'E '() #f)
               (Note 'D '0va 'E '() #f)
               (Note 'C '0va 'E '(Accent) #f)
               (Note 'D '0va 'E '(SustainOff SlurOff) #f))))

  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 3 1 'E '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff)))
   (list '(C . 0va)
         (list (Note 'C '0va 'E '(SustainOn SlurOn) #f)
               (Note 'D '0va 'E '(Accent) #f)
               (Note 'C '0va 'E '(SustainOff SlurOff) #f))))

  (check-equal?
   (render-maybe-intervalss-motifs C-major (cons 'C '0va) (gen-oscillate-motif 3 -1 'E '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff)))
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
   (render-repeated-motif 3 'Piano C-major (cons 'C '0va) (list 'Q 'E) (gen-oscillate-motif 6 1 'E  '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff)))
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
  (let* ([osc-mot (gen-oscillate-motif 6 -2 'S '(() (Accent) ()) '(SustainOn SlurOn) '(SustainOff SlurOff))]
         [voice-events (render-repeated-motif 5 'PPPPP C-whole-tone (cons 'C '15va) (list 'H) osc-mot)])
    (PitchedVoice 'AcousticGrand voice-events)))

(define/contract (render-tuplet-repeated-motif-voice)
  (-> voice/c)
  (let* ([osc-mot (gen-oscillate-motif 1 9 -3 'S '(()) '(SustainOn SlurOn) '(SustainOff SlurOff))]
         [tup-mot (TupletMaybeIntervalsMotif 3 2 'E osc-mot)]
         [voice-events (render-repeated-motif 5 'PPPPP C-whole-tone (cons 'Fs '0va) (list 'E 'E 'E) tup-mot)])
    (PitchedVoice 'Clarinet voice-events)))

;; (parameterize ((time-signature/param (TimeSignatureSimple 9 'E)) (tempo/param (TempoDur 'Q 60))) (gen-score-file (score/parameterized (list (render-tuplet-repeated-motif-voice)))))

;; texture with alternating, variable-length repeats of an oscillating motif
;; where link is shared pitch (either final or preceding final) with new
;; interval in contrasting direction, e.g. if last pitch and oscillation
;;
;; up / down (negative) goes to down / up (positive)
;; down / up (positive) goes to up / down (negative)
;;
;; new voice should be in different octave so it doesn't sound 
;; like continuation of same voice and with different pan setting
;; or maybe pan setting is enough to differentiate? experiment
;;
;; overlap second voice by count from end of first, 0 means end,
;; note preceeding that, and etc.
;; overlapped start is same maybe in new octave, then oscillation
;; is in opposite direction
;;
;; overlapping voice may have different duration so speed of 
;; oscillation can vary
;;
;; include accents for grouping in 2s, 3s, 4s, etc.
;;
;; second voice is first overlapping voice, then first rests and
;; continues overlapping second, then two trade off back and forth
;;
;; data common to both voices
;; - scale
;; - start pitch
;; - count of overlap pitches (natural? to allow for 0)
;; 
;; data for lead voice:
;; - interval interval/c
;; - count of oscillations exact-positive-integer?
;; - duration duration?
;; - controls (listof control/c)
;;
;; data for derived voice:
;; - interval interval/c
;;
;; interval for derived voice should reverse direction ascending/descending

;; tbd: chords for start-pitch, interval and xpose so it's not just two pitches all the time

(struct/contract OscCommon ([repetitions exact-positive-integer?] [scale Scale?] [start-pitch pitch/c] [count-leading-rests natural?]) #:transparent)

(struct/contract OscLead ([interval interval/c] [count-oscillations exact-positive-integer?] [duration duration?] [controls (non-empty-listof (listof control/c))]) #:transparent)

(struct/contract OscDerived ([xpose interval/c] [interval interval/c]) #:transparent)

(define osc-event/c
  (make-flat-contract #:name 'osc-event/c #:first-order (or/c Note? Chord? Tuplet?)))

;; https://stackoverflow.com/questions/55644535/is-it-possible-to-use-extract-struct-info-outside-a-macro
(define (explode-struct s)
  (define-values (type skipped?) (struct-info s))
  (define-values (name inits autos acc mut imms super super-skipped?) (struct-type-info type))
  (apply values (for/list [(i (range 0 inits))] (acc s i))))

;; Tricky bit:  if I have second voice overlap first by N notes and continue with the first voice
;; overlapping the second with N notes, then the most N can be is the count of notes / 2, rounding
;; down for an odd N.
;; An alternative is to start the second repetition of the first voice immediately after the last
;; note of second or on the next beat, so the overall repetition is of 1 + 2, 1 + 2 ...
(define/contract (gen-alt-osc common lead derived)
  (-> OscCommon? OscLead? OscDerived? (cons/c (non-empty-listof voice-event/c) (non-empty-listof voice-event/c)))
  ;; generate lead and follow motifs
  (let-values ([(reps scale start-pitch count-leading-rests) (explode-struct common)]
               [(lead-interval count-oscillations lead-duration lead-controlss) (explode-struct lead)]
               [(derived-xpose derived-interval) (explode-struct derived)])
    (let ([lead-motif (gen-oscillate-motif count-oscillations lead-interval lead-duration lead-controlss '(SustainOn SlurOn) '(SustainOff SlurOff))]
          [derived-motif (gen-oscillate-motif count-oscillations derived-interval lead-duration lead-controlss '(SustainOn SlurOn) '(SustainOff SlurOff))])
      ;; compute rests, derived start-pitch
      (let* ([start-rests (map Rest (make-list count-leading-rests lead-duration))]
             [count-overlap-notes (- count-oscillations count-leading-rests)]
             [run-rests (map Rest (make-list (- count-leading-rests count-overlap-notes) lead-duration))]
             [pitch-range-pair (scale->pitch-range-pair scale)]
             [raw-derived-start-pitch (if (even? count-leading-rests) start-pitch (xpose scale pitch-range-pair start-pitch lead-interval))]
             [xposed-derived-start-pitch (xpose scale pitch-range-pair raw-derived-start-pitch derived-xpose)])
        (let ([lead-notes (second (render-maybe-intervalss-motifs scale start-pitch (apply-dynamic-to-motif 'PPPPP lead-motif)))]
              [derived-notes (second (render-maybe-intervalss-motifs scale xposed-derived-start-pitch (apply-dynamic-to-motif 'PPPPP derived-motif)))])
          `(,(apply append (make-list reps (append lead-notes run-rests)))
            .
            ,(append start-rests (apply append (make-list (sub1 reps) (append derived-notes run-rests))) derived-notes)))
          ))))

(define common/param  (make-parameter (OscCommon 4 C-major (cons 'C '0va) 12)))

(define lead/param  (make-parameter (OscLead 1 24 'S '(()))))

(define derived/param  (make-parameter (OscDerived 7 -3)))

(define/contract (render-osc-voices)
  (-> (non-empty-listof voice/c))
  (let* ([voice-eventss (gen-alt-osc (common/param) (lead/param) (derived/param))]
         [lead-voice (PitchedVoice 'AcousticGrand -0.5 (car voice-eventss))]
         [derived-voice (PitchedVoice 'AcousticGrand 0.5 (cdr voice-eventss))])
    (list lead-voice derived-voice)))

;; (parameterize ((time-signature/param (TimeSignatureSimple 4 'Q)) (tempo/param (TempoDur 'Q 120))) (gen-score-file (score/parameterized (render-osc-voices))))

;; (parameterize ((tempo/param (TempoDur 'Q 120)) (common/param (OscCommon 4 C-major (cons 'C '0va) 12)) (lead/param (OscLead 1 24 'S '(())))) (gen-score-file (score/parameterized (render-osc-voices))))

;; feels like its going to be a lot of work to make something that's just boring

;; mix it up with varied repetitions:  widening/narrowing intervals,
;; shorter/longer durations, transpositions so register progresses
;; up/down

;; programmatic sequence is easier when data is not rendered into
;; notes yet, still in terms of inputs to motifs
;;
;; three-stage process, computing series of pair of motifs, then
;; rendering to score, then rendering via lilypond

;; variety of ostinatos:  walking bass style with rapid register
;; shift, wide-spanning arpeggios, oscillation over wide intervals

;; staging of pairs of voices for richer texture, how to pairs
;; interact?


