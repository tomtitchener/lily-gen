#lang racket

;; weighted-motifs-workspace.rkt

(require racket/generator)

(require lily-gen/lib/workspace)
(require lily-gen/lib/lily)
(require lily-gen/lib/utils)
(require lily-gen/lib/meter)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/score-utils)
(require lily-gen/lib/scale)
(require lily-gen/lib/generators)

(define high-start-pitch/param (make-parameter (cons 'C '8va)))

(define mid-start-pitch/param (make-parameter (cons 'Gs '0va)))

(define low-start-pitch/param (make-parameter (cons 'C '0va)))

(struct/contract VoiceParams2 ([instr       instr?]
                               [scale       Scale?]
                               [start-pitch pitch/c]
                               [motifs      weight&maybe-intervalss-motifs/c]))

(define (voiceparam2-values vps)
  (values (VoiceParams2-instr vps)
          (VoiceParams2-scale vps)
          (VoiceParams2-start-pitch vps)
          (VoiceParams2-motifs vps)))

(define/contract (voice-events->generator/VoiceParams2 voice2params)
  (-> VoiceParams2? generator?)
  (let-values ([(_ scale start-pitch motifs) (voiceparam2-values voice2params)])
    (weighted-maybe-intervalss-motifs/generator scale start-pitch motifs)))

;; this contains motifs of only the maybe-intervalss-motif-elements/c type, 
;; and even then, only of a single interval (Note) vs. multiple (Chord).
;;
;; start by adding a couple chords to this example.
;;
;; continue by trying out the FixedPitchMotif and FixedOctaveMotif
;;
;; finish with the TupletMotif
;;
(define weight&maybe-interval-motifs/param
  (thunk
   (list (list 1 (list (list (list 1 2) '(Accent) '(E.)) (list (list 0 4) '() '(S)) (list -1 '() '(E)) (list #f '() '(E))))  ;; ends one same step
         (list 1 (list (list -1 '() '(S)) (list 1 '() '(S)) (list 1 '(Accent) '(E))))                                        ;; ends up one step
         (list 1 (list (list -3 '() '(S)) (list 0 '() '(E)) (list 0 '() '(S)) (list 1 '(Accent) '(E)) (list 2 '() '(E))))    ;; ends on same step
         (list 1 (FixedPitchMaybeIntervalsMotif (cons 'As '8vb) (list (list -1 '() '(S)) (list 1 '() '(S)) (list 1 '(Accent) '(E)))))
         (list 1 (FixedOctaveMaybeIntervalsMotif '15vb (list (list -1 '() '(S)) (list 1 '() '(S)) (list 1 '(Accent) '(E)))))
         (list 1 (TupletMaybeIntervalsMotif 3 2 'E (list (list -1 '() '(S)) (list -1 '() '(S)) (list 1 '() '(S)) (list -1 '() '(S)) (list -1 '() '(S)) (list 1 '() '(S)))))
         ))) 

(define voice2-high/param
  (thunk (VoiceParams2 (piano/param)
                       (scale/param)
                       (high-start-pitch/param)
                       (weight&maybe-interval-motifs/param))))

(define voice2-mid/param
  (thunk (VoiceParams2 (piano/param)
                       (scale/param)
                       (mid-start-pitch/param)
                       (weight&maybe-interval-motifs/param))))

(define voice2-low/param
  (thunk (VoiceParams2 (piano/param)
                       (scale/param)
                       (low-start-pitch/param)
                       (weight&maybe-interval-motifs/param))))

(define voices2-params/param (thunk (list (voice2-high/param) (voice2-mid/param) (voice2-low/param))))

(define/contract voice-param2-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([notes-or-restss/gens (map voice-events->generator/VoiceParams2 (voices2-params/param))]
          [notes-or-restss      (map (lambda (gen)
                                       (apply append (while/generator->list (sum<=? (const 1) (count/param)) gen)))
                                     notes-or-restss/gens)]
          [voice-eventss        (map add-key-signature/parameterized notes-or-restss)]
          [instrs               (map VoiceParams2-instr (voices2-params/param))])
     (map (lambda (instr ves) (SplitStaffVoice instr ves)) instrs voice-eventss))))

#|

(parameterize
  ((scale/param C-whole-tone))
   (cnt/param 20)
  (gen-score-file (score/parameterized (voice-param2-voices/parameterized))))

That's easier to listen to.  And with three voices you get random rhythmic synchronization.

|#
