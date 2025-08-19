#lang racket

;; canon-workspace.rkt:  ressurrect canon texture
;;

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)

;; create a library of motifs to explore generating different textures with rotated canons
;;
;; - motif is (non-empty-listof (or/c TupletMaybeIntervalsMotif? maybe-intervalss-motif/c))
;;   * (TupletMaybeIntervalsMotif? num denom dur tuplet-motif-element/c)
;;      where first two are natural-number/c, third is duration?, fourth is just maybe-intervalss-motif/c
;;      not either of FixedPitchMaybeIntervalsMotif? or FixedOctaveMaybeIntervalsMotif? because I use absolute transposition
;;   * maybe-intervalss-motif/c is non-empty-listof (list/c maybe-interval-or-intervals/c (listof control/c) (non-empty-listof duration?))
;;
;; create a registry of individual motifs as a hash list accessible by symbol
;;
;; - create a bottom-level routine to take a scale, a pitch, and list of symbols
;;   * build list of symbols into a list of motifs
;;   * call render-abs-maybe-intervalss-motifs to create a new notes-motif as an individual line in a choir
;;
;; - create a wrapper (-> (non-empty-listof symbol?) (non-empty-listof (list/c Scale? pitch/c exact-integer?)) notes-motifs/c) to
;;   take a symbol for a motif plus a list of scale, starting-pitch, and rotation count that builds a texture
;    by calling render-abs-maybe-intervalss-motifs for each where canon is concatenation of motifs in first argument
;;
;; - memoize experiments with another hash list by symbol of trials where score and midi files are saved by unique names
;;   through workspace parameterization
;;   * different range spans for motif pitches
;;   * different starting-pitches for same canon in the same key, widely separated registers, overlapping registers
;;   * same starting-pitches for same canon in different keys, keys with many overlapping pitches and keys with few
;;   * ...
;;

;; Pattern that's recognizable:  ((5 (Accent) (S)) (1 () (S)) (2 (Accent) (E)) (4 (Accent) (E)))

;; hash table method: NB these are all absolute transpositions 
;; (hash key val ...) so e.g. for motifs
(define motifs-hash
  (hash 'eighth-rest-mot            '((#f () (E)))
        'second-up-chug-mot         '((1 (Accent) (E)) (1 () (E)) (2 (Accent) (E)) (2 () (E)))
        'second-down-chug-mot       '((2 (Accent) (E)) (2 () (E)) (1 (Accent) (E)) (1 () (E)))
        'second-up-chug-3rds-mot    '(((1 3) (Accent) (E)) ((1 3) () (E)) ((2 3) (Accent) (E)) ((2 3) () (E)))
        'second-down-chug-3rds-mot  '(((2 3) (Accent) (E)) ((2 3) () (E)) ((1 3) (Accent) (E)) ((1 3) () (E)))
        'second-up-chug-3rds-S-mot  '(((1 3) (Accent) (S)) ((1 3) () (S)) ((2 3) (Accent) (S)) ((2 3) () (S)))
        'second-down-chug-3rds-S-mot'(((2 3) (Accent) (S)) ((2 3) () (S)) ((1 3) (Accent) (S)) ((1 3) () (S)))
        'tuple-second-up-chug-mot   `(,(TupletMaybeIntervalsMotif 3 2 'Q '((1 (Accent) (E)) (1 () (E)) (1 () (E))))
                                      ,(TupletMaybeIntervalsMotif 3 2 'Q '((2 (Accent) (E)) (2 () (E)) (2 () (E)))))
        'two-thirds-up-mot          '((#f () (E)) ((2 -3) () (S)) ((2 -3) () (S)) ((3 -3) (Accent) (S)) (#f () (S)))
        'thirds-down-mot            '(((3 -3) (Accent) (S)) ((1 -3) () (S)) ((1 -3) (Accent) (S)) (#f () (E.)))
        'two-sixths-up-mot          '((#f () (E)) ((2 -6) () (S)) ((2 -6) () (S)) ((3 -6) (Accent) (S)) (#f () (S)))
        'sixths-down-mot            '(((3 -6) (Accent) (S)) ((1 -6) () (S)) ((1 -6) (Accent) (S)) (#f () (E.)))
        ;;
        'bounce-down-mot-1          '((4 (Accent) (E)) (1 () (S))       (5 () (S))             (1 () (E))      (2 (Accent) (E)))
        'bounce-down-mot-2          '((4 (Accent) (E)) (1 () (E))       (5 () (S))             (1 () (S))      (2 (Accent) (E)))
        'bounce-up-mot              '((-2 () (E))      (1 (Accent) (S)) ((-3 -3) (Accent) (S)) ((1 -3) () (E)) (-4 () (E)))
        'ring-high-mot              '((#f () (E))      (9 () (S))       (9 (Accent) (S))        (9 () (E)))
        ;;
        ))

(define canons-hash
  (hash 'second-up-chug-can `(((second-up-chug-mot) ,C-major (C . 0va) 0)
                              ((second-up-chug-mot) ,C-major (E . 0va) 3)
                              ((second-up-chug-mot) ,C-major (G . 0va) 5)
                              ((second-up-chug-mot) ,C-major (A . 0va) 7))
        'second-up-down-chug-can `(((second-up-chug-mot)   ,C-major (C . 0va) 0)
                                   ((second-down-chug-mot) ,C-major (A . 8vb) 3)
                                   ((second-up-chug-mot)   ,C-major (E . 0va) 5)
                                   ((second-down-chug-mot) ,C-major (C . 0va) 7))
        'test-simple-mots-can `(((second-up-chug-mot second-down-chug-mot) ,C-major (C . 0va) 0)
                                ((second-down-chug-mot second-up-chug-mot) ,C-major (A . 8vb) 1))
        'chug-up-down-can `(((second-up-chug-3rds-mot   second-down-chug-mot)     ,C-major (D . 8va) 0)
                            ((second-down-chug-3rds-mot second-up-chug-3rds-mot)  ,C-major (G . 0va) 1)
                            ((second-up-chug-3rds-mot   second-down-chug-mot)     ,C-major (G . 8vb) 2)
                            ((second-down-chug-3rds-mot second-up-chug-3rds-mot)  ,C-major (C . 8vb) 3))
        'tuple-second-up-chug-can `(((tuple-second-up-chug-mot second-down-chug-mot) ,C-major (D . 8va) 1)
                                    ((second-down-chug-mot tuple-second-up-chug-mot) ,C-major (F . 0va) 0))
        'sixths-up-down-can `(((two-sixths-up-mot sixths-down-mot two-sixths-up-mot sixths-down-mot two-sixths-up-mot sixths-down-mot) ,C-major (C . 8va) 0)
                              ((two-thirds-up-mot thirds-down-mot two-thirds-up-mot thirds-down-mot two-thirds-up-mot thirds-down-mot) ,F-major (A . 0va) 3)
                              ((sixths-down-mot two-sixths-up-mot sixths-down-mot two-sixths-up-mot sixths-down-mot two-sixths-up-mot) ,C-major (E . 0va) 0)
                              ((second-up-chug-3rds-S-mot second-down-chug-3rds-S-mot eighth-rest-mot second-up-chug-3rds-S-mot second-down-chug-3rds-S-mot
                                second-up-chug-3rds-S-mot second-down-chug-3rds-S-mot eighth-rest-mot second-up-chug-3rds-S-mot second-down-chug-3rds-S-mot)
                               ,C-major (E . 0va) 5)
                              )
        ;;
        'bounce-around-can `(((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) 0)
                             ((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) 4)
                             ((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) 8)
                             ((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) 12)
                             ((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) 16))
        ))

;; - fourth example with irregular stagger of delays yields the most interesting texture with all voices playing
;; - two motifs stand out: repeated high ds S-S-E, repeated back-and-forth low sixths between voices
(define staggered-canons-hash
  (hash 'staggered-bounce-around-can-1 `((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) (() (W W W) (W W) (W) (H)))
        'staggered-bounce-around-can-2 `((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) (() (W Q Q) (W Q Q) (W Q Q) (W Q Q)))
        'staggered-bounce-around-can-3 `((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) (() (W W W) (W W Q) (W W W) (W W Q)))
        'staggered-bounce-around-can-4 `((bounce-down-mot-1 bounce-up-mot ring-high-mot bounce-down-mot-2) ,C-major (C . 0va) (() (W W W) (W W Q) (W W) (W Q)))
        ))

;; - next, identify transitions and how to encode them
;;   * one would be repetitions to grow motto at beginning, at end, or in the middle
;;   * another would shortening motto from start or end by omitting notes
;;   * another would be growing motto with new notes at the beginning or the end
;; - need to work out coding first then how to configure, pick incrementally growing to second pattern
;;
;; first try incremental substitution from the end of a motif forward
;;   * start with staggered list of motifs like staggered-bounce-around-can-4
;;   * for now, just introduce one replacement motif
;;     - outcome is swapping old with new, back-to-front, what about different lengths?
;;     - start by trying back to front
;;       * for shorter target, replace from back to front then after exhausting new source, drop rest of source note by note until just target is left,
;;         always completes in count of repetitions equal to length of source
;;       * for longer target first chew incrementally through source, then incrementally grow toward front of target until done
;;     - next try same idea front to back, see if there's a difference in the effect
;;     - also possible to do simultaneously with all motifs in sequence, e.g. to do one, just repeat surrounding motifs and treat all equally
;;       and might as well start with that and see how it sounds all-at-once vs motif-by-motif
;;   * notation will be equal-length lists of motifs, note that with swapping in a list of rests, one-for-one, you'd make texture sparser

(define (squeeze-mot mot)
  (match mot
    [(TupletMaybeIntervalsMotif num denom dur mot)
     (TupletMaybeIntervalsMotif num denom dur (squeeze-mot mot))]
    [(? maybe-intervalss-motif/c maybe-intervalss-motif)
     (map (lambda (mot)
            (match mot
              [(list mints ctrls durs)
               (list (squeeze-mint-or-intss mints) ctrls durs)]))
          maybe-intervalss-motif)]
    [(var one-or-others)
     (map squeeze-mot one-or-others)]))

;; immediate simultaneous canons distinguished by rotation distance, pulling back to front
(define (render-canon can-sym)
  (define (render-canon-voice can)
    (match can
      [(list mot-syms scale start-pitch rot)
       (define (render-canon-mot mot)
         (render-abs-maybe-intervalss-motifs scale start-pitch mot))
       (let* ([squeezed-mots (map (lambda (mot-sym) (squeeze-mot (hash-ref motifs-hash mot-sym))) mot-syms)]
              [notes (apply append (map render-canon-mot squeezed-mots))])
         (cons (scale->KeySignature scale) (rotate-list-by notes rot)))]))
  (let* ([notess (map render-canon-voice (hash-ref canons-hash can-sym))]
         [pans (voice-cnt->pan-distrib (length notess))]
         [voices (map (lambda (pan notes) (SplitStaffVoice (instr/param) pan notes)) pans notess)])
    (let ([name (symbol->string can-sym)])
      (parameterize ((score-title/param name) (file-name/param name))
        (gen-score-file (score/parameterized voices))))))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100))) (render-canon 'bounce-around-can))

;; staggered canons with voices introduced one-by-one at parameterized intervals
;; then sustained canons for N repetitions before shifts start, keep in mind 
;; different loop point for each voice for changes to spread to all voices?

(define (accum-rests rs)
  (let loop ([rests rs] [accum '()] [ret '()])
    (if (empty? rests)
        ret
        (let* ([next-accum (append accum (car rests))]
               [next-ret   (append ret (list next-accum))])
          (loop (cdr rests) next-accum next-ret)))))

(define (restsyms2rests rests-syms)
  (define (rest-sym2rest sym) `(#f () ,(list sym)))
  (map rest-sym2rest rests-syms))

(define (render-staggered-canon can-sym)
  (match (hash-ref staggered-canons-hash can-sym)
    [(list mot-syms scale start-pitch rests)
       (define (render-canon-mot mot)
         (render-abs-maybe-intervalss-motifs scale start-pitch mot))
       (let* ([mots (map (lambda (mot-sym) (hash-ref motifs-hash mot-sym)) mot-syms)]
              [squeezed-mot (apply append (map squeeze-mot mots))]
              [restss (map restsyms2rests (accum-rests rests))]
              [rests+motss (map (lambda (rests) (append rests squeezed-mot)) restss)]
              [ext-rests+motss (map (lambda (mot) (apply append mot (make-list 20 squeezed-mot))) rests+motss)]) ;; constant!
         (let* ([notess (map render-canon-mot ext-rests+motss)]
                [pans (voice-cnt->pan-distrib (length notess))] ;; or create new voice-cnt->pan-distrib that spreads 1 5 2 4 3
                [ordered-pans (list (first pans) (last pans) (second pans) (fourth pans) (third pans))]
                [voices (map (lambda (pan notes) (SplitStaffVoice (instr/param) pan notes)) ordered-pans notess)])
           (let ([name (symbol->string can-sym)])
             (parameterize ((score-title/param name) (file-name/param name))
               (gen-score-file (score/parameterized voices))))))]))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100))) (render-staggered-canon 'staggered-bounce-around-can-4))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100)) (time-signature/param (TimeSignatureSimple 15 'E))) (render-staggered-canon 'staggered-bounce-around-can-4))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100)) (time-signature/param (TimeSignatureCompound '((3 E) (3 E) (2 E) (3 E) (4 E))))) (render-staggered-canon 'staggered-bounce-around-can-4))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100)) (time-signature/param (TimeSignatureGrouping '(3 3 2 3 4) 15 'E))) (render-staggered-canon 'staggered-bounce-around-can-4))

;; next:  blend-from-back test
;; - get two motifs from motifs-hash
;; - convert to lists of note event
;; - blend-from back to generate list-of-list of music events
;; - put into a score to review
(define (test-blend scale start-pitch mot-1 mot-2)
  (let ([m1 (squeeze-mot (hash-ref motifs-hash mot-1))]
        [m2 (squeeze-mot (hash-ref motifs-hash mot-2))])
    (let ([ns1 (render-abs-maybe-intervalss-motifs scale start-pitch m1)]
          [ns2 (render-abs-maybe-intervalss-motifs scale start-pitch m2)])
      (let* ([bnss1-2 (blend-from-back ns1 ns2)]
             [qr (list (Rest 'Q))] ;; frame blend to make it easier to read
             [bnss1-2r (map (lambda (bl) (append qr bl)) bnss1-2)]
             [all-ns (append ns1 (apply append bnss1-2r) ns2)]
             [title (string-append (symbol->string mot-1) "+" (symbol->string mot-2))])
        (let ([voice (SplitStaffVoice (instr/param) 'PanCenter all-ns)])
          (parameterize ((score-title/param title) (file-name/param title))
            (gen-score-file (score/parameterized (list voice)))))))))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100))) (test-blend C-major '(C . 0va) 'bounce-down-mot-1 'bounce-up-mot))

;; abstract blend that yields
;; - one statement of mot-1
;; - blends of mot-1 with mot-2 from back
;; blend starts truncating mot-1 immediately, ends with mot-2
;; so total sequence is mot-1 (blend ...) mot-2
(define (gen-blend scale start-pitch mot-1 mot-2)
  (let ([m1 (squeeze-mot (hash-ref motifs-hash mot-1))]
        [m2 (squeeze-mot (hash-ref motifs-hash mot-2))])
    (let ([ns1 (render-abs-maybe-intervalss-motifs scale start-pitch m1)]
          [ns2 (render-abs-maybe-intervalss-motifs scale start-pitch m2)])
      (apply append (cons ns1 (blend-from-back ns1 ns2))))))

;; verify gen-blend helper blends from back (without rests)
(define (test-gen-blend scale start-pitch mot-1 mot-2)
  (let* ([notes (gen-blend scale start-pitch mot-1 mot-2)]
         [voice (SplitStaffVoice (instr/param) 'PanCenter notes)]
         [title (string-append (symbol->string mot-1) "+" (symbol->string mot-2))])
    (parameterize ((score-title/param title) (file-name/param title))
      (gen-score-file (score/parameterized (list voice))))))

;; (parameterize ((instr/param 'Marimba) (tempo/param (TempoDur 'Q 100))) (test-gen-blend C-major '(C . 0va) 'bounce-down-mot-1 'bounce-up-mot))

;; back up to render-staggered-canon:
;; - just repeats canon 20 times to get things going
;; - first and last voice are separated by 8 1/2 measures,
;;   if I start meddling with canon now, it'll take that
;;   long for first voice to catch up
;; - doesn't provide control at point after last voice joins
;;   texture, finishes first statement of canon e.g. at
;;   third eighth note in bar 11
;; - should be possible to compute that point given durations
;;   of canon and cumulative delays of each voice, e.g.
;;   * 15 eighth notes for canon or one bar less an eighth
;;   * (W W W) + (W W Q) + (W W) + (WQ) for cumulative
;;     delay makes 8W + 2Q for 
        
      

        
    
