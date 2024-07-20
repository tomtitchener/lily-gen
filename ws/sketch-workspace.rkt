#lang racket

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/generators)

;; sample weighted selection from list of motifs
;; probaby obsolete seeing as lowest level of generator
;; isn't motif but list of *intervals*
;; idea here is arbitrary long collection of elements,
;; so could take output from self-sum-product and pick
;; random count to rotate whole thing to reset pattern
;; or do something to generate an arbitrarily long list
;; of products once the full set is available?
(module+ test
  (require rackunit)
  (let* ([tups1 '((1  () (E)) (0  (Accent) (S)) (3 ()       (S)))]
         [tups2 '((-3 () (T)) (0  (Accent) (T)) (0 (Accent) (T)) (0 (Accent) (T)))]
         [tups3 '((6  () (S)) (-6 ()       (S)) (6 ()       (S)) (-6 () (S)))]
         [mots  (list tups1 tups2 tups3)]
         [wgen  (weighted-list-element/generator (map (lambda (mot) (list 1 mot)) mots))]
         [length<=? (lambda (m) (sum<=? (lambda (_) 1) m))])
    (for/list ([mot (while/generator->list (length<=? 100) wgen)])
      (check member mot mots))
    (for ((_ (in-range 100))) (check member (wgen) mots))))

;; generate fast, uniform duration, winding motif out of motif combinators
;; for some count of levels, generate core from two motifs, one transposing
;; the other, then output transposed by flipping randomly back and forth
;; between two source motifs, motifs themselves are just intervals, sequence
;; results, variation on iteration,
;; maybe going backward in the generation history to the origin,
;; maybe in retrograde

;; note these transforms operate on intervals, or really maybe-interval-or-intervals,
;; producing another maybe-interval-or-intervals,
;; whereas the current transpose routines in scale.rkt take a pitch and a
;; maybe-interval-or-intervals and produce a maybe-pitch-or-pitches/c,
;; so I need to do the same math but skip the bit about translating to a pitch

;; not entirely sure how this works: goal is input collection of maybe 5 - 10
;; 2 - 5 element interval sequences, have base level cross product of each
;; applied once l-r and r-l to start, with further iterations for more extended
;; runs

;; sketch-workspace.rkt> (for*/list ([is '((0 7 -14 0) (0 1 2) (6 -1 -2) (-6 3 -2 3 -2 3))]
;;                                   [js '((0 7 -14 0) (0 1 2) (6 -1 -2) (-6 3 -2 3 -2 3))])
;;                         (map (lambda (j) (map (lambda (i) (+ j i)) is)) js))
;; '(((0 7 -14 0) (7 14 -7 7) (-14 -7 -28 -14) (0 7 -14 0))
;;   ((0 7 -14 0) (1 8 -13 1) (2 9 -12 2))
;;   ((6 13 -8 6) (-1 6 -15 -1) (-2 5 -16 -2))
;;   ((-6 1 -20 -6) (3 10 -11 3) (-2 5 -16 -2) (3 10 -11 3) (-2 5 -16 -2) (3 10 -11 3))
;;   ((0 1 2) (7 8 9) (-14 -13 -12) (0 1 2))
;;   ((0 1 2) (1 2 3) (2 3 4))
;;   ((6 7 8) (-1 0 1) (-2 -1 0))
;;   ((-6 -5 -4) (3 4 5) (-2 -1 0) (3 4 5) (-2 -1 0) (3 4 5))
;;   ((6 -1 -2) (13 6 5) (-8 -15 -16) (6 -1 -2))
;;   ((6 -1 -2) (7 0 -1) (8 1 0))
;;   ((12 5 4) (5 -2 -3) (4 -3 -4))
;;   ((0 -7 -8) (9 2 1) (4 -3 -4) (9 2 1) (4 -3 -4) (9 2 1))
;;   ((-6 3 -2 3 -2 3) (1 10 5 10 5 10) (-20 -11 -16 -11 -16 -11) (-6 3 -2 3 -2 3))
;;   ((-6 3 -2 3 -2 3) (-5 4 -1 4 -1 4) (-4 5 0 5 0 5))
;;   ((0 9 4 9 4 9) (-7 2 -3 2 -3 2) (-8 1 -4 1 -4 1))
;;   ((-12 -3 -8 -3 -8 -3) (-3 6 1 6 1 6) (-8 1 -4 1 -4 1) (-3 6 1 6 1 6) (-8 1 -4 1 -4 1) (-3 6 1 6 1 6)))
;; 
;; (transpose/absolute C-major (scale->pitch-range-pair C-major) (cons 'C '0va) (flatten l)) ...
;;
;; next: feed this into a single-voice score just to see what it sounds like,
;; may want additional iteration, e.g. x y y or x x y

;; parameterize by
;; - bin-op with default '+'
;; - iteration count, default is 1
;;   iteration tells count of times to apply sum, e.g.
;;   ((1 2) (10 20)) -> (((2 3) (3 4)) ((11 12) (21 22)) ((11 21) (12 22)) ((20 30) (30 40)))
;;
;;        (1 2) (1 2) -> ((2 3) (3 4)) -- (((+ 1 1) (+ 2 1)) ((+ 2 1) (+ 2 2)))
;;                    (1 2) (10 20) -> ((11 12) (21 22)) 
;;                                      (10 20) (1 2) -> ((11 21) (12 22) 
;;                                                      (10 20) (10 20) -> ((20 30) (30 40)) 
;;
#;(define/contract (self-sum-product as)
  (-> (non-empty-listof (non-empty-listof exact-integer?)) (non-empty-listof (non-empty-listof (non-empty-listof exact-integer?))))
  (for*/list ([is as] [js as])
    (map (lambda (j) (map (lambda (i) ((bin-op-maybe sum-int-or-ints) j i)) is)) js)))

(define/contract (sum-int-or-ints lhs rhs)
  (-> maybe-interval-or-intervals/c maybe-interval-or-intervals/c maybe-interval-or-intervals/c)
  (cond
    [(and (list? lhs) (list? rhs))
     (map (curry + (car lhs)) rhs)]
    [(and (not (list? lhs)) (list? rhs))
     (map (curry + lhs) rhs)]
    [(and (list? lhs) (not (list? rhs)))
     (+ (car lhs) rhs)]
    [else
     (+ lhs rhs)]))

(module+ test
  (require rackunit)
  (check-equal? ((bin-op-maybe sum-int-or-ints) #f #f) #f)
  (check-equal? ((bin-op-maybe sum-int-or-ints) #f 1) #f)
  (check-equal? ((bin-op-maybe sum-int-or-ints) (list 1 2) #f) #f)
  (check-equal? ((bin-op-maybe sum-int-or-ints) (list 1 2) (list 1 2)) (list 2 3))
  (check-equal? ((bin-op-maybe sum-int-or-ints) 1 (list 1 2)) (list 2 3))
  (check-equal? ((bin-op-maybe sum-int-or-ints) (list 1 2) 2) 3))

;; handle maybe-interval-or-intervals/c vs. just eact-integer? for rests and chords
;; - (bin-op-maybe +) works for maybe part, but '+' doesn't work for non-empty-list-of interval/c
(define/contract (self-sum-product as)
  (-> (non-empty-listof (non-empty-listof maybe-interval-or-intervals/c)) (non-empty-listof (non-empty-listof (non-empty-listof maybe-interval-or-intervals/c))))
  (for*/list ([is as] [js as])
    (map (lambda (j) (map (lambda (i) ((bin-op-maybe sum-int-or-ints) j i)) is)) js)))

(module+ test
  (require rackunit)
  (check-equal?
   (self-sum-product '((0 7 -14 0) (0 1 2) (6 -1 -2) (-6 3 -2 3 -2 3)))
   '(((0 7 -14 0) (7 14 -7 7) (-14 -7 -28 -14) (0 7 -14 0))
     ((0 7 -14 0) (1 8 -13 1) (2 9 -12 2))
     ((6 13 -8 6) (-1 6 -15 -1) (-2 5 -16 -2))
     ((-6 1 -20 -6) (3 10 -11 3) (-2 5 -16 -2) (3 10 -11 3) (-2 5 -16 -2) (3 10 -11 3))
     ((0 1 2) (7 8 9) (-14 -13 -12) (0 1 2))
     ((0 1 2) (1 2 3) (2 3 4))
     ((6 7 8) (-1 0 1) (-2 -1 0))
     ((-6 -5 -4) (3 4 5) (-2 -1 0) (3 4 5) (-2 -1 0) (3 4 5))
     ((6 -1 -2) (13 6 5) (-8 -15 -16) (6 -1 -2))
     ((6 -1 -2) (7 0 -1) (8 1 0))
     ((12 5 4) (5 -2 -3) (4 -3 -4))
     ((0 -7 -8) (9 2 1) (4 -3 -4) (9 2 1) (4 -3 -4) (9 2 1))
     ((-6 3 -2 3 -2 3) (1 10 5 10 5 10) (-20 -11 -16 -11 -16 -11) (-6 3 -2 3 -2 3))
     ((-6 3 -2 3 -2 3) (-5 4 -1 4 -1 4) (-4 5 0 5 0 5))
     ((0 9 4 9 4 9) (-7 2 -3 2 -3 2) (-8 1 -4 1 -4 1))
     ((-12 -3 -8 -3 -8 -3) (-3 6 1 6 1 6) (-8 1 -4 1 -4 1) (-3 6 1 6 1 6) (-8 1 -4 1 -4 1) (-3 6 1 6 1 6)))))

;; | #f | exact-integer? | listof exact-integer?
(define self-same-mintss/param  (make-parameter '((0 7 -14 0) (0 1 2) (6 -1 -2) (-6 3 -2 3 -2 3))))

(define start-pitch/param  (make-parameter (cons 'C '0va))) ;; coordinate with scale/param

(define duration/param (make-parameter 'S))

(define (gen-ssprod-voice/param)
  (let* ([mints  (apply append (apply append (self-sum-product (self-same-mintss/param))))]
         [mpitss (transpose/absolute (scale/param) (scale-range-min-max-pair/param) (start-pitch/param) mints)]
         [norrs  (map (lambda (mpits) (if mpits
                                         (if (list? mpits)
                                             (Chord mpits (duration/param) '() #f)
                                             (Note (car mpits) (cdr mpits) (duration/param) '() #f))
                                         (Rest (duration/param)))) mpitss)])
    (SplitStaffVoice #;PitchedVoice (instr/param) 'PanCenter norrs)))

;; same total count as next
#;(parameterize ((score-title/param "self-sum-prod-1") (tempo/param (TempoDur 'Q 120)) (file-name/param "self-sum-prod-1"))
    (gen-score-file (score/parameterized (list (gen-ssprod-voice/param)))))

;; all fours
#;(parameterize ((score-title/param "self-sum-prod-2") (tempo/param (TempoDur 'Q 120)) (file-name/param "self-sum-prod-2")
                                                       (self-same-mintss/param '((0 7 -14 0) (0 1 2 4) (-1 -2 0 -1) (-6 3 -2 3))))
    (gen-score-file (score/parameterized (list (gen-ssprod-voice/param)))))

;; chords, really boring
#;(parameterize ((score-title/param "self-sum-prod-3") (tempo/param (TempoDur 'Q 120)) (file-name/param "self-sum-prod-3")
                                                       (self-same-mintss/param '((0 2 4 6) (7 5 3 1) (2 1 2 -2 -1 -2) (3 2 3 0 10 ))))
  (gen-score-file (score/parameterized (list (gen-ssprod-voice/param)))))

;; rests proof of concept
#;(parameterize ((score-title/param "self-sum-prod-4") (tempo/param (TempoDur 'Q 120)) (file-name/param "self-sum-prod-4")
                                                       (self-same-mintss/param '((1 0 #f 1) (#f 3 -3 1))))
  (gen-score-file (score/parameterized (list (gen-ssprod-voice/param)))))

;; chords proof of concept
(parameterize ((score-title/param "self-sum-prod-5") (tempo/param (TempoDur 'Q 120)) (file-name/param "self-sum-prod-5")
                                                     (self-same-mintss/param '(((1 6) (0 5) #f (1 3) (3 5) (1 3)) ((1 3) 3 #f (-1 -3) 1 #f))))
    (gen-score-file (score/parameterized (list (gen-ssprod-voice/param)))))


;; next: start adding controls
;; - Dynamic pianissimo 
;; - Sempre staccatissimo
;; - Pan within voice (may need Lilypond work)
;;
;; Uh oh:  pan setting is by staff and SplitVoice like KeyboardVoice has two staffs, "up" and "down"
;; so if I do an override, I *only* override the staff with all the notes, and the other staff stays
;; with the original pan position
;; This is hard to fix as what I want is  tracker in the other clef that spaces out the period for
;; the current clef and duplicates the changes
;; I think the only way to solve this is to write a piano voice on a single staff and just let the
;; leger lines fly in the score, which works, but needs a separate pass for showing score, which
;; won't show pan settings until I generate the annotation

;; use this as core voice to spread over choir of 8 in unison and experiment
;; with morphing pan, e.g. start PanCenter for say 30 notes, then zoom out
;; to half left/half right and back, then all left+all right and back, then
;; unique Pan per voice and back, then note-by-note follow the leader from
;; center to right and wrap back from left to center all-together before
;; reversing direction


;; integrate minimally into workspace.rkt framework
;; - start with workspace framework I can use
;;   * score/parameterized:  input voices, uses tempo/param
;;     time-signature/param, score-title/param, score-copyright/param
;;   * gen-score-file:  input Score, uses file-name/param
;;   means I need (listof voice/c), starting simple, I can use
;;   instr/param default 'AcousticGrand, then count/param for the count
;;   of voices, and SplitStaffVoice to spread notes between treble and
;;   bass
;;   -
;;   what I want is a list of intervals or even a list of list of intervals
;;   to randomly collect a series of say 30 values for my motif or maybe
;;   interval/accent pairs and a default dynamic, maybe a sempre staccato
;;   or other articulation, with for starters a fast duration or even a
;;   triple interval/accent/duration in the list to randomly choose from
;;   -
;;   to get going, just make a single-voice reference before turning it
;;   into a choir
;;

;; for lists of triples, use weighted-list-element/generator where elements
;; are triples, weights can be uniform to start, parameterized later?


;; eventually, I want to render motifs as notes, but my first thought is to experiment with
;; the pan control, which I guess is one of the (listof control/c) in a motif, so that's by
;; definition something I could add in a morpher, which already *does* rendering
;;
;; so what's a first step in the right direction of integrating with the workshop framework,
;; maybe to generate a choir of unison voices with the same pan setting so later on I can morph
;; to distribute left/right; note morph works on one voice at a time so a choir requires code
;; to morph multiple voices; so what I need is a count of voices, a list of per-voice settings
;; like scale, start-pitch, and listof control/c and then for now a single motif
;;
;; morph-motifs is built to apply successive changes to the starting initial motif up to
;; some terminating point determined by the morph routine; what I seem to be working toward
;; would be more of a two-dimensional scheme where there's one morph that applies vertically
;; to start each voice in the choir, then a list of another, per-voice morph that changes over
;; however many generations the morpher produces
;;
;; to start I'd want a vertical morph that applied the same pan setting to all voices, then
;; horizontal morphs that just repeated for some count of generations (i.e. the same morph);
;; then I'd want a new block with the same repeating horizontal morph but one that split the
;; initial pan evenly over all voices (with variations e.g. split half-and-half or by quarters,
;; all left, all right);  then a vertical morph to start all voices with the same pan but
;; horizontal morphs that staggered a L->R shifts maybe with R->L back-and-forth or L->R repeats
;; as though going in a circle note-by-note, two notes, at a time, three notes at a time, etc.
;;
;; morphers should add inital and then add per-generation annotation saying strategy like N reps
;; pan X, then generation number starting with 2
;;
;; remember this is an experiment to see what sort of effects I can get with synthesized
;; output, seeing as this wouldn't be something you could do with real musicians; all as part
;; of the idea of having a quick prototype environment for trying different textures out
;;
;; consider abstract application of morpherf as part of a second-order strategy where the
;; vertical dimension is a first-order morph of a motif to a list of motifs *without rendering*
;; then the horizontal dimension in the sense of the actual unrolling of voices with notes, is
;; a second-order morph of the original motif results as actual motifs but this time rendered as
;; voices
;;
;; raises question of value of supplying previous generation(s) of note-motifs to morpher itself,
;; though maybe first-order environment will care for starting-pitch of second-order motif and
;; params, wouldn't necessarily be a bad thing even if accumulated note-motifs never gets rendered
;;
;; so really conceptually second-order morphing comes first because what I'm interested in is
;; the intermediate motifs, which get dumped by the original implementation after being rendered

;; approach this in stages:  first have a single voice where all morph does is cycle the pan
;; setting through 9 Pan values and annotate, representing an example of a second-stage morph
;; for my trial code, each geneation is a single pitch where the list of controls has just a
;; pan value
;;
;; uh oh, note that without passing previous motif forward, I can't know what previous pan
;; setting was either, but that's easy to fix, or rather, it's not actually a problem here
;; because the morpher always knows the current motif, it just won't know all the previous
;; generations of the motifs, the same way the morpher only knows the current start-pitch
;; and scale, so if I want the morpher to work off the previous ints-motifs then I just 
;; accumulate them the same way I do with each notes-motif and expand the morpher
;; signature to take a list of ints-motifs instead of the current ints-motif, plus maybe
;; the previous scale and start-pitch
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eventually:
;;
;; - make it easy to generate and experiment with snippets e.g.
;;   realize a snippet and from that derive variations, extensions
;;
;; - make it easier to bring varied textures together vs. homogeneity
;;   of textures so far all derived from a common source
;; 
;; - draw lines in different shapes
;;
;; - accumulate lines in a choir more varied than rotation:
;;   different registers, rhythmic activity, ...
;;   different inside parameter values
;;
;; - reduce a line to chords: anticipate/echo/accumulate

;; Or consider even further abstraction so you start with atoms:
;; - a collection of short, 4-6 element interval sequences to be assembled
;;   sequentially, say one group uniformly ascends, another descends, with
;;   variants that have a 1-3 internal crests
;; - connect sequentially into a series continuing where the last left off
;;   or continue from some internal point of the sequence prior to the end,
;;   maybe implemented as one routine with an offset, maybe bounded by a range
;; - aggregate say a collection of 2-5 atoms into a new unit to be connected
;;   sequentially from larger sub-assemblies, repeated in successively larger
;;   sub-assemblies 
;; - separate durations or not? combine for now to leverage motif data and code
;;   ditto controls ... or can be derived during sequencing 

;; there's lots of flexibility for creating the next generation:
;; - transposition from the last pitch of the previous generation with the same accents and durations
;; - transposition from some interval within the first and last pitches of the previous generation
;; - inversion, retrograde, inverted retrograde of the previous generation
;; - expansion of range say by some product of each interval in the previous generation
;; - diminution of range by a division of each interval in the previous generation
;; - iteration by internal extension of intervals, repetition of pitches, rests, so the pattern
;;   covers similar ground, maybe even repeating from the same starting pitch, but is more 
;;   discursive

;; workspace could have collection of motifs for experimenting with simultaneity:
;; 
;; - noodle motifs that migrate in range slowly up/down
;; - arpeggio motifs that span large ranges but with characteristic intervals up/down
;; - derived motifs that horizontalize the first two with chords in anticipation,
;;   sustaining, or recall after the last note

;; larger scope progressions like shorter and shorter durations growing quieter and
;; quieter succeeded by highlight pitches via accent in a regular pattern where the
;; slower/accented pitches gradually supercede the quick/soft pitches
