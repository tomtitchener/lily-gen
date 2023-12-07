#lang racket

;; iterate-workspace.rkt

(require lily-gen/lib/workspace)

(require lily-gen/lib/scale)

(require lily-gen/lib/score)

(require lily-gen/lib/score-utils)

(require (only-in seq iterate))

;; first target: try out transpose/iterate with default values as parameters

(define gens/param (make-parameter 2))

(define start-pitch/param (make-parameter (cons 'C '0va)))

(define offset/param (make-parameter 0))

(define kernel/param (make-parameter '(0 1 2 1)))

(define inits/param  (make-parameter '(2 1)))

(define shortest-dur/param (make-parameter 'SF))

;; pattern: thunks defer parameter eval letting /parameterize 
;; routines be customized inside a parameterize call

;; wrap with a thunk to defer eval of parameters 
(define/contract transpose/iterate/parameterized
  (-> (listof (listof pitch/c)))
  (thunk
   (transpose/iterate (gens/param)
                      (scale/param)
                      (scale-range-min-max-pair/param)
                      (start-pitch/param)
                      (offset/param)
                      (kernel/param)
                      (inits/param))))

;; given (length kernel) as multiplier between generations, compute uniform durations for pitches
;; for each generation as (expt kernel-length num-generation) starting from 1 e.g. for four gens,
;; the series would be 4 16 64 256 mapped by int->durations into a list of durations becomes
;; '((T) (E) (H) (W. H)) reversed as '((W. H) (H) (E) (T)) for list from lowest to highest count
;; of pitches e.g. generations '(0 1 2 3)
(define/contract simple-voices-durations/parameterized
  (-> (listof (listof duration?)))
  (thunk
   (let ([shortest-dur  (duration->int (shortest-dur/param))]
         [kernel-length (length (kernel/param))])
     (map int->durations (reverse (sequence->list (take (gens/param) (iterate (curry * kernel-length) shortest-dur))))))))

;; helpers do not refer to parameters
;; NB: for each in mpitches, output is always (length durations)
(define/contract (durs&mpits->notes-or-rests durations mpitches)
  (-> (listof duration?) (listof maybe-pitch/c) (listof (or/c Note? Rest?)))
  (flatten (map (curry ctrls-durs&mpit->notes-or-rests '() durations) mpitches)))

(define/contract transpose-iterate-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([simple-mpitchess  (transpose/iterate/parameterized)]
          [simple-durationss (simple-voices-durations/parameterized)]
          [notes-or-restss   (map durs&mpits->notes-or-rests simple-durationss simple-mpitchess)]
          [voice-eventss     (map add-key-signature/parameterized notes-or-restss)])
     (map (curry SplitStaffVoice (instr/param)) voice-eventss))))

#|

;; to generate using default parameter values:
;;   (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized)))

;; Understanding transpose/iterate:  here's the reference kernel and inits.

  (parameterize ([gens/param 4] [kernel/param '(3 2 1 -3)] [inits/param '(0 2 4 -1 -3 2)])
    (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; Length of kernel tells multiplier by (* num notes) per generation, where the unit at generation 0
;; is inits num notes so e.g. for kernel of 4 notes and inits of 6 notes, the counts goes 6 24 96 384
;; or 6 * 4^0, 6 * 4^1, 6 * 4^2, 6 * 4^3
;; Overlap voice to voice is faster and faster, generation by generation, but between two voices,
;; with 4x duration shifts per voice, a kernel of 4 notes and an inits of notes, the faster voice
;; ends half-way through the second note of the slower voice e.g. for half notes and eighth notes,
;; 1       2       3       4       5       6       
;; 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4
;; shows the four repetitions of 6 in the faster voice for one repetition of 6 in the slower voice

;; generation 0 is just inits relative to starting pitch (default <C 0va>) so C,0va E,0va G 0va, B 1vb, G 1vb, B 1vb)
;; and that's always layed out as the top voice

;; note that kernel is never heard verbatim, only as the cross product with inits:
;; unfold.rkt> (iterate-list-comprehension-sum 2 0 '(3 2 1 -3) '(0 2 4 -1 -3 2) )
;; '((0 2 4 -1 -3 2) (3 5 7 2 0 5 2 4 6 1 -1 4 1 3 5 0 -2 3 -3 -1 1 -4 -6 -1))
;;   ^            ^   ^           ^            ^             ^
;;   | kernel ....|   | inits ... |........... |............ 

;; one tricky bit to keep in mind is tranpositions are zero based, so 0 == unison, 1 == second, 2 == third, 3 == fourth, etc.

;; third generation:
;; '(6  8 10  5  3  8
;;   5  7  9  4  2  7
;;   4  6  8  3  1  6
;;   0  2  4 -1 -3  2

;;   5  7  9  4  2  7
;;   4  6  8  3  1  6
;;   3  5  7  2  0  5
;;  -1  1  3 -2 -4  1

;;   4  6  8  3  1  6
;;   3  5  7  2  0  5
;;   2  4  6  1 -1  4
;;  -2  0  2 -3 -5  0
;;                     <- this is the confusing join because the whole shifts down so fast,
;;   0  2  4 -1 -3  2     with repetition of two pitches (0 0) which almost never happens
;;  -1  1  3 -2 -4  1
;;  -2  0  2 -3 -5  0
;;  -6 -4 -2 -7 -9 -4)

;; All this to explore the basic means of operation, which produce nice, recursive patterns at
;; successive generations that, when played together in sync where the multiplier from generation
;; to generation from the kernel (e.g. with a kernel of N pitches, the length of each generation:
;;   (*  (length kernel) (length currnet generation))
;; mirrored in the difference in durations (... half, eighth, thirty-second)

;; So once you get to a kernel of 4, you're looking at a pretty big jump in the
;; durations for the voices:  (32nd:8th, 8th:2, 2:1+1 ...), at least given the way 
;; my duration generator currently works.  The advantage being that all the voices
;; have the same length and that all voices start at the beginning of the cycle and
;; end after the last note in the cycle.  Which means once you get past three or
;; maybe four voices, the earlier generations move extremely slowly. 

;; Which doesn't have to remain that way, except it would mean figuring out a ratio
;; that got you close to the same ideal but with different counts of repetitions for
;; the faster voices.

;; Explore:
;; 1) short inits 4/5 notes
;; 2) kernels that start with small intervals; 1, 2 (second, third), 
;; 3) whole-tone scale

  (parameterize ([gens/param 4] [scale/param C-whole-tone] [kernel/param '(0 2 1 0)] [inits/param '(0 2 2 1 0)])
    (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

  (parameterize ([gens/param 5] [scale/param C-whole-tone] [kernel/param '(2 0)] [inits/param '(0 2 2 1 0)])
    (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; ascending/descending:

  (parameterize
   ([time-signature/param (TimeSignature 12 'S)] [shortest-dur/param 'T] [gens/param 4] [scale/param C-whole-tone]
    [kernel/param '(0 5 1 4 3 2)] [inits/param '(0 1 -1 2 -2 0)])
      (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; really boring, six note inits is easly heard wanging around the six note transposes from kernel/param,
;; everything fitting neatly inside other voices
;;
;; diverse lengths:

  (parameterize ([time-signature/param 12 'S] [start-pitch/param (cons 'Ef '0va)] [shortest-dur/param 'T]
                [gens/param 4] [scale/param Ef-major] [kernel/param '(0 5 -4)] [inits/param '(0 1 2 1)])
      (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; this gets play between 4 unit repetitions of inits vs. 3 unit transpositions from kernel, which plays out in 3x duration
;; relation between voices so voice 4 moves 3x faster than voice 3 and etc. for voices 2 and 1 while inits unit is 4 notes
;; so the faster voice fits in three reptitions of inits compared to 1 statement for the slower voice, something that's really
;; only audible in the last two (fastest) voices because the first move too slowly

  (parameterize ([gens/param 4] [scale/param Ef-major] [start-pitch/param (cons 'Ef '0va)] [kernel/param '(0 6 1 2)]
                 [inits/param '(-4 -7 -2)] [shortest-dur/param 'T])
      (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; this gets kernel:inits ratio of 4:3 to show syncopation between pattern (inits) and transpositions (kernel)
;; also, with shortest-dur/param of 'T, doesn't go romping off to 'SF by default
;;
;; example of recreating from just midi file which turned out to be a morning's worth of effort
;; easy enough to recreate key signature, but needed to remember starting pitch to get inits first
;; from first voice, then kernel by count of repetitions from second voice
;;
;; what about a 2x rhythmic ratio between voices via a kernel and a long inits:

 (parameterize ([gens/param 4] [scale/param Ef-major] [start-pitch/param (cons 'Ef '0va)] 
                [kernel/param '(0 3)] [inits/param '(-1 0 3 3 2 2 6 -2)] [shortest-dur/param 'T])
      (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; gets you a frozen-while-flying or stroboscopic effect with each voice going 2x faster than next voice, 
;; a simultaneous slo-mo

  (parameterize ([gens/param 4] [scale/param Ef-major] [start-pitch/param (cons 'Ef '0va)]
                 [kernel/param '(0 3 4 2)] [inits/param '(-1 0 3 3 2 2 6 -2)] [shortest-dur/param 'T])
      (gen-score-file (score/parameterized (transpose-iterate-voices/parameterized))))

;; regularity of rhythmic proportions between voices gets stale really fast
;; slower voices slow down really fast
;; application of self-similarity seems too simple-minded

|#
