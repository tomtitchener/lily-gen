#lang racket

;; workspace.rkt:  experiment with REPL using parameterization

;; nothing to provide, this is a stub to play with using the repl

(require racket/generator)

(require (only-in seq iterate take))
(require (only-in algorithms repeat))

(require lily-gen/lib/lily)
(require lily-gen/lib/utils)
(require lily-gen/lib/meter)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/score-utils)
(require lily-gen/lib/scale)
(require lily-gen/lib/generators)

;; general-purpose params, common to specialized generators

(define scale/param (make-parameter C-major))

(define (scale->KeySignature scale)
  (let-values ([(tonic mode) (scale->key-signature-values scale)])
    (KeySignature tonic mode)))

(define key-signature/param (make-derived-parameter scale/param identity scale->KeySignature))

;; Hack!  Calling scale->pitch-range-pair in scale.rkt causes a memory access failure.  Why?
;; Something to do with contracts?
(define (loc-scale->pitch-range-pair scale)
  (-> Scale? pitch-range-pair/c)
  (cons (index->pitch scale 0) (index->pitch scale (scale->max-idx scale))))

(define scale-range-min-max-pair/param (make-derived-parameter scale/param identity loc-scale->pitch-range-pair))

(define cnt/param  (make-parameter 10))

(define instr/param  (make-parameter 'AcousticGrand))

(define file-name/param (make-parameter "test"))

(define tempo/param (make-parameter (TempoDur 'Q 60)))

(define time-signature/param (make-parameter (TimeSignatureSimple 4 'Q)))

(define score-title/param (make-parameter "workspace"))

(define score-copyright/param (make-parameter "copyright"))

;; Not for use with SplitStaff voices
(define/contract (add-clefs clef voice-events)
  (-> clef? (listof voice-event/c) (listof voice-event/c))
  (add-bass-or-treble-clefs-to-voice-events voice-events clef))

;; to use this, provide list of voice/c, consumes tempo/param, time-signature/param,
;; score-title/param, score-copyright/param
(define/contract (score/parameterized voices)
  (-> (listof voice/c) Score?)
  (let* ([voices-group (VoicesGroup (tempo/param) (time-signature/param) voices)]
         [extended&aligned-voices-group (extend&align-voices-group-durations voices-group)])
    (Score (score-title/param) (score-copyright/param) (list extended&aligned-voices-group))))

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

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;

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

(define/contract (add-key-signature/parameterized voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
  (cons (key-signature/param) voice-events))

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

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;

;; Next: experiment with octatonic, whole-tone, and mixed diatonic scales
;; input to generator of randomly weighted descending intervals, durations and repeats.
;;
;; Inner pattern generator takes scale and high and low pitches for range.
;; Implement nested generators 
;; 
;; a) level 0 is random sequence of descending seconds and thirds, weighted toward seconds over the
;;    span of two octaves with the starting pitch silent so you don't repeat from the same pitch
;;    -- this is a new generator:  weighted-intervals-generator
;; b) level 1 is map that takes level 0 output and passes it along or replaces it with val or
;;    #f to be intepreted later as a rest
;;    -- this is weighted-maybe-generator
;; c) level 2 is either/or filter that takes level 1 output and replaces with 1..5 repetitions
;;    randomly weighted toward 1
;;    -- this is weighted-repeats-generator
;;

(define C-whole-oct (octatonic-whole-scale 'C))

(define/contract (dur&fs-or-pitches->rests-or-notes dur fs-or-pitches)
  (-> duration? (non-empty-listof (or/c #f pitch/c)) (non-empty-listof (or/c Rest? Note?)))
  (let ([cnt (length fs-or-pitches)])
    (match (car fs-or-pitches)
      [#f         (repeat cnt (Rest dur))]
      [(cons p o) (repeat cnt (Note p o dur '() #f))])))

(define repeat/c 
  (make-flat-contract #:name 'repeat/c #:first-order exact-positive-integer?))

(define weight&repeatss/c
  (make-flat-contract #:name 'weight&repeatss/c #:first-order (non-empty-listof (list/c relative-weight/c repeat/c))))

(define weight&durationss/c
  (make-flat-contract #:name 'weight&durationss/c #:first-order (non-empty-listof (list/c relative-weight/c duration?))))

(define piano/param (make-parameter 'AcousticGrand))

(define high-pitch-range/param (make-parameter (cons (cons 'C '15va) (cons 'C '15vb))))

(define high-start-pitch/param (make-parameter (cons 'C '8va)))

(define mid-pitch-range/param (make-parameter (cons (cons 'C '15va) (cons 'C '15vb))))

(define mid-start-pitch/param (make-parameter (cons 'Gs '0va)))

(define low-pitch-range/param (make-parameter (cons (cons 'C '15va) (cons 'C '15vb))))

(define low-start-pitch/param (make-parameter (cons 'C '0va)))

(define descending-weight&intervalss/param (make-parameter (list (list 5 -1) (list 1 -2))))

(define weights/rests/param (make-parameter (cons 5 1)))

(define weight&repeat-prs/param (make-parameter (list (list 15 1) (list 5 2) (list 1 3))))

(define weight&duration-prs/param (make-parameter (list (list 8 'S) (list 2 'E) (list 1 'Q))))

(struct/contract VoiceParams ([instr               instr?]
                              [scale               Scale?]
                              [start-pitch         pitch/c]
                              [pitch-range         pitch-range-pair/c]
                              [weight&intervalss   weight&intervalss/c]
                              [weights/rests       (cons/c relative-weight/c relative-weight/c)]
                              [weight&repeat-prs   weight&repeatss/c]
                              [weight&duration-prs weight&durationss/c]))

(define (voiceparam-values vps)
  (values (VoiceParams-instr vps)
          (VoiceParams-scale vps)
          (VoiceParams-start-pitch vps)
          (VoiceParams-pitch-range vps)
          (VoiceParams-weight&intervalss vps)
          (VoiceParams-weights/rests vps)
          (VoiceParams-weight&repeat-prs vps)
          (VoiceParams-weight&duration-prs vps)))

(define/contract (voice-events->generator/VoiceParams voiceparams)
  (-> VoiceParams? generator?)
  (let-values ([(_ scale start-pitch pitch-range weight&intervalss weights/rests weight&repeats weight&durations) (voiceparam-values voiceparams)])
    (let* ([pitch-gen         (weighted-intervals->pitches/generator scale pitch-range start-pitch weight&intervalss)]
           [pitch-or-f-gen    (weighted-maybe/generator weights/rests pitch-gen)]
           [cnt-gen           (weighted-list-element/generator weight&repeats)]
           [pitches-or-fs-gen (combine-generators/generator repeat cnt-gen pitch-or-f-gen)]
           [dur-gen           (weighted-list-element/generator weight&durations)])
      (combine-generators/generator dur&fs-or-pitches->rests-or-notes dur-gen pitches-or-fs-gen))))

(define voice-high/param
  (thunk (VoiceParams (piano/param)
                      (scale/param)
                      (high-start-pitch/param)
                      (high-pitch-range/param)
                      (descending-weight&intervalss/param)
                      (weights/rests/param)
                      (weight&repeat-prs/param)
                      (weight&duration-prs/param))))

(define voice-mid/param
  (thunk (VoiceParams (piano/param)
                      (scale/param)
                      (mid-start-pitch/param)
                      (mid-pitch-range/param)
                      (descending-weight&intervalss/param)
                      (weights/rests/param)
                      (weight&repeat-prs/param)
                      (weight&duration-prs/param))))

(define voice-low/param
  (thunk (VoiceParams (piano/param)
                      (scale/param)
                      (low-start-pitch/param)
                      (low-pitch-range/param)
                      (descending-weight&intervalss/param)
                      (weights/rests/param)
                      (weight&repeat-prs/param)
                      (weight&duration-prs/param))))

(define voices-params/param (thunk (list (voice-high/param) (voice-mid/param) (voice-low/param))))

(define/contract voice-param-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([notes-or-restss-gens (map voice-events->generator/VoiceParams (voices-params/param))]
          [notes-or-rests       (map (lambda (gen) (apply append (while/generator->list (const #t) gen))) notes-or-restss-gens)]
          [voice-eventss        (map add-key-signature/parameterized notes-or-rests)]
          [instrs               (map VoiceParams-instr (voices-params/param))])
     (map (lambda (instr ves) (SplitStaffVoice instr ves)) instrs voice-eventss))))

#|

;; It all works.  But the result, when customized to span a longer stretch than this
;; example, is sort of chaotic, with voices sharing the most general of features like the
;; overall descending dirction, the intermittent repetition.  It's the usual problem where
;; I take too atomized an approach, manipulating the lowest-level components, randomizing
;; at a level too abstract for any recognizable organization.

(parameterize
  ((scale/param                        C-whole-tone)
   (descending-weight&intervalss/param (list (list 1 -1) (list 2 1) (list 1 -2)))
   (high-start-pitch/param             (cons 'E '8va))
   (mid-start-pitch/param              (cons 'C '8va))
   (low-start-pitch/param              (cons 'As '8va)))
  (gen-score-file (score/parameterized (voice-param-voices/parameterized))))

|#

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;

;; next target: weighted-motifs/generator
;; (-> Scale? pitch/c weight&maybe-interval-motifss/c generator?)

(struct/contract VoiceParams2 ([instr       instr?]
                               [scale       Scale?]
                               [start-pitch pitch/c]
                               [motifs      weight&maybe-intervals-motifs/c]))

(define (voiceparam2-values vps)
  (values (VoiceParams2-instr vps)
          (VoiceParams2-scale vps)
          (VoiceParams2-start-pitch vps)
          (VoiceParams2-motifs vps)))

(define/contract (voice-events->generator/VoiceParams2 voice2params)
  (-> VoiceParams2? generator?)
  (let-values ([(_ scale start-pitch motifs) (voiceparam2-values voice2params)])
    (weighted-motifs/generator scale start-pitch motifs)))

(define weight&maybe-interval-motifs/param
  (thunk
   (list (list 1 (list (list 1 '(Accent) '(E.)) (list 0 '() '(S)) (list -1 '() '(E)) (list #f '() '(E))))                    ;; ends one same step
         (list 1 (list (list -1 '() '(S)) (list 1 '() '(S)) (list 1 '(Accent) '(E))))                                        ;; ends up one step
         (list 1 (list (list -3 '() '(S)) (list 0 '() '(E)) (list 0 '() '(S)) (list 1 '(Accent) '(E)) (list 2 '() '(E))))))) ;; ends on same step

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
          [notes-or-restss      (map (lambda (gen) (apply append (while/generator->list (sum<=? (const 1) (cnt/param)) gen))) notes-or-restss/gens)]
          [voice-eventss        (map add-key-signature/parameterized notes-or-restss)]
          [instrs               (map VoiceParams2-instr (voices2-params/param))])
     (map (lambda (instr ves) (SplitStaffVoice instr ves)) instrs voice-eventss))))

#|

(parameterize
  ((scale/param C-whole-tone))
   (cnt/param 20)
  (gen-score-file (score/parameterized (voice-param2-voices/parameterized))))

That's easier to listen to.  And with three voices you get random rhythmic synchronization 

|#

;; 
