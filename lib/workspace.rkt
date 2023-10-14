#lang racket

;; workspace.rkt:  experiment with routines

;; nothing to provide, this is a stub

(require (only-in seq iterate take))
(require (only-in algorithms repeat))
(require (only-in srfi/1 list-index))

(require lily-gen/lib/utils)
(require lily-gen/lib/score)
(require lily-gen/lib/score-utils)
(require lily-gen/lib/scale)
(require lily-gen/lib/lily)
(require lily-gen/lib/generators)

;; first target:  transpose/iterate

;; transpose/iterate with default values as parameters

(define gens-param (make-parameter 2))

(define scale-param (make-parameter C-major))

(define (scale->KeySignature scale)
  (let-values ([(tonic mode) (scale->key-signature-values scale)])
    (KeySignature tonic mode)))

(define key-signature (thunk (scale->KeySignature (scale-param))))

(define scale-range-min-max-pair-param (thunk (scale->pitch-range-pair (scale-param))))

(define start-pitch-param (make-parameter (cons 'C '0va)))

(define offset-param (make-parameter 0))

(define kernel-param (make-parameter '(0 1 2 1)))

(define inits-param  (make-parameter '(2 1)))

(define instr-param  (make-parameter 'AcousticGrand))

(define file-name-param (make-parameter "test"))

(define tempo-param (make-parameter (TempoDur 'Q 60)))

;; override when (length (inits-param)) is 3, 5, etc, to unit or multiples
(define beats-per-bar-param (make-parameter 4))

;; for when (length (kernel-param)) is even, when it's 3, 5, etc, make this 'SF or multiples of 'SF
(define duration-per-beat-param (make-parameter 'Q))

(define time-signature (thunk (TimeSignatureSimple (beats-per-bar-param) (duration-per-beat-param))))

(define score-title-param (make-parameter "workspace"))

(define score-copyright-param (make-parameter "copyright"))

(define shortest-dur-param (make-parameter 'SF))

;; wrap this with a let setting overrides to the default param vals above
(define/contract transpose/iterate/parameterized
  (-> (listof (listof pitch/c)))
  (thunk
   (transpose/iterate (gens-param)
                      (scale-param)
                      (scale-range-min-max-pair-param)
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
   (let ([shortest-dur  (duration->int (shortest-dur-param))]
         [kernel-length (length (kernel-param))])
     (map int->durations (reverse (sequence->list (take (gens-param) (iterate (curry * kernel-length) shortest-dur))))))))

(define/contract (durs&mpit->notes-or-rests durs mpitch)
  (-> (listof duration?) maybe-pitch/c (or/c (listof Note?) (listof Rest?)))
  (match mpitch
    [(cons _ _)
     (ctrls-durs&pit->notes '() durs mpitch)]
     [#f
      (map Rest durs)]))

(define/contract (durs&pits->notes-or-rests durations mpitches)
  (-> (listof duration?) (listof maybe-pitch/c) (listof (or/c Note? Rest?)))
  (flatten (map (curry durs&mpit->notes-or-rests durations) mpitches)))

;; deprecated with choice of SplitStaff voice
(define/contract (add-clefs clef voice-events)
  (-> clef? (listof voice-event/c) (listof voice-event/c))
  (add-bass-or-treble-clefs-to-voice-events voice-events clef))

(define/contract (add-key-signature voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
  (cons (key-signature) voice-events))

(define/contract (add-clefs&key-signature voice-events)
  (-> (listof voice-event/c) (listof voice-event/c))
    (add-clefs 'Treble (add-key-signature voice-events)))

(define/contract simple-iterated-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([simple-mpitchess  (transpose/iterate/parameterized)]
          [simple-durationss (simple-voices-durations/parameterized)]
          [notes-or-restss   (map durs&pits->notes-or-rests simple-durationss simple-mpitchess)]
          [voice-eventss     (map add-key-signature notes-or-restss)])
     (map (curry SplitStaffVoice (instr-param)) voice-eventss))))

(define/contract (voices-group/parameterized voices)
  (-> (listof voice/c) VoicesGroup?)
  (VoicesGroup (tempo-param) (time-signature) voices))

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

;; Understanding transpose/iterate:  here's the reference kernel and inits.

;; (parameterize ((gens-param 4) (kernel-param '(3 2 1 -3)) (inits-param '(0 2 4 -1 -3 2))) (gen-score-file/parameterized))

;; Length of kernel tells multiplier by (* num notes) per generation, where the unit at generation 0
;; is inits num notes so e.g. for kernel of 4 notes and inits of 6 notes, the counts goes 6 24 96 384
;; or 6 * 4^0, 6 * 4^1, 6 * 4^2, 6 * 4^3
;; Overlap voice to voice is faster and faster, generation by generation, but between two voices,
;; with 4x duration shifts per voice, a kernel of 4 notes and an inits of notes, the faster voice
;; ends half-way through the second note of the slower voice e.g. for half notes and eighth notes,
;; 1       2       3       4       5       6       
;; 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4
;; shows the four repetitions of 6 in the faster voice for one repetition of 6 in the slower voice

;; (parameterize ((gens-param 4) (kernel-param '(3 2 1 -3)) (inits-param '(0 2 4 -1 -3 2))) (gen-score-file/parameterized))

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
;;
;; (parameterize ((gens-param 4) (scale-param C-whole-tone) (kernel-param '(0 2 1 0)) (inits-param '(0 2 2 1 0))) (gen-score-file/parameterized))
;; (parameterize ((gens-param 5) (scale-param C-whole-tone) (kernel-param '(2 0)) (inits-param '(0 2 2 1 0))) (gen-score-file/parameterized))


;; ascending/descending:
;;
;; (parameterize ((beats-per-bar-param 12) (duration-per-beat-param 'S) (shortest-dur-param 'T) (gens-param 4) (scale-param C-whole-tone)
;;   (kernel-param '(0 5 1 4 3 2)) (inits-param '(0 1 -1 2 -2 0))) (gen-score-file/parameterized))
;;
;; really boring, six note inits is easly heard wanging around the six note transposes from kernel-param,
;; everything fitting neatly inside other voices
;;
;; diverse lengths:
;; 
;; (parameterize ((beats-per-bar-param 12) (duration-per-beat-param 'S) (start-pitch-param (cons 'Ef '0va)) (shortest-dur-param 'T)
;;   (gens-param 4) (scale-param Ef-major) (kernel-param '(0 5 -4)) (inits-param '(0 1 2 1))) (gen-score-file/parameterized))
;;
;; this gets play between 4 unit repetitions of inits vs. 3 unit transpositions from kernel, which plays out in 3x duration
;; relation between voices so voice 4 moves 3x faster than voice 3 and etc. for voices 2 and 1 while inits unit is 4 notes
;; so the faster voice fits in three reptitions of inits compared to 1 statement for the slower voice, something that's really
;; only audible in the last two (fastest) voices because the first move too slowly
;;
;;(parameterize ((gens-param 4) (scale-param Ef-major) (start-pitch-param (cons 'Ef '0va)) (kernel-param '(0 6 1 2))
;;(inits-param '(-4 -7 -2)) (shortest-dur-param 'T)) (gen-score-file/parameterized))
;;
;; this gets kernel:inits ratio of 4:3 to show syncopation between pattern (inits) and transpositions (kernel)
;; also, with shortest-dur-param of 'T, doesn't go romping off to 'SF by default
;;
;; example of recreating from just midi file which turned out to be a morning's worth of effort
;; easy enough to recreate key signature, but needed to remember starting pitch to get inits first
;; from first voice, then kernel by count of repetitions from second voice
;;
;; what about a 2x rhythmic ratio between voices via a kernel and a long inits:
;;
;; (parameterize ((gens-param 4) (scale-param Ef-major) (start-pitch-param (cons 'Ef '0va)) (kernel-param '(0 3))
;; (inits-param '(-1 0 3 3 2 2 6 -2)) (shortest-dur-param 'T)) (gen-score-file/parameterized))
;;
;; gets you a frozen-while-flying or stroboscopic effect with each voice going 2x faster than next voice, 
;; a simultaneous slo-mo
;;
;; (parameterize ((gens-param 4) (scale-param Ef-major) (start-pitch-param (cons 'Ef '0va)) (kernel-param '(0 3 4 2))
;; (inits-param '(-1 0 3 3 2 2 6 -2)) (shortest-dur-param 'T)) (gen-score-file/parameterized))
;;

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;

;; Next: experiment with octatonic scales and generators, forgetting about parameterization for now.
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
;; problem is that with stacked generators I'll get repeats of #f which is not what I want, or is it?
;; 
;; generators.rkt> (define ig (sequence->generator '(1 2 3 4 5 6 7 8 9)))
;; generators.rkt> (define mg (weighted-maybe-generator '(1 1) ig))
;; generators.rkt> (define og (weighted-repeats-generator '(1 1 1) '(1 2 3)) mg)
;; generators.rkt> (og)
;; '(#f #f #f)
;; generators.rkt> (og)
;; '(#f #f #f)
;; generators.rkt> (og)
;; '(3 3 3)
;; generators.rkt> (og)
;; '(#f #f)
;; generators.rkt> (og)
;; '(#f #f)
;; generators.rkt> (og)
;; '(6 6 6)
;; generators.rkt> (og)
;; '(7 7)
;; generators.rkt> (og)
;; '(#f #f)
;; generators.rkt> (og)
;; '(9 9)
;;
;; Outer generator keeps scale constant and progresses range and span from narrow/high to wide/low or reverse
;;
;; What is output from combined generators?  Note that generator-generator keeps calling inner-gen until it's
;; done, then starts new inner-gen with next result from outer-gen.
;;
;; Just work through them one-by-one first.
;;
;; a) random sequence descending seconds and thirds
;;    - inputs: weights, intervals, scale, pitch-range-pair/c
;;    - output: pitch/c until 'done
;;    - impl:   generate-while
;;              * pred tests if (gen) is within range
;;              * gen:
;;                - inputs: weights, intervals, scale, pitch/c (high)
;;                - state:  
;;                    static:  buckets
;;                    dynamic: previously-answered pitch/c
;;                - impl:
;;                    * pick bucket for random, interval for bucket
;;                    * answer transposition of previous pitch with new interval
;;                          
;; see weighted-list-intervals-generator, this is level 0
;;
;; 
(define C-whole-oct (octatonic-whole-scale 'C))
;;
(define ints-g (weighted-intervals-generator '(5 1) '(-1 -2) C-whole-oct (cons (cons 'C '22va) (cons 'C '22vb))))
;;
(define may-g (weighted-maybe-generator '(2 1) ints-g))
;;
(define reps-g (weighted-repeats-generator '(15 5 1) '(1 2 3) may-g))
;;
;; (generate-while (const #t) reps-g)
;;
;; experimental mapping from (non-empty-listof (or/c #f pitch/c)) -> (non-empty-listof (or/c Rest? Note?))
;; - #f -> Rest
;; - pitch/c -> Note
;; question is duration and (for Note) accent
;; - for list of Note, pick a duration, more elements -> shorter the duration, pick randomly from list
;;   for different lengths
;; - do the same for list of Rest
;;
;; might as well be a generator-map with function
;; (-> (non-empty-listof (or/c #f pitch/c)) (non-empty-listof (or/c Rest? Note?)))
;; that takes weights and durations lists
(define/contract (fs-or-pitches->rests-or-notes weights durations)
  (-> (non-empty-listof exact-positive-integer?) ;; weights
      (non-empty-listof duration?) ;; durations, must be same lengths
      (-> (non-empty-listof (or/c #f pitch/c)) (non-empty-listof (or/c Rest? Note?))))
  (unless (= (length weights) (length durations))
    (error 'fs-or-pitches->rests-or-notes "unequal lengths of weights ~v and durations ~v" weights durations))
  (let* ([buckets (gen-buckets weights)])
    (lambda (fs-or-pitches)
     (let* ([r (random)]
            [ix (list-index (lambda (bucket) (<= r bucket)) buckets)]
            [dur (list-ref durations ix)])
      (match (car fs-or-pitches)
        [#f
         (repeat (length fs-or-pitches) (Rest dur))]
        [(cons p o)
         (repeat (length fs-or-pitches) (Note p o dur '() #f))])))))
;;
(define ves-g (generator-map (fs-or-pitches->rests-or-notes '(8 2 1) '(S E Q)) reps-g))
