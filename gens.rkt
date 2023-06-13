#lang racket

;; Generators

(require racket/generator)
(require "score.rkt")

(provide (all-defined-out))

(define/contract (cycle-generator lst [start 0])
  (->* ((listof any/c)) (exact-nonnegative-integer?) generator?)
  (let ([i start]
        [l (length lst)])
    (infinite-generator
     (yield (list-ref lst (remainder i l)))
     (set! i (add1 i)))))

(define/contract (generator-done? gen)
  (-> generator? boolean?)
  (symbol=? 'done (generator-state gen)))

(define/contract (generate-while pred gen)
  (-> predicate/c generator? (listof any/c))
  (let loop ()
    (let ([next (gen)])
      (if (pred next)
          (cons next (loop))
          '()))))

;; answers Note? or Rest? for length of shortest generator, then void to mark generator state 'done,
;; infinite if all generators are infinite
(define/contract (note-or-rest-gen pitch-or-f-gen durations-gen accent-or-f-gen)
  (-> generator? generator? generator? generator?)
  (infinite-generator
   (let ([pitch-or-f  (pitch-or-f-gen)]
         [duration    (durations-gen)]
         [accent-or-f (accent-or-f-gen)])
     (when (ormap generator-done? (list pitch-or-f-gen durations-gen accent-or-f-gen))
       void)
     (if pitch-or-f
         (let ([pitch    (car pitch-or-f)]
               [octave   (cdr pitch-or-f)]
               [controls (if accent-or-f (list accent-or-f) '())])
           (yield (Note pitch octave duration controls #f)))
         (yield (Rest duration))))))

#|
(define pitch-or-f-gen  (cycle-generator (list (cons 'C '0va) #f (cons 'E '0va))))
(define duration-gen    (cycle-generator '(E S S E S S)))
(define accent-or-f-gen (cycle-generator '(Accent #f #f)))
(define note-or-rest-gen (note-or-rest-gen pitch-or-f-gen duration-gen accent-or-f-gen))
(define sum-note-or-rest-durations<=? (sum<=? voice-event->duration-int note-or-rest-gen))
(define note-or-rests (generate-while note-or-rest-gen 
|#

#|
;;;;;;;;;;;;;;;;;
;;; GRAVEYARD ;;;
;;;;;;;;;;;;;;;;;

Re: random subsystem in Racket.  Want to use (random-seed (current-milliseconds)) to get started.
For recreating same number stream in a new score, save (current-milliseconds) via number->string
to seed field in score.  There's better ways in Racket to initialize the pseudo-random number
generator but I'm not sure they're worth the effort of recording a vector in the seed.

Remember also progression by increasing intensity, e.g. for period use series like fibonacci, 
e.g. 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, so the rate of change accelerates.  

So what are the controls for a given segment?
Instead of trying to dream up the full set of controls, start with minimal set.
* scale
* starting pitch 
* ending pitch 
* starting duration
* ending duration
* probabilities for ascending vs. descending
* starting count of pitches

At the production end of things, I'm going to need to take a pitch with its pitch-class and octave
and a duration to produce a Note, leaving an empty list for controls and #f for the tie field.

That'll be inside a generator that producess a list of them which together form the second input to
the SplitStaffVoice constructor.

(define ascending-thirds (repeat-list 10 (note-range C-major 'S. (cons 'C '0va) -9 15 2)))

(define ascending-thirds-voice (SplitStaffVoice 'AcousticGrand ascending-thirds))

(define ascending-descending-thirds (repeat-list 10 (inverted-note-ranges D-major 'S (cons 'D '8vb) -7 18 2)))

(define ascending-descending-thirds-voice (SplitStaffVoice 'AcousticGrand ascending-descending-thirds))
(define voices-group (VoicesGroup
                      (TempoDur 'Q 120)
                      (TimeSignatureSimple 3 'Q)
                      (list ascending-thirds-voice ascending-descending-thirds-voice)))

 (define simple-score (Score "simple" "" (list voices-group)))

;; works but only if that last remainder duration in 128th notes exactly equals one of the duration-vals
;; note the resolution doesn't go down to a single 128th note, stopping at 4, which is a 32nd note
;; so I should first safeguard that the total-dur is evenly divisible by 4 or else round it up to that
;; and somehow guarantee the remaining bits are rests
;; or maybe I'm jumping too far ahead--in the context of a time signature I'd round out adding rests 
;; to the end of the current bar
;; not only that but I'd add ties to reflect the grouping of beats inside the bar and between bars,
;; all of which I've already done in Haskell
;; the alternative would be to hold off dealing with durations for now and just leave the durations
;; as duration values to be rendered as durations later, in the context of a meter
;; a duration value should thus be just an exact-positive-integer? and computation in normal arithmetic,
;; which in turn means I accumulate tuples that look in contact syntax like
;;  (list/c (or/c pitch/c #f) exact-positive-integer? (listof Control?))
;; where #f for the first element means a Rest else the pitch and octave for a Note, exact-positive-integer?
;; is a duration value and (listof Control?) just gets plopped into the field for Note or Rest, and
;; the final tie for a Note gets computed
;;
;; the processing gets phased, with a preliminary pass given the meter to adjust the tuples with duration 
;; vals each into a Rest, a list of Rests, a Note, or a list of tied Notes so the overall type will be
;; (-> TimeSignature -> (listof (list/c (or/c pitch/c #f) exact-positive-integer? (listof Control?)))
;; 

;; note further I force the termination based on total duration, whereas I might want it to be in
;; terms of some other limit like min or max pitch or some other assessment of say the last 10 pitches
;; or durations
;; which just means I want to further customize the generator with a termination callback

;; looking over Haskell implementation it's divided into two major bits, with addEndDurs doing the
;; apportioning of durations given the time signature and current dur val, though addEndDurs is a
;; poor name because that really means to do the hard work of apportioning

;; to start, rewrite note-gen as note-tuple-gen

;; note generator governed by total duration with 
;; inner generators for pitch, duration, dynamic, accent,
;; answering #f to indicate no current value

(define/contract (note-tuple-gen total-dur dur-gen mpitch-gen mdyn-gen macc-gen)
  (-> exact-positive-integer?
      (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer?)
      (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c))
      (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c) (or/c #f dynamic?))
      (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c) (or/c #f dynamic?) (or/c #f accent?))
      (listof (list/c (or/c pitch/c #f) exact-positive-integer? (listof control/c))))
  (let inner ([ret '()]
              [cur-dur 0])
    (if (>= cur-dur total-dur)
        (reverse ret)
        (let* ([next-dur   (dur-gen total-dur cur-dur)]
               [next-mpit  (mpitch-gen total-dur cur-dur next-dur)]
               [next-mdyn  (mdyn-gen total-dur cur-dur next-dur next-mpit)]
               [next-macc  (macc-gen total-dur cur-dur next-dur next-mpit next-mdyn)]
               [next-ctrls (append (if next-mdyn (list next-mdyn) '()) (if next-macc (list next-macc) '()))]
               [next-tuple (list next-mpit next-dur next-ctrls)]
               [next-ret (cons next-tuple ret)])
          (inner next-ret (+ cur-dur next-dur))))))

;; totval in answered function has to be an exact multiple of (duration->int dur)
(define/contract (gen-mono-durval-gen dur)
  (-> duration? (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer?))
  (let ([durval (duration->int dur)])
    (lambda (totval curval)
      (when (< totval curval)
        (error 'mono-durval-gen "invalid input, tot ~e < cur ~e, caller error" totval curval))
      (when (not (zero? (remainder totval durval)))
        (error 'mono-durval-gen "invalid input, totval ~e is not evenly divisible by durval ~e" totval durval))
      (let ([remval (- totval curval)])
        (if (< remval durval) remval durval)))))

(define/contract (gen-mono-mpit-gen pit)
  (-> pitch/c (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c)))
  (lambda (totval curval durval)
    pit))

(define/contract (gen-alt-mpit-gen pit)
  (-> pitch/c (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c)))
  (let ([cnt 0])
    (lambda (totval curval durval)
      (let ([mpit (if (odd? cnt) pit #f)])
        (set! cnt (add1 cnt))
        mpit))))

(define/contract (gen-mono-mdyn-gen mdyn)
  (-> (or/c #f dynamic?) (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c) (or/c #f dynamic?)))
  (lambda (totval curval durval mpit)
    (if mpit mdyn #f)))

(define/contract (gen-mono-macc-gen macc)
  (-> (or/c #f accent?) (-> exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? (or/c #f pitch/c) (or/c #f dynamic?) (or/c #f accent?)))
  (lambda (totval curval durval mpit mdyn)
    (if mpit macc #f)))

(define note-tuples (note-tuple-gen 128 (gen-mono-durval-gen 'S) (gen-alt-mpit-gen (cons 'C '0va)) (gen-mono-mdyn-gen 'Forte) (gen-mono-macc-gen 'Accent)))
runs.rkt> note-tuples
'((#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent))
  (#f 8 ())
  ((C . 0va) 8 (Forte Accent)))

May work, but seems like overkill, and omitting time signature is an mistake.
More intuitive to have a generator that creates a note or a rest based on some window of the past note-or-rests.
That's because you wouldn't make decisions like reversing the direction of a series of pitches if you didn't know
the overall trend, or maybe also where the change falls with respect to beats in the bar.
And similarly to choose when to add a rest or what the next duration should be.
Recall earlier scheme code where I interpreted ratios to create buckets of probabilities and the random number
generator to pick from a weighted distribution of x possible choices.
Or where I used a sliding window to get a sense of the trend in a recent list of events like intervals and used
the result to bias the random selection based on history.

Also, to separate concerns, make the generator like a let-lambda that binds state over a routine that outputs just
the next iota and have the caller determine when to stop, unlike above where total duration bounds the output list.

To carry over what I was doing with Haskell I could start with a pattern generator that took lists of components and
answered a stream of note-or-rests by cycling through each.  Which could be subsumed in a parent generator that took
the same list of components and created n child generators by rotating the lists to emit a list of n note-or-rests,
one per voice that the caller would terminate by some criteria like total lengths reaching a threshold, maybe catching
up slower voices until the get close to the first.

Then maybe I could add some randomization to perturb the next-notes by e.g. a random interval from an input list.

After that I could start thinking about sliding windows for ascend/descend and rhythms that play off the meter.

;; sample
(define/contract (gen-from-cycles pitch/rests-cycle durations-cycle accent/fs-cycle)
  (-> (-> symbol? (or/c (or/c pitch/c #f) void?))
      (-> symbol? (or/c duration? void?))
      (-> symbol? (or/c (or/c accent? #f) void?))
      (-> symbol? (or/c (or/c Note? Rest?) void?)))
  (lambda (cmd)
    (let inner ([c cmd])
      (case c
        ['val  (let* ([next-pitch/rest (pitch/rests-cycle c)]
                      [next-duration   (durations-cycle c)]
                      [next-accent/f   (accent/fs-cycle c)]
                      [next-controls   (if next-accent/f (list next-accent/f) '())])
                 (if next-pitch/rest
                     (let ([pitch (car next-pitch/rest)]
                           [octave (cdr next-pitch/rest)])
                       (Note pitch octave next-duration next-controls #f))
                     (Rest next-duration)))]
        ['inc  (begin
                 (pitch/rests-cycle c)
                 (durations-cycle c)
                 (accent/fs-cycle c))]
        ['val+ (let ([val (inner 'val)])
                 (inner 'inc)
                 val)]))))

;; create a thunk from a list and an optional starting index with two commands
;; * 'val answers list-ref of remainder of current index and list length, answers any/c
;; * 'inc increments index and answers void?
;; mutable state: index advances per each 'inc infinitely
(define/contract (make-cycle-proto lst [start 0])
  (->* ((listof any/c)) (exact-nonnegative-integer?) (-> symbol? (or/c any/c void?)))
  (let ([i start]
        [l (length lst)])
    (lambda (cmd)
      (case cmd
        ['val  (list-ref lst (remainder i l))]
        ['inc  (set! i (add1 i))]
        ['val+ (let ([v (list-ref lst (remainder i l))])
                 (begin
                   (set! i (add1 i))
                   v))]))))

|#

