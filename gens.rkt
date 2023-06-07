#lang racket

;; Experimental

(require "utils.rkt")
(require "scale.rkt")
(require "score.rkt")
(require "score-syms.rkt")
(require (only-in "lily-utils.rkt" duration->int))
(require srfi/1)
(require (only-in algorithms repeat))

#|

Example texture using runs or patterns of consecutive intervals using indices
mapped into scales, octave pitch pairs across series of scales, matched with
list of repeated durations to generate simple notes.

For diatonic scales, with seven notes, index range given count of octaves
is from 0 to 55 (are outer two octaves too ugly to listen to?):

scale.rkt> (idx->pitch C-major 0)
'(C . 29vb)
scale.rkt> (idx->pitch C-major 55)
'(B . 22va)

Note these indices are normalized by C octave ranges:

scale.rkt> (idx->pitch B-major 0)
'(Cs . 29vb)
scale.rkt> (idx->pitch B-major 55)
'(B . 22va)

The bottom and top indices are the scale pitches closest to C and B enharmonically:

scale.rkt> (idx->pitch Dff-major 0)
'(Dff . 29vb)
scale.rkt> (idx->pitch Dff-major 55)
'(Cf . 22va)

If you split a range between two scales, you get successive scale degrees (range is half-open):

scale.rkt> (list (map ((curry idx->pitch) A-major) (range 0 5)) (map ((curry idx->pitch) Af-major) (range 5 10)))
'(((Cs . 29vb) (D . 29vb) (E . 29vb) (Fs . 29vb) (Gs . 29vb))
  ((Af . 29vb) (Bf . 29vb) (C . 22vb) (Df . 22vb) (Ef . 22vb)))

Piano range is three octaves below middle C and four octaves above middle C for a total of 7 octaves,
plus a couple pitches on the low end down to the A below C.  Octave range from 29vb to 22va is an eight
octave range.  The confusing thing is the default, no-annotation octave for Lilypond is the octave below
middle C.  So the eight symbols denote the range that corresponds to the four octaves below middle C and
the four octaves above middle C.  The ordinary piano keyboard cuts that bottom octave at the low A, and
in fact the tonal quality in the lowest and highest octaves is diminished, from very soft hardly audible
on the low end to piercing and shrill on the high end, at least for the synth instruments that currently
interest me.

If I limit myself to five sharps up and five flats down, I have for choice of keys the range
Df Af Ef Bf F C G D A E B for a total of 11.  So to enumerate I have a space of 56 pitches low 
to high by 11 keys flat to sharp or a total of 616 points in a cartesian plane, though the
range of keys could be enumerated by intervals other than a fifth, say a fourth or a third.

Or that's one way of thinking of things.  Without organization of pitches by affinity, accent,
reptition or some other emphasis, a given diatonic scale could be heard as major, minor or any
of the modal scales.  

My starting point was a texture of overlapping ascending, mainly stepwise motifs, with voices joining
in a choir to generate a sustained texture.  The motifs would start in mid-low range, in the second
octave below middle C, with longer durations starting with small runs of four or five notes, hopping
up or down a second, third, or fourth from the ending pitch to start the next run, gradually getting
shorter/faster and including more notes in a run, extending the range for a given run.

Or something like that.  A first step would be just to have a generator with some random behavior
built into it that I could parameterize, and get fancier as I go along.

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

|#

;; A simple pitch range into a list of notes of equal duration, no rests.
(define/contract (note-range scale duration pitch start stop [step 1])
  (->* (Scale? duration? pitch/c integer? integer?) (integer?) (listof Note?))
  (let ([pitches (transpose/absolute scale pitch (range start stop step))])
    (map (lambda (p) (Note (car p) (cdr p) duration '() #f)) pitches)))

(define/contract (inverted-note-ranges scale duration pitch start stop [step 1])
  (->* (Scale? duration? pitch/c integer? integer?) (integer?) (listof Note?))
  (let ([beginning (note-range scale duration pitch start stop step)]
        [ending    (note-range scale duration pitch stop start (- step))])
    (append beginning ending)))


(define/contract (repeat-list n lst)
  (-> exact-nonnegative-integer? list? list?)
  (apply append (repeat n lst)))

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

#|
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
|#


#|
;;;;;;;;;;;;;;;;;
;;; Graveyard ;;;
;;;;;;;;;;;;;;;;;
;; note that successive generators could pick up on the
;; values generated so far, so for the duration generator,
;; it'd just know about the overall total and the total
;; so far,
;; then the pitch generator would know about those two
;; totals plus the current duration,
;; and the dynamic generator would add the current pitch,
;; and the accent generator would add the current dynamic

;; put the dynamic generator last?
;; remember pitch, dynamic, and accent could be #f

(define/contract (note-gen total-dur dur-gen pitch-gen dyn-gen acc-gen)
  (-> exact-positive-integer?
      (-> exact-positive-integer? exact-nonnegative-integer? duration?)
      (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c))
      (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c) (or/c #f dynamic?))
      (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c) (or/c #f dynamic?) (or/c #f accent?))
      (listof Note?))
  (let inner ([ret '()]
              [cur-dur 0])
    (if (>= cur-dur total-dur)
        (reverse ret)
        (let* ([next-dur   (dur-gen total-dur cur-dur)]
               [next-pit   (pitch-gen total-dur cur-dur next-dur)]
               [next-dyn   (dyn-gen total-dur cur-dur next-dur next-pit)]
               [next-acc   (acc-gen total-dur cur-dur next-dur next-pit next-dyn)]
               [next-ctrls (append (if next-dyn (list next-dyn) '()) (if next-acc (list next-acc) '()))]
               [next-note (if next-pit
                              (Note (car next-pit) (cdr next-pit) next-dur next-ctrls #f)
                              (Rest next-dur next-ctrls))]
               [next-ret (cons next-note ret)])
          (inner next-ret (+ cur-dur (duration->int next-dur)))))))

(define/contract (gen-mono-dur-gen dur)
  (-> duration? (-> exact-positive-integer? exact-nonnegative-integer? duration?))
  (lambda (tot cur)
    (when (< tot cur)
        (error 'mono-dur "invalid input, tot ~e < cur ~e" tot cur))
    (let ([rem (- tot cur)])
      (if (< rem (duration->int dur)) (int->duration rem) dur))))

(define/contract (gen-mono-pit-gen pit)
  (-> pitch/c (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c)))
  (lambda (tot cur dur)
    pit))

(define/contract (gen-mono-dyn-gen dyn)
  (-> dynamic? (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c) (or/c #f dynamic?)))
  (lambda (tot cur dur pit)
    #f))

(define/contract (gen-mono-acc-gen acc)
  (-> accent? (-> exact-positive-integer? exact-nonnegative-integer? duration? (or/c #f pitch/c) (or/c #f dynamic?) (or/c #f accent?)))
  (lambda (tot cur dur pit dyn)
    #f))

(define notes (note-gen 128 (gen-mono-dur-gen 'E) (gen-mono-pit-gen (cons 'C '0va)) (gen-mono-dyn-gen 'Forte) (gen-mono-acc-gen 'Accent)))


        ;; convert timeSig, curLen, addLen into a list of tuples per group being careful to allow for
        ;; overlap of curLen into beginning of timeSig, then call inner-add-end-durs for each
        ;; * timeSig into (list timeSig) by groups e.g. 7/8 grouped as 2 2 3 becomes (2/8 2/8 3/8)
        ;; * curLen into (list curLen)
        ;; * addLen into (list addLen)
        [(TimeSignatureGrouping? timeSig)
         (let* ([groups    (TimeSignatureGrouping-groups timeSig)]
                [num       (TimeSignatureGrouping-num timeSig)]
                [denom     (TimeSignatureGrouping-denom timeSig)]
                [timeSigs  (map (lambda (num) TimeSignatureSimple num denom) groups)]
                [beatLen   (duration->int denom)]
                [barLen    (* num beatLen)]
                [groupLens (map (lambda (groupLen) (* groupLen beatLen)) groups)]
                [groupSums (scanl + groupLens)]
                [spillOver (remainder curLen barLen)])
           (let inner ([ix (list-index (lambda (gs) (> gs spillOver)) groupSums)]
[remAddLen

(define/contract (curlen->addlen timesig curlen)
  (-> time-signature/c exact-positive-integer? exact-positive-integer?)
  (match timesig
    [(TimeSignatureSimple num denom) (min curlen (* num (duration->int denom)))]
    [(TimeSignatureGrouping _ num denom) (min curlen (* num (duration->int denom)))]
    [else (error curlen->addlen "unrecognized time signature ~v" timesig)]))

(define/contract (timesig-num timesig)
  (-> time-signature/c exact-positive-integer?)
  (match timesig
    [(TimeSignatureSimple num denom) num]
    [(TimeSignatureGrouping groups num denom) num]
    [else (error timesig-num "timesig-num unrecognized time signature ~v" timesig)]))

(define/contract (timesig-denom timesig)
  (-> time-signature/c duration?)
  (match timesig
    [(TimeSignatureSimple num denom) denom]
    [(TimeSignatureGrouping groups num denom) denom]
    [else (error timesig-denom "timsig-denom unrecognized time signature ~v" timesig)]))

(define/contract (add-end-durs timesig curlen addlen)
  (-> time-signature/c exact-nonnegative-integer? exact-nonnegative-integer? (listof duration?))
  (cond [(TimeSignatureSimple? timesig)
         (let ([num   (TimeSignatureSimple-num timesig)]
               [denom (TimeSignatureSimple-denom timesig)])
           (inner-add-end-durs num denom curlen addlen))]
        [(TimeSignatureGrouping? timesig)
         ;; groups is a list numerators as positive integers
         (let ([groups (TimeSignatureGrouping-groups timesig)]
               [num    (TimeSignatureGrouping-num timesig)]
               [denom  (TimeSignatureGrouping-denom timesig)])
           (let ([timesigs  (map (lambda (group) (TimeSignatureSimple group denom)) groups)]
                 [barlens   (map (lambda (group) (* group (duration->int denom))) groups)]
                 [grpcurlen (remainder curlen (* num (duration->int denom)))])
             (let* ([barlenstots   (scanl + barlens)]
                    [initcycleix   (list-index (lambda (barlenstot) (> barlenstot grpcurlen)) barlenstots)]
                    [cycletimesigs (make-cycle timesigs initcycleix)])
               ;; initial durs is special because it starts after curlen offset
               (let* ([inittimesig  (cycletimesigs 'val)]
                      [initaddlen   (min addlen (- (list-ref barlenstots initcycleix) curlen))]
                      [initcurlen   (- (list-ref barlens initcycleix) initaddlen)]
                      [initdurs     (add-end-durs inittimesig initcurlen initaddlen)])
                 ;; rest of durs cycles through time sigs with curlen 0 and addlen same as barlen
                 (let ([p (lambda (seed)
                            (begin
                              (cycletimesigs 'inc)
                              (zero? seed)))]
                       [f (lambda (seed)
                            (let* ([timesig (cycletimesigs 'val)]
                                   [addlen  (curlen->addlen timesig seed)])
                              (add-end-durs timesig 0 addlen)))]
                       [g (lambda (seed)
                            (let* ([timesig (cycletimesigs 'val)]
                                   [addlen  (curlen->addlen timesig seed)])
                              (- seed addlen)))])
                   (let* ([seed (- addlen initaddlen)]
                          [succdurss (unfold p f g seed)])
                     (flatten (cons initdurs succdurss))))))))]
        [(TimeSignatureCompound? timesig)
         ;; groups is a list of list of positive integers of one or more:
         ;; * (num denom) i.e. TimeSignatureSimple 
         ;; * (num1 num2 ... denom) i.e. TimeSignatureGrouping
         (let ([group->timesig (lambda (group)
                                 (cond [(eq? 2 (length group)) (TimeSignatureSimple (car group) (cadr group))]
                                       [else (let ([nums (take (- (length group) 1) group)]
                                                   [denom (last group)])
                                               TimeSignatureGrouping nums (apply + nums) (int->duration denom))]))])
           (let* ([timesigs (map group->timesig (TimeSignatureCompound-groups timesig))]
                  [barlens   (map (lambda (timesig) (* (timesig-num timesig) (duration->int (timesig-denom timesig)))) timesigs)]
                  [grpcurlen (remainder curlen (apply + barlens))])
             (let* ([barlenstots   (scanl + barlens)]
                    [initcycleix   (list-index (lambda (barlenstot) (> barlenstot grpcurlen)) barlenstots)]
                    [cycletimesigs (make-cycle timesigs initcycleix)])
               ;; initial durs is special because it starts after curlen offset
               (let* ([inittimesig  (cycletimesigs 'val)]
                      [initaddlen   (min addlen (- (list-ref barlenstots initcycleix) curlen))]
                      [initcurlen   (- (list-ref barlens initcycleix) initaddlen)]
                      [initdurs     (add-end-durs inittimesig initcurlen initaddlen)])
                 (cycletimesigs 'inc)
                 ;; rest of durs cycles through time sigs with curlen 0 and addlen same as barlen
                 (let ([p (lambda (seed)
                            (begin
                              (cycletimesigs 'inc)
                              (zero? seed)))]
                       [f (lambda (seed)
                            (let* ([timesig (cycletimesigs 'val)]
                                   [addlen  (curlen->addlen timesig seed)])
                              (add-end-durs timesig 0 addlen)))]
                       [g (lambda (seed)
                            (let* ([timesig (cycletimesigs 'val)]
                                   [addlen  (curlen->addlen timesig seed)])
                              (- seed addlen)))])
                   (let* ([seed (- addlen initaddlen)]
                          [succdurss (unfold p f g seed)])
                     (flatten (cons initdurs succdurss))))))))]
        [else (error 'add-end-durs "unrecognized time sig ~v" timesig)]))
|#

