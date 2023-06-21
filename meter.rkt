#lang racket

;; * add rests to the end of all voices to fill in the last measure
;;   for the longest voice if necessary
;; * align durations in each VoicesGroup voice to reflect meter
;;   e.g. by adding ties
;; assumes:
;; * durations for voice-events in voice aren't optimized for beats
;;   in the bar or bar length
;; * voices may be of different lengths
;; * total duration voice-events without ties for the output equals
;;   total duration of voice-events without ties for the input

(provide
 (contract-out
  [extend&align-voices-group-durations (-> VoicesGroup? VoicesGroup?)]))

;; - - - - - - - -
;; implementation
(require srfi/1)

(require racket/generator)

(require "score.rkt")

(require "score-utils.rkt")

(require "generators.rkt")

(require "lily-utils.rkt")

(require "utils.rkt")

;; advance (num . denom) cycle through curlen until there's less than a barlen left
;; answer the spillover curlen into the (num . denom) cycle
;; side-effect: maybe advance cycle to point at that (num . denom) value
;; assumes:  time-signature represented by cycle covers all of curlen
(define/contract (advance-num-denom-gen-to-start num-denom-gen curlen)
  (-> generator? natural-number/c natural-number/c)
  (let* ([num-denom (num-denom-gen)]
         [barlen (num-denom-pr->barlen num-denom)])
    (cond [(zero? curlen)
           0]
          [(>= barlen curlen)
           curlen]
          [else
           (advance-num-denom-gen-to-start num-denom-gen (- curlen barlen))])))

;; low-level routine given a simple time signature, current length in 128th notes
;; and length to span in 128th notes, e.g. for a rest, then a rest per duration in the
;; response list, else for a note then a note with ties per duration except for the last
;; note: if input addlen exceeds length of a single bar, this will chew threw all of it
;; assuming a repeated sequence of num denom time in the remaining bars
(define/contract (add-end-durs-for-num&denom num denom curLen addLen)
  (-> natural-number/c duration? natural-number/c natural-number/c (listof duration?))
  (let* ([beatLen (duration->int denom)]
         [barLen  (* num beatLen)])
    (let accum ([cur curLen]
                [add addLen]
                [acc '()])
      (let ([remBeat (- beatLen (remainder cur beatLen))] ;; how much remains to the next beat, none if (eq? remBeat beatLen)
            [remBar  (- barLen (remainder cur barLen))])  ;; how much remains to the next bar, none if (eq? remBar barLen)
        (cond
          [(zero? add) acc]
          [(not (equal? beatLen remBeat)) ;; next dur is distance to next beat unless amount remaining is less
           (let ([nextCur (if (< add remBeat) (+ cur add) (+ cur remBeat))]
                 [nextAdd (if (< add remBeat) 0 (- add remBeat))]
                 [nextAcc (append acc (reverse (int->durations (min add remBeat))))])
             (accum nextCur nextAdd nextAcc))]
          [(not (equal? barLen remBar)) ;; next dur is distance to next bar unless amount remaining is less
           (let ([nextCur (if (< add remBar) (+ cur add) (+ cur remBar))]
                 [nextAdd (if (< add remBar) 0 (- add remBar))]
                 [nextAcc (append acc (int->durations (min add remBar)))])
             (accum nextCur nextAdd nextAcc))]
          [(>= add barLen) ;; beginning of new bar and add is more than bar, next dur is entire bar
           (let ([nextCur (+ cur barLen)]
                 [nextAdd (- add barLen)]
                 [nextAcc (append acc (reverse (int->durations add)))])
             (accum nextCur nextAdd nextAcc))]
          [(< add barLen) ;; beginning of new bar and add is less than bar, next dur is remaining add
           (let ([nextCur (+ cur add)]
                 [nextAdd 0]
                 [nextAcc (append acc (int->durations add))])
             (accum nextCur nextAdd nextAcc))]
          [else (error 'add-end-durs-for-num&denom "beatLen: ~v barLen: ~v remBeat: ~v remBar: ~v cur: ~v add: ~v acc: ~v"
                       beatLen barLen remBeat remBar cur add acc)])))))

;; first, compute first bar list of dur lens given time signature and tot-curlen 
;; 1) convert the meter or meters in timesig to a cycle of num denom pairs for simple time sigs
;; 2) advance cycle past tot-curlen to first partial bar, answer dur len from start (spilllen)
;; 3) compute durlen from spilllen to end of first bar
;; 4) initial amount to add is minimum of tot-addlen and durlen to end of bar
;; 5) compute list of durs for first bar given spillover as curlen, initial addlen
;; 6) compute list of list of dur lens for successive bars given remaining addlen bar by
;;    bar until remaining addlen goes to zero
(define/contract (add-end-durs-for-time-signature timesig tot-curlen tot-addlen)
  (-> time-signature/c natural-number/c natural-number/c (listof duration?))
  (let* ([num-denom-gen (time-signature->num-denom-generator timesig)]                                           ;; 1
         [spilllen      (advance-num-denom-gen-to-start num-denom-gen tot-curlen)]                               ;; 2
         [num-denom-pr  (num-denom-gen)]
         [rem-bar       (- (num-denom-pr->barlen num-denom-pr) spilllen)]                                        ;; 3
         [init-addlen   (min tot-addlen rem-bar)]                                                                ;; 4
         [init-durs     (add-end-durs-for-num&denom (car num-denom-pr) (cdr num-denom-pr) spilllen init-addlen)] ;; 5
         [succ-durs     (let inner ([rem-addlen (- tot-addlen init-addlen)])                                     ;; 6
                          (if (zero? rem-addlen)
                              '()
                              (let* ([num-denom-pr (num-denom-gen)]
                                     [inner-addlen (min rem-addlen (num-denom-pr->barlen num-denom-pr))])
                                (cons (add-end-durs-for-num&denom (car num-denom-pr) (cdr num-denom-pr) 0 inner-addlen)
                                      (inner (- rem-addlen inner-addlen))))))])
    (apply append (cons init-durs succ-durs))))

;; require list of durs for voices with multiple staves (KeyboardVoice)
(define/contract (add-rests-for-durlens tot-durlen voice added-durlens)
  (-> natural? voice/c (listof natural?) voice/c)
  (define (durlen->rests added-durlen) (map Rest (int->durations (tot-durlen - added-durlen))))
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (append voice-events (durlen->rests (car added-durlens))))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (append (car voice-events-pr) (durlen->rests (car added-durlens)))
                                (append (cdr voice-events-pr) (durlen->rests (cadr added-durlens)))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (append voice-events (durlen->rests (car added-durlens))))]))

;; add rests to the end of voice-events to carry all voices to the end of the last measure
(define/contract (extend-voices-durations time-sig voices)
  (-> time-signature/c (listof voice/c) (listof voice/c))
  (let* (;; list of list of total durlens per voice e.g. '((24) (24 32) (18))
         [voices-total-durlenss (map voice->total-durs voices)]
         ;; max of all list of list of total durlens, e.g. 32, make voices at least this long
         [max-total-durlen      (apply max (map ((curry apply) max) voices-total-durlenss))]
         ;; len of bar
         [bar-durlen            (time-signature->barlen time-sig)]
         ;; given max total-durlen, how much spills over into the last bar for the max-total-durlen?
         [last-bar-spill-over   (modulo max-total-durlen bar-durlen)]
         ;; given last-bar-spill-over, how much remains to get to the end of that bar?
         [last-bar-remainder    (if (zero? last-bar-spill-over) 0 (- bar-durlen last-bar-spill-over))]
         ;; target total durlen is sum of max-total-durlen and last-bar-remainder
         [target-total-durlen   (+ max-total-durlen last-bar-remainder)]
         ;; difference between per-voice total-durlenss and target-total-durlen is durlen to add for rests per voice
         [voices-rem-durlenss   (map ((curry map) ((curry -) target-total-durlen)) voices-total-durlenss)])
    (map ((curry add-rests-for-durlens) max-total-durlen) voices voices-rem-durlenss)))

;; spread note-or-chord (except for dur) across all durs with pitch or pitches
;; from input with:
;; * ties for all note-or-chord except the last,
;; * controls only for the first
;; called when dur for note or chord spans beat or bar divisions
(define/contract (spread-note-or-chord-durations note-or-chord durs)
  (-> (or/c Note? Chord?) (listof duration?) (listof (or/c Note? Chord?)))
  (let ([copy-note-or-chord
         (lambda (note-or-chord dur controls tie)
           (match note-or-chord
             [(Note pitch octave _ _ _)
              (Note pitch octave dur controls tie)]
             [(Chord pitches _ _ _)
              (Chord pitches dur controls tie)]))]
        [note-or-chord->controls
         (lambda (note-or-chord)
           (match note-or-chord
             [(Note _ _ _ controls _)
              controls]
             [(Chord _ _ controls _)
              controls]))])
    (cond [(= 1 (length durs))
           (list (copy-note-or-chord note-or-chord (car durs) (note-or-chord->controls note-or-chord) #f))]
          [(= 2 (length durs))
           (cons (copy-note-or-chord note-or-chord (car durs) (note-or-chord->controls note-or-chord) #t)
                 (list (copy-note-or-chord note-or-chord (cadr durs) '() #f)))]
          [else
           (let* ([start    (copy-note-or-chord note-or-chord (car durs) (note-or-chord->controls note-or-chord) #t)]
                  [mid-durs (take (cdr durs) (- (length (cdr durs)) 1))]
                  [middle   (map (lambda (dur) (copy-note-or-chord note-or-chord dur '() #t)) mid-durs)]
                  [end      (copy-note-or-chord note-or-chord (last durs) '() #f)])
             (cons start (append middle (list end))))])))

;; bug: Note and Rest require duration? which can only be one of discrete e.g. non-tied values '(W. W H. H .. HTE)
;; separation into integral rests is ok because you never tie between rests
;; but if I stick to Note I have to be prepared to aggregate tied notes which is the only way to make arbitrary
;; length notes
;; but I don't deal with ties below at all, in fact, I'll arbitrary suppress them depending on whether or not I
;; split the note into a list of tied notes
;; so that's just a bug, but it calls into question the likely practice of the generation of a list of note or rest
;; events, which is likely to consume arbitrary duration lengths in units of 128th notes
;; which is what I believe I did in my Haskell implmentation, which was to leave durations as duration values in
;; the base types and to assume at the point of lilypond output they'd been reduced to the discrete values, with
;; ties between notes if necessary
;; do I want to preserve this boundary and replicate the DurationVal type from Haskell which is sort of a two-faced
;; solution as it allows for things to blow up at the lilypond generation level after you've gone to all the trouble
;; of enconding an instance of the Score type?
;;
;; it's a pretty big dividing point because as things stand now, all the work below misses the point and it's going
;; to take some thoughtful reworking to fix
;; as the Haskell implementation stands it's a bit of a lie because of the DurationVal embedded in the Score type
;; in my past Scheme experiments though I've never implemented the distribution of durations to reflect beats and
;; bars, though I have plenty of code that treats with more abstract levels of data like the duration length, which
;; gets resolved at a later stage to a list of one or more rests or tied notes which is what would better in terms
;; of pipelining
;;
;; which asks the question, what do I want the pipelining to be then, what are the stages in assembling as score?
;; I could have a point where I pick a duration and map it to a list of one or more notes or durations where the
;; notes are tied and then I have to deal with tied notes below, which I don't currently do
;; or I could have an intermediate form that indicates a pitch or a rest along with an arbitrary natural? for a
;; duration in units of 128th notes
;; that's intuitively more appealing because at that level I'm working with atoms anyway, likely including
;; optional maybe accents and/or maybe dynamics
;; in which case this code needs to shift down a level to deal with lists just the pitch/rest and duration with
;; input of (cons/c (or/c pitch/c #f) natural?) and output of (listof (or/c Note? Rest?)) where adjacent notes
;; may or may not be tied
;; or maybe the conversion could retain the level of focus and convert (listof (cons/c (or/c pitch/c #f) natural?))
;; to (listof (listof (cons/c (or/c pitch/c #f) duration?))) where adjacent (pitch/c . duration?) pairs are to be
;; tied and adjacent (#f . duration?) are integral units to be eventually converted to Rest (or Spacer)
;; what about Keyboard voice?
;;
;; or maybe seeing as I'm really reducing the number space for the natural-number to only those values allowed by
;; duration? then I might as well go
;; (-> (listof maybe-pitch&natural/c) (listof (or/c Note? Rest?)) 
;; where tied as needed to match the time-signature and the intermediate stage would be
;; (-> (listof maybe-pitch&natural/c) (listof (listof maybe-pitch&duration/c))) where inner lists 
;; of more than one pitch would be translated into a series of tied notes and rests would just be flattened
;;
;; but what that list of pitch implies is a single attack or envelope which is where I'd want to attach controls
;; so the meaningful division is between events that can be decorated with controls vs. events that lack controls
;;
;; note furthermore the need to capture the generator pattern as part of this
;; I currently assume a complete list as input but maybe what I'm sourcing is a generators of the components
;; which in this case wold be (cons/c maybe-pitch/c natural-number/c)
;;
;; the problem with that is I need to have a complete list in order to determine total length to append the
;; rests to the ending bar
;;
;; which seems like premature optimization really, as during the accumulation of the components that make up
;; a list of note-or-rest voice-events there's no reason I can't consume incrementally from source generators
;; of the atoms which here I propose as maybe-note and duration maybe including controls as a sub-generator
;; for the note path, all subject to eventual termination with e.g. generate-while which could stop when the
;; next duration would cause the running total to exceed a mximum value
;;
;; and then at *that* point I can decide whether to clip the voice-events by end of the last bar for the
;; shortest or to extend all voices with rests to match the end of the bar after longest
;;
;; all of these sharply redraws the contents of this module to routines that will be called inside an
;; generator or otherwise, possibly in terms of generators of lower level, atomic components, principal
;; among which is the use of natural-number/c for the duration-length contract which gets transmuted into
;; (non-empty-listof pitch/c) where a list of more than one pitch/c still represents one attack to be
;; eventually be translate into a Note with a possibly non-empty list of control/c with all but the laast
;; having #t for Note-tie, but that's only for a later step that lifts generic components into concrete
;; instances of voice-event/c
;;
;; 

;; (define maybe-pitch&natural/c
;;   (make-flat-contract #:name 'maybe-pitch&natural/c #:first-order (cons/c maybe-pitch/c natural?)))

;; (define maybe-pitch&naturals/c
;;   (make-flat-contract #:name 'maybe-pitch&naturals/c #:first-order (listof maybe-pitch&natural/c)))

;; (define/contract (align-event-durations time-signature events)
;;   (-> time-signature/c (listof maybe-pitch&natural/c) (listof (listof maybe-pitch&natural/c)))
;;   (define/contract (adjust-event-durations event pr)
;;     (-> maybe-pitch&natural? (cons/c natural? maybe-pitch&naturals/c) (cons/c natural? maybe-pitch&naturals/c))
;;     (let ([curlen (car pr)]
;;           [ret    (cdr pr)])
;;       '()))
;;   (define/contract (adjust-events-durations events pr)
;;     (-> maybe-pitch&naturals/c (cons/c natural? maybe-pitch&naturals) (cons/c natural? maybe-pitch&naturals))
;;     (let ([curlen (car pr)]
;;           [ret    (cdr pr)])
;;       (if (cdar event)
;;           (let* ([addlen (apply + (map (compose duration->int cdr ...)) events)]
;;                  [dur-lens 
;;                  []))
;;           '())))
;;   (flatten (cdr (foldl adjust-events-durations (cons 0 '()) (group-by cdar events)))))

(define/contract (adjacent-rests? a b)
  (-> voice-event/c voice-event/c boolean?)
  (and (Rest? a) (Rest? b)))

(define/contract (adjacent-tied-notes? a b)
  (-> voice-event/c voice-event/c boolean?)
  (and (Note? a) (Note-tie a) (Note? b)))

(define/contract (adjacent-tied-chords? a b)
  (-> voice-event/c voice-event/c boolean?)
  (and (Chord? a) (Chord-tie a) (Chord? b)))

(define/contract (adjacent-rests-or-tied-notes-or-chords? a b)
  (-> voice-event/c voice-event/c boolean?)
  (or (adjacent-rests? a b) (adjacent-tied-notes? a b) (adjacent-tied-chords? a b)))

(module+ test
  (require rackunit)
  (check-false (adjacent-rests? (Rest 'Q) (Spacer 'Q)))
  (check-false (adjacent-rests? (Spacer 'Q) (Rest 'Q)))
  (check-true  (adjacent-rests? (Rest 'Q) (Rest 'Q)))
  (define tied-note (Note 'C '0va 'Q '() #t))
  (define not-tied-note (Note 'C '0va 'Q '() #f))
  (check-true (adjacent-tied-notes? tied-note tied-note))
  (check-true (adjacent-tied-notes? tied-note not-tied-note))
  (check-false (adjacent-tied-notes? not-tied-note not-tied-note))
  (define pitches (list (cons 'C '0va) (cons 'C '8va)))
  (define tied-chord (Chord pitches 'Q '() #t))
  (define not-tied-chord (Chord pitches 'Q '() #f))
  (check-true (adjacent-tied-chords? tied-chord tied-chord))
  (check-true (adjacent-tied-chords? tied-chord not-tied-chord))
  (check-false (adjacent-tied-chords? not-tied-chord not-tied-chord))
  (check-true (adjacent-rests-or-tied-notes-or-chords? (Rest 'Q) (Rest 'Q)))
  (check-true (adjacent-rests-or-tied-notes-or-chords? tied-note tied-note))
  (check-true (adjacent-rests-or-tied-notes-or-chords? tied-note not-tied-note))
  (check-true (adjacent-rests-or-tied-notes-or-chords? tied-chord tied-chord))
  (check-true (adjacent-rests-or-tied-notes-or-chords? tied-chord not-tied-chord))
  )

;; using time-signature and list of voice-events, arrange the durations
;; to reflect the inner beat and measure divisions
;; * for list of one or more rests, aggregate the duration for all rests
;;   and emit one rest per duration because there are no ties or controls
;; * for list of one or more other voice events, split individual note
;;   or chords into a list of tied note or chords, suppressing the controls
;;   for the inner (tied) note or notes
(define/contract (align-voice-events-durations time-signature voice-events)
  (-> time-signature/c (listof voice-event/c) (listof voice-event/c))
  ;; inner fold over (should be) single-item lists of voice-event
  (define (adjvedurs voice-event pr)
    (let ([curlen (car pr)]
          [ves    (cdr pr)])
      ;; individual Rest, Note, or Chord durations might traverse beat or bar divisions 
      (cond [(Rest? voice-event)
             (let* ([addlen (duration->int (Rest-dur voice-event))]
                    [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                    [rests  (map Rest durs)])
               (cons (+ curlen addlen) (append ves rests)))]
            [(or (Note? voice-event)
                 (Chord? voice-event))
             (let* ([addlen          (voice-event->duration-int voice-event)]
                    [durs            (add-end-durs-for-time-signature time-signature curlen addlen)]
                    [notes-or-chords (spread-note-or-chord-durations voice-event durs)])
               (cons (+ curlen addlen) (append ves notes-or-chords)))]
            [else
             ;; Tuplet, Spacer: treat integrally with durs as is
             ;; KeySignature, clef: zero-dur from voice-event->duration-int
             (let ([addlen (voice-event->duration-int voice-event)])
               (cons (+ curlen addlen) (list voice-event)))])))
  ;; outer fold over grouping of voice-events by rest or 
  (define (adjvesdurs ve-group pr)
    (let ([curlen (car pr)]
          [ves    (cdr pr)])
      (cond
        ;; special handling for lists of Rest, Note, and Chord:
        ;; - sum durations then use that to get the list of durations 
        ;;   for the time-signature and sum durations from the start
        ;; - for list of Rest, map Rest to each of durations (no ties)
        ;; - for list of Note or Chord call spread-note-or-chord-durations
        ;;   to copy controls and distribute ties
        [(and (> (length ve-group) 1)
              (Rest? (car ve-group)))
         (let* ([addlen (apply + (map voice-event->duration-int ve-group))]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [rests  (map Rest durs)])
           (cons (+ curlen addlen) (append ves rests)))]
        [(and (> (length ve-group) 1)
              (or (Note? (car ve-group))
                  (Chord? (car ve-group))))
         (let* ([addlen (apply + (map voice-event->duration-int ve-group))]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [notes-or-chords (spread-note-or-chord-durations (car ve-group) durs)])
           (cons (+ curlen addlen) (append ves notes-or-chords)))]
        ;; remaining lists are all one item long of Note or Chord (no ties), Rest
        ;; or zero-duration voice-events like KeySignature or clef
        ;; fold over them to be sure, distributing rest, note, and chord durations
        ;; according to beat and bar
        [else
         (let ([pr2 (foldl adjvedurs (cons curlen '()) ve-group)])
           (cons (car pr2) (append ves (cdr pr2))))])))
  ;; fold over ve-groups as (list (listof voice-event)) where rests, tied notes, and 
  ;; tied chords are grouped together and everything else is in a singleton list
  (let ([ve-groups (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)])
    (let* ([pr (foldl adjvesdurs (cons 0 '()) ve-groups)]
           [rets (cdr pr)]
           [ret (flatten rets)])
      (flatten (cdr (foldl adjvesdurs (cons 0 '()) ve-groups)))
      )))

(module+ test
  (require rackunit)
  ;; build list of voice-event with some adjacent rests, some tied notes, some tied chords
  ;; mixed in with individual untied notes and chords, try
  ;; (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)
  (define rest (Rest 'E))
  (define voice-events (list rest
                             not-tied-note
                             not-tied-note
                             tied-note
                             not-tied-note
                             rest
                             rest
                             not-tied-chord
                             not-tied-note
                             rest
                             tied-chord not-tied-chord
                             tied-chord not-tied-chord
                             tied-chord tied-chord not-tied-chord
                             not-tied-note
                             not-tied-note))
  (check-equal?
   (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)
   (list
    (list (Rest 'E))
    (list (Note 'C '0va 'Q '() #f))
    (list (Note 'C '0va 'Q '() #f))
    (list (Note 'C '0va 'Q '() #t) (Note 'C '0va 'Q '() #f))
    (list (Rest 'E) (Rest 'E))
    (list (Chord '((C . 0va) (C . 8va)) 'Q '() #f))
    (list (Note 'C '0va 'Q '() #f))
    (list (Rest 'E))
    (list (Chord '((C . 0va) (C . 8va)) 'Q '() #t) (Chord '((C . 0va) (C . 8va)) 'Q '() #f))
    (list (Chord '((C . 0va) (C . 8va)) 'Q '() #t) (Chord '((C . 0va) (C . 8va)) 'Q '() #f))
    (list (Chord '((C . 0va) (C . 8va)) 'Q '() #t) (Chord '((C . 0va) (C . 8va)) 'Q '() #t) (Chord '((C . 0va) (C . 8va)) 'Q '() #f))
    (list (Note 'C '0va 'Q '() #f))
    (list (Note 'C '0va 'Q '() #f))))

  (let ([voice-events (list rest
                            not-tied-note
                            not-tied-note
                            tied-note
                            not-tied-note)])
    (check-equal?
     (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)
     (list
      (list (Rest 'E))
      (list (Note 'C '0va 'Q '() #f))
      (list (Note 'C '0va 'Q '() #f))
      (list (Note 'C '0va 'Q '() #t) (Note 'C '0va 'Q '() #f)))))
  
  ;; simple adjustment to distribute quarter notes over beats in 4/4
  (define four-four-time-signature (TimeSignatureSimple 4 'Q))
  (let ([voice-events (list rest
                            not-tied-note
                            not-tied-note
                            not-tied-note
                            not-tied-note)])
    (check-equal?
     (align-voice-events-durations four-four-time-signature voice-events)
     (list
      (Rest 'E)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f))))

  (let ([voice-events (list rest
                            not-tied-note
                            not-tied-note
                            tied-note
                            not-tied-note)])
    (check-equal?
     (align-voice-events-durations four-four-time-signature voice-events)
     (list
      (Rest 'E)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'E '() #f)
      (Note 'C '0va 'E '() #t)
      (Note 'C '0va 'Q '() #t)
      (Note 'C '0va 'E '() #f)))))

(define/contract (align-voice-durations time-signature voice)
  (-> time-signature/c voice/c voice/c)
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (align-voice-events-durations time-signature voice-events))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (align-voice-events-durations time-signature (car voice-events-pr))
                                (align-voice-events-durations time-signature (cdr voice-events-pr))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (align-voice-events-durations time-signature voice-events))]))
  
(define (extend&align-voices-group-durations voices-group)  
  (let* ([time-signature  (VoicesGroup-time-signature voices-group)]
         ;; first extend all voices to end at the last bar line
         [extended-voices (extend-voices-durations time-signature (VoicesGroup-voices voices-group))]
         ;; then align voice-event durations to reflect the meter
         [extended&aligned-voices (map ((curry align-voice-durations) time-signature) extended-voices)])
    (struct-copy VoicesGroup voices-group [voices extended&aligned-voices])))

#|
(define tpo (TempoDur 'Q 120))
(define tsg (TimeSignatureSimple 4 'Q))
(define vevts (list (Note 'C '0va 'H '() #f) (Note 'F '0va 'H. '() #f)))
(define pvc (PitchedVoice 'AcousticGrand vevts))
(define pvc2 (PitchedVoice 'AcousticGrand (list (Note 'D '0va 'H. '() #f))))
(define vgrp (VoicesGroup tpo tsg (list pvc pvc2)))
(extend&align-voices-group-durations vgrp)
(VoicesGroup
 (TempoDur 'Q 120)
 (TimeSignatureSimple 4 'Q)
 (list
  (PitchedVoice 'AcousticGrand (list (Note 'C '0va 'H '() #f) (Note 'F '0va 'H '() #t) (Note 'F '0va 'Q '() #f) (Rest 'H.)))
  (PitchedVoice 'AcousticGrand (list (Note 'D '0va 'H. '() #f) (Rest 'Q) (Rest 'W)))))
|#
