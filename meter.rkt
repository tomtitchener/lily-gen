#lang racket

;; meter: manage listof voice-event/c in a voice/c to reflect the meter
;; (needs a better name)

(provide
 (contract-out
  ;; rewrite voice-events with durations that reflect beat and bar divisions
  ;; e.g. divide a 'Q. note that falls between beats in 4/4 into tied 'E and 'Q notes
  [align-voice-events-durations (-> time-signature/c (listof voice-event/c) (listof voice-event/c))]
  
  ;; map align-voice-events-durations over one or more lists of voice-event/c in a voice/c
  [align-voice-durations (-> time-signature/c voice/c voice/c)]

  ;; find the listof voice-event/c with the longest cumulative duration,
  ;; determine the duration to finish the current measure,
  ;; add rests to all listof voice-event/c to match the longest listof voice-event/c
  [extend-voices-durations (-> time-signature/c (listof voice/c) (listof voice/c))]
  
  ;; combine extend-voices-durations with align-voices-durations 
  ;; for the listof voice/c in the VoicesGroup
  [extend&align-voices-group-durations (-> VoicesGroup? VoicesGroup?)]

  ;; find the listof voice-event/c with the shortest cumulative duration,
  ;; clip all voices to duration at the beginning of the last measure so
  ;; they all end at the same measure
  [clip-voices-durations (-> time-signature/c voice/c voice/c)]

  ;; combine clip-voices-durations with align-voices-durations 
  ;; for the listof voice/c in the VoicesGroup
  [clip&align-voices-group-durations (-> VoicesGroup? VoicesGroup?)]))

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
(define/contract (add-end-durs-for-num&denom num-denom-pr curLen addLen)
  (-> num-denom/c natural-number/c natural-number/c (listof duration?))
  (let* ([beatLen (duration->int (cdr num-denom-pr))]
         [barLen  (* (car num-denom-pr) beatLen)])
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

;; given time-signature, duration total so far (tot-curlen) and duration total to add (tot-addlen)
;; answer list of durations that fill tot-addlen with respect to the current beat and bar length
;; 1) convert the meter or meters in timesig to a cycle of num denom pairs for simple time sigs
;; 2) advance cycle past tot-curlen to current bar, answer dur len from start of current bar
;;   (spill-len, may be 0)
;; 3) compute durlen from spill-len to end of first bar (rem-bar)
;; 4) initial amount to add is minimum of tot-addlen and durlen to end of bar (init-addlen)
;; 5) compute list of durs for first bar given spillover as curlen, initial addlen (init-durs)
;; 6) compute list of list of dur lens for successive bars given remaining addlen bar by
;;    bar until remaining addlen goes to zero (succ-durs)
(define/contract (add-end-durs-for-time-signature timesig tot-curlen tot-addlen)
  (-> time-signature/c natural-number/c natural-number/c (listof duration?))
  (let* ([num-denom-gen (time-signature->num-denom-generator timesig)]                    ;; 1
         [spill-len     (advance-num-denom-gen-to-start num-denom-gen tot-curlen)]        ;; 2
         [num-denom-pr  (num-denom-gen)]
         [rem-bar       (- (num-denom-pr->barlen num-denom-pr) spill-len)]                ;; 3
         [init-addlen   (min tot-addlen rem-bar)]                                         ;; 4
         [init-durs     (add-end-durs-for-num&denom num-denom-pr spill-len init-addlen)]  ;; 5
         [succ-durs     (let inner ([rem-addlen (- tot-addlen init-addlen)])              ;; 6
                          (if (zero? rem-addlen)
                              '()
                              (let* ([num-denom-pr (num-denom-gen)]
                                     [bar-len      (num-denom-pr->barlen num-denom-pr)]
                                     [inner-addlen (min rem-addlen bar-len)])
                                (cons (add-end-durs-for-num&denom num-denom-pr 0 inner-addlen)
                                      (inner (- rem-addlen inner-addlen))))))])
    (apply append (cons init-durs succ-durs))))

;; require list of durs for voices with multiple staves (KeyboardVoice)
(define/contract (add-rests-for-durlens voice added-durlens)
  (-> voice/c (listof natural-number/c) voice/c)
  (define (durlen->rests added-durlen) (map Rest (int->durations added-durlen)))
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (append voice-events (durlen->rests (car added-durlens))))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (append (car voice-events-pr) (durlen->rests (car added-durlens)))
                                (append (cdr voice-events-pr) (durlen->rests (cadr added-durlens)))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (append voice-events (durlen->rests (car added-durlens))))]))

;; add rests to the end of voice-events to carry all voices to the end of the last measure
;; (-> time-signature/c (listof voice/c) (listof voice/c))
(define (extend-voices-durations time-sig voices)
  (let* (;; list of list of total durlens per voice e.g. '((24) (24 32) (18))
         [voices-total-durlens (map voice->total-durs voices)] ;; flatten is cause of loss of structure
         ;; max of all list of list of total durlens, e.g. 32, make voices at least this long
         [max-total-durlen      (apply max (flatten voices-total-durlens))]
         ;; len of bar
         [bar-durlen            (time-signature->barlen time-sig)]
         ;; given max total-durlen, how much spills over into the last bar?
         [last-bar-spill-over   (modulo max-total-durlen bar-durlen)]
         ;; given last-bar-spill-over, how much remains to get to the end of that bar?
         [last-bar-remainder    (if (zero? last-bar-spill-over) 0 (- bar-durlen last-bar-spill-over))]
         ;; target total durlen is sum of max-total-durlen and last-bar-remainder
         [target-total-durlen   (+ max-total-durlen last-bar-remainder)]
         [voices-rem-durlenss (map (lambda (durlens) (map (curry - target-total-durlen) durlens)) voices-total-durlens)])
    (map (lambda (voice rem-durlens) (add-rests-for-durlens voice rem-durlens)) voices voices-rem-durlenss)))

;; clip-voice-events finishes with left-over duration that's shorter the voice-event at that spot
;; given the voice-event and the remaining durlen, first convert durlen to a list of durations,
;; then map the remaining voice event into one or more events of the same length
(define/contract (replace-voice-event-durlen voice-event durlen)
  (-> voice-event/c natural-number/c (listof voice-event/c))
  (let ([durs (int->durations durlen)])
    (match voice-event
      ;; when length durs > 1, need to have ties between multiple notes or chords
      [(Note pit oct _ ctrls _)
       (ctrls-durs&pit->notes ctrls durs (cons pit oct))]
      [(Chord pits _ ctrls _)
       (ctrls-durs&pits->chords ctrls durs pits)]
      ;; when length durs > 1, can just replace with individual events for Rest or Spacer
      [(Rest _)
       (map Rest durs)]
      [(Spacer _)
       (map Spacer durs)]
      ;; ignore Tuplet rhythm and just use it a source of notes unless there 
      ;; are fewer notes than durations, in which case fill out with rests
      [(Tuplet _ _ _ notes)
       (let* ([cnt-rests (- (length notes) (length durs))]
              [rests (if (<= cnt-rests 0) '() (map Rest (range 0 cnt-rests)))]
              [all-notes (append notes rests)])
         (flatten (map replace-voice-event-durlen all-notes (duration->int durs))))]
      ;; should have consumed all zero-duration voice-events already
      [(KeySignature _ _)
       (error 'replace-voice-event-durlen "unexpected voice-event ~v" voice-event)]
      [(? clef?)
       (error 'replace-voice-event-durlen "unexpected voice-event ~v" voice-event)])))

(define/contract (clip-voice-events total-durlen voice-events)
  (-> natural-number/c (listof voice-event/c) (listof voice-event/c)) 
  (let* ([voice-events-start (take-while (sum<=? voice-event->duration-int total-durlen) voice-events)]
         [voice-events-start-len (apply + (map voice-event->duration-int voice-events-start))])
    (cond
      [(= voice-events-start-len total-durlen)
       voice-events-start]
      [(< voice-events-start-len total-durlen)
       (let* ([next-voice-event (list-ref (length voice-events-start) voice-events)]
              [last-durlen (- total-durlen (voice-event->duration-int next-voice-event))])
         (append voice-events (replace-voice-event-durlen last-durlen next-voice-event)))]
      [else
       (error 'clip-voice-events
              "voice-events-start-len ~v > total-durlen ~v"
              voice-events-start-len total-durlen)])))
       
;; require list of durs for voices with multiple staves (KeyboardVoice)
(define/contract (clip-voice-events-at-total-durlen total-durlen voice)
  (-> natural-number/c voice/c voice/c)
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (clip-voice-events total-durlen voice-events))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (clip-voice-events total-durlen (car voice-events-pr))
                                (clip-voice-events total-durlen (cdr voice-events-pr))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (clip-voice-events total-durlen voice-events))]))
  
;; (-> time-signature/c (listof voice/c) (listof voice/c))
(define (clip-voices-durations time-sig voices)
  (let* (;; list of list of total durlens per voice e.g. '((24) (24 32) (18))
         [voices-total-durlens (map voice->total-durs voices)] ;; flatten is cause of loss of structure
         ;; max of all list of list of total durlens, e.g. 32, make voices at least this long
         [min-total-durlen      (apply min (flatten voices-total-durlens))]
         ;; len of bar
         [bar-durlen            (time-signature->barlen time-sig)]
         ;; given min total-durlen, how much spills over into the last bar?
         [last-bar-spill-over   (modulo min-total-durlen bar-durlen)]
         ;; target total durlen is diff of min-total-durlen and last-bar-spill-over
         [target-total-durlen   (- min-total-durlen last-bar-spill-over)])
    (map ((curry clip-voice-events-at-total-durlen) target-total-durlen) voices)))

;; spread note-or-chord (except for dur) across all durs with pitch or pitches
;; from input with:
;; * ties for all note-or-chord except the last,
;; * controls only for the first
;; called when dur for note or chord spans beat or bar divisions
#;(define/contract (spread-note-or-chord-durations note-or-chord durs)
  (-> (or/c Note? Chord?) (listof duration?) (or/c (listof Note?) (listof Chord?)))
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
;; (-> time-signature/c (listof voice-event/c) (listof voice-event/c))
(define (align-voice-events-durations time-signature voice-events)
  ;; inner fold over (should be) single-item lists of voice-event
  (define (adjvedurs voice-event pr)
    (let ([curlen (car pr)]
          [ves    (cdr pr)])
      ;; individual Rest, Note, or Chord durations might traverse beat or bar divisions 
      (match voice-event
        [(Rest dur)
         (let* ([addlen (duration->int dur)]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [rests  (map Rest durs)])
           (cons (+ curlen addlen) (append ves rests)))]
        [(Note pitch-class octave dur ctrls _)
         (let* ([addlen (duration->int dur)]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [notes-or-chords (ctrls-durs&pit->notes ctrls durs (cons pitch-class octave))])
           (cons (+ curlen addlen) (append ves notes-or-chords)))]
        [(Chord pitches dur ctrls _)
         (let* ([addlen (duration->int dur)]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [notes-or-chords (ctrls-durs&pits->chords ctrls durs pitches)])
           (cons (+ curlen addlen) (append ves notes-or-chords)))]
        ;; TBD: special handling for Tuplet, which must not traverse a bar, can I
        ;; tell that's happening here?
        [(or (Spacer _) (Tuplet _ _ _ _) (KeySignature _ _) (? clef?))
         ;; Tuplet, Spacer: treat integrally with durs unchanged
         ;; KeySignature, clef: zero-dur from voice-event->duration-int
         (let ([addlen (voice-event->duration-int voice-event)])
           (cons (+ curlen addlen) (list voice-event)))])
      ))
  ;; outer fold over grouping of voice-events by Rest, Note, or Chord
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
        [(and (> (length ve-group) 1) (Rest? (car ve-group)))
         (let* ([addlen (apply + (map voice-event->duration-int ve-group))]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [rests  (map Rest durs)])
           (cons (+ curlen addlen) (append ves rests)))]
        [(and (> (length ve-group) 1) (Note? (car ve-group)))
         (let* ([addlen (apply + (map voice-event->duration-int ve-group))]
                [durs   (add-end-durs-for-time-signature time-signature curlen addlen)]
                [note   (car ve-group)]
                [pitch  (cons (Note-pitch note) (Note-octave note))]
                [ctrls  (Note-controls note)]
                [notes  (ctrls-durs&pit->notes ctrls durs pitch)])
           (cons (+ curlen addlen) (append ves notes)))]
        [(and (> (length ve-group) 1) (Chord? (car ve-group)))
         (let* ([addlen  (apply + (map voice-event->duration-int ve-group))]
                [durs    (add-end-durs-for-time-signature time-signature curlen addlen)]
                [chord   (car ve-group)]
                [pitches (Chord-pitches chord)]
                [ctrls   (Chord-controls chord)]
                [chords  (ctrls-durs&pits->chords ctrls durs pitches)])
           (cons (+ curlen addlen) (append ves chords)))]
        ;; remaining lists are all one item long of Note (no ties), Chord (no ties), 
        ;; Rest, zero-duration voice-events like KeySignature or clef, or integral
        ;; integral duration events like Spacer or Tuplet, 
        ;; fold over them to be sure, distributing rest, note, and chord durations
        ;; according to beat and bar
        [else
         (let ([pr2 (foldl adjvedurs (cons curlen '()) ve-group)])
           (cons (car pr2) (append ves (cdr pr2))))])))
  ;; 1) group voice-events as (listof voice-event/c) by adjacent rests, tied notes, or tied chords
  ;;    yielding (listof (listof voice-event/c)) with single-item lists for single rests, non-tied
  ;;    notes, non-tied chords, zero-duration events like clef? and KeySignature?, Spacer and Tuplet
  ;; 2) fold over (list (listof voice-event)) distributing durations by position of event relative
  ;     to time-signature
  (let ([ve-groups (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)])
    (let* ([pr (foldl adjvesdurs (cons 0 '()) ve-groups)]
           [rets (cdr pr)]
           [ret (flatten rets)])
      (flatten (cdr (foldl adjvesdurs (cons 0 '()) ve-groups))))))

;; (-> time-signature/c voice/c voice/c)
(define (align-voice-durations time-signature voice)
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (align-voice-events-durations time-signature voice-events))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (align-voice-events-durations time-signature (car voice-events-pr))
                                (align-voice-events-durations time-signature (cdr voice-events-pr))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (align-voice-events-durations time-signature voice-events))]))

;; (-> VoicesGroup? VoicesGroup?)
(define (extend&align-voices-group-durations voices-group)  
  (let* ([time-signature  (VoicesGroup-time-signature voices-group)]
         ;; first extend all voices to end at the last bar line
         [extended-voices (extend-voices-durations time-signature (VoicesGroup-voices voices-group))]
         ;; then align voice-event durations to reflect the meter
         [extended&aligned-voices (map (curry align-voice-durations time-signature) extended-voices)])
    (struct-copy VoicesGroup voices-group [voices extended&aligned-voices])))

;; clip all voices to duration of end of last full bar for the shortest voice
;; (-> VoicesGroup? VoicesGroup?)
(define (clip&align-voices-group-durations voices-group)  
  (let* ([time-signature  (VoicesGroup-time-signature voices-group)]
         ;; first extend all voices to end at the last bar line
         [clipped-voices (clip-voices-durations time-signature (VoicesGroup-voices voices-group))]
         ;; then align voice-event durations to reflect the meter
         [clipped&aligned-voices (map (curry align-voice-durations time-signature) clipped-voices)])
    (struct-copy VoicesGroup voices-group [voices clipped&aligned-voices])))

(module+ test
  (require rackunit)
  ;; build list of voice-event with some adjacent rests, some tied notes, some tied chords
  ;; mixed in with individual untied notes and chords, try
  ;; (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events)
  (define rest (Rest 'E))
  (define voice-events-1 (list rest
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
   (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events-1)
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

  (define voice-events-2 (list rest
                               not-tied-note
                               not-tied-note
                               tied-note
                               not-tied-note))
  (check-equal?
   (group-by-adjacent-sequences adjacent-rests-or-tied-notes-or-chords? voice-events-2)
   (list
    (list (Rest 'E))
    (list (Note 'C '0va 'Q '() #f))
    (list (Note 'C '0va 'Q '() #f))
    (list (Note 'C '0va 'Q '() #t) (Note 'C '0va 'Q '() #f))))
  
  ;; simple adjustment to distribute quarter notes over beats in 4/4
  (define four-four-time-signature (TimeSignatureSimple 4 'Q))
  (define voice-events-3 (list rest
                               not-tied-note
                               not-tied-note
                               not-tied-note
                               not-tied-note))
  (check-equal?
   (align-voice-events-durations four-four-time-signature voice-events-3)
   (list
    (Rest 'E)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)))

  (define voice-events-4 (list rest
                               not-tied-note
                               not-tied-note
                               tied-note
                               not-tied-note))
  (check-equal?
   (align-voice-events-durations four-four-time-signature voice-events-4)
   (list
    (Rest 'E)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'E '() #f)
    (Note 'C '0va 'E '() #t)
    (Note 'C '0va 'Q '() #t)
    (Note 'C '0va 'E '() #f)))
  
  (define voice-eventss-start (list voice-events-1 voice-events-2 voice-events-3 voice-events-4))
  (define voice-events-durs-start (map (lambda (voice-events) (apply + (map voice-event->duration-int voice-events))) voice-eventss-start))
  (define voices-start (map (lambda (voice-events) (PitchedVoice 'AcousticGrand voice-events)) voice-eventss-start))
  (define voices-stop (extend-voices-durations four-four-time-signature voices-start))
  (define voice-eventss-stop (map PitchedVoice-voiceevents voices-stop))
  (define voice-events-durs-stop (map (lambda (voice-events) (apply + (map voice-event->duration-int voice-events))) voice-eventss-stop))
  (define four-four-barlen (time-signature->barlen four-four-time-signature))

  (check-true
   (apply = voice-events-durs-stop)
   (format "unequal voice-events-durs-stop ~v" voice-events-durs-stop))
  (check-true
   (andmap (lambda (dur-start dur-stop) (>= dur-stop dur-start)) voice-events-durs-start voice-events-durs-stop)
   (format "voice-events-durs-stop ~v not all >= voice-events-durs-start ~v" voice-events-durs-stop voice-events-durs-start))
  (check-true 
   (andmap (lambda (dur-stop) (zero? (remainder dur-stop four-four-barlen))) voice-events-durs-stop)
   (format "voice-events-durs-stop not all visible by barlen ~v" four-four-barlen)))

