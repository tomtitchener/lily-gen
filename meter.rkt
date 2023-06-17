#lang racket

;; * add rests to the end of all voices to fill in the last measure
;;   for the longest voice if necessary
;; * align durations in each VoicesGroup voice to reflect meter
;;   e.g. by adding ties
;; assumes:
;; * durations for voice-events in voice aren't optimized for beats
;;   in the bar or bar length
;; * voices may be of different lengths
;;
;; * total duration voice-events without ties for the output equals
;;   total duration of voice-events without ties for the input

(provide
 (contract-out
  [extend&align-voices-group-durations (-> VoicesGroup? VoicesGroup?)]))

;; - - - - - - - - -
;; implementation
(require srfi/1)

(require racket/generator)

(require "score.rkt")

(require "score-utils.rkt")

(require "generators.rkt")

(require "lily-utils.rkt")

;; convert the meter or meters in time-signature to a cycle of SimpleTimeSignature
(define/contract (time-signature->num-denom-generator timesig)
  (-> time-signature/c generator?)
  (match timesig
    ;; TimeSignatureSimple is just a single-item list of a num denom pair
    [(TimeSignatureSimple num denom)
     (cycle-generator (list (cons num denom)))]
    ;; TimeSignatureGrouping is a list of num denom pairs, one for each of nums
    [(TimeSignatureGrouping nums _ denom)
     (let ([num-denom-prs (map (lambda (num) (cons num denom)) nums)])
       (cycle-generator num-denom-prs))]
    ;; TimeSignatureGrouping is the concatenated list of list of num denom pairs
    ;; with each inner list containing one list of num denom pairs per outer list
    ;; of one or nums that ends with a denom
    [(TimeSignatureCompound nums-denoms)
     (let* ([num-denom-prss (map (lambda (nums-denom)
                                   (let ([nums (take nums-denom (- (length nums-denom) 1))]
                                         [denom (last nums-denom)])
                                     (map (lambda (num) (cons num denom)) nums)))
                                 nums-denoms)])
       (cycle-generator (apply append num-denom-prss)))]))

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
(define/contract (add-end-durs-for-time-sig timesig tot-curlen tot-addlen)
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
(define/contract (spread-durs note-or-chord durs)
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
           (list note-or-chord)]
          [(= 2 (length durs))
           (cons (copy-note-or-chord note-or-chord (car durs) (note-or-chord->controls note-or-chord) #t)
                 (list (copy-note-or-chord note-or-chord (cadr durs) '() #f)))]
          [else
           (let* ([start    (copy-note-or-chord note-or-chord (car durs) (note-or-chord->controls note-or-chord) #t)]
                  [mid-durs (take (cdr durs) (- (length (cdr durs)) 1))]
                  [middle   (map (lambda (dur) (copy-note-or-chord note-or-chord (car durs) '() #t)) mid-durs)]
                  [end      (copy-note-or-chord note-or-chord (last durs) '() #f)])
             (cons start (append middle (list end))))])))

;; using time-signature and list of voice-events, arrange the durations
;; to reflect the inner beat and measure divisions
;; * for list of one or more rests, aggregate the duration for all rests
;;   and emit one rest per duration because there are no ties or controls
;; * for list of one or more other voice events, split individual note
;;   or chords into a list of tied note or chords, suppressing the controls
;;   for the inner (tied) note or notes
(define/contract (align-voice-events-durations time-sig voice-events)
  (-> time-signature/c (listof voice-event/c) (listof voice-event/c))
  (let ([ve-groups (group-by Rest? voice-events)])
    (define (adjvedurs voice-event pr)
      (let ([curlen (car pr)]
            [ves    (cdr pr)])
        (cond [(Note? voice-event)
               (let* ([addlen     (duration->int (Note-dur voice-event))]
                      [durs       (add-end-durs-for-time-sig time-sig curlen addlen)]
                      [notes      (spread-durs voice-event durs)])
                 (cons (+ curlen addlen) (append ves notes)))]
              [(Chord? voice-event)
               (let* ([addlen (duration->int (Chord-dur voice-event))]
                      [durs   (add-end-durs-for-time-sig time-sig curlen addlen)]
                      [chords (spread-durs voice-event durs)])
                 (cons (+ curlen addlen) (append ves chords)))]
              [else
               ;; Tuplet: treat integrally with durs as is
               ;; KeySignature, clef: zero-dur from voice-event->duration-int
               (let ([addlen (voice-event->duration-int voice-event)])
                 (cons (+ curlen addlen) (list voice-event)))])))
    (define (adjvesdurs ve-group pr)
      (let ([curlen (car pr)]
            [ves    (cdr pr)])
        (cond [(Rest? (car ve-group))
               (let* ([addlen (apply + (map (compose duration->int Rest-dur) ve-group))]
                      [durs   (add-end-durs-for-time-sig time-sig curlen addlen)]
                      [rests  (map Rest durs)])
                 (cons (+ curlen addlen) (append ves rests)))]
              [else
               (let ([pr2 (foldl adjvedurs (cons curlen '()) ve-group)])
                 (cons (car pr2) (append ves (cdr pr2))))])))
    (flatten (cdr (foldl adjvesdurs (cons 0 '()) ve-groups)))))

(define/contract (align-voice-durations time-sig voice)
  (-> time-signature/c voice/c voice/c)
  (match voice
    [(PitchedVoice instr voice-events)
     (PitchedVoice instr (align-voice-events-durations time-sig voice-events))]
    [(KeyboardVoice instr voice-events-pr)
     (KeyboardVoice instr (cons (align-voice-events-durations time-sig (car voice-events-pr))
                                (align-voice-events-durations time-sig (cdr voice-events-pr))))]
    [(SplitStaffVoice instr voice-events)
     (SplitStaffVoice instr (align-voice-events-durations time-sig voice-events))]))
  
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
