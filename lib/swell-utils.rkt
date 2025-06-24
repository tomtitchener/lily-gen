#lang racket

;; swell-utils: routines with no internal dependencies

(provide
 (contract-out
  [encode-voices-group-swells (-> VoicesGroup? VoicesGroup?)])
 )

(require lily-gen/lib/score)

;; internal search through list:
;; - pass events through until finding first dynamic
;;   or swell starting with default dynamic of MF,
;;   allowing for first note with both dynamic and swell,
;;   internal traversal args are
;;   a) current dynamic (always something, default MF),
;;   b) current swell (may be #f)
;;   c) list of voice-events contained within swell
;;      (e.g. with ties), initially '()
;;   d) and output voice-event list, initially '()'
;; 
;;   swell one of Crescendo Decrescendo SwellStop
;;
;; Below: instead of appending to output list, cons
;; to the output list and reverse result before return
;;
;; Below: seed current dyn with MF so there's never not
;; a current dyn'
;;
;; - per-voice-event (only Note to start, later Chord):
;;
;;   * if not Note, then just recur with existing arg
;;     values and new voice-event at end of list
;;
;;   (rest is implicitly for Note voice-event only)
;;
;;   * - Args: current dyn 
;;             no current swell (#f)
;;             no contained voices-events '()
;;             return list
;;       Vals: (or/c #f dynamic?)
;;             no new swell (#f)
;;       => Recur: (if (new dyn) (new dyn) else current)
;;                 #f
;;                 '()
;;                 cons voice-event to return list
;;
;;   * - Args: current dyn
;;             no current swell (#f)
;;             no contained voices-events '()
;;             return list
;;       Vals: (or/c #f dynamic?)
;;             new swell
;;       => Recur: if (new dyn) (new dyn) else current
;;                 new swell
;;                 (add event to contained events)
;;                 return-list as is
;;
;;       Start of new Crescendo or Decrescendo,
;;       or
;;       End of existing Crescendo or Decrescendo on SwellStop,
;;       new dynamic and non-empty list of contained voice-events
;;       or
;;       Error if SwellStop with no contained voice-events
;;       ?? but SwellStop explicitly avoids naming dynamic,
;;       ?? which invalidates adjustExpression!
;;
;;       Do this Later: check this only at conclusion 
;;       of swell by new dynamic
;; 
;;       Note:  examine duration of Note, if shorter
;;       than e.g. half note, accumulate note-event
;;       for emitting at end of swell as is
;;
;;
;;     - if no new dynamic and existing swell, recur
;;       with existing dynamic, new swell, add voice-event
;;       to end of list of contained voice-events, and
;;       existing output list (check list of voice-events
;;       contained within swell is not empty, should have
;;       picked on up with swell)
;;     - if no new dynamic and no existing swell, recur
;;       with existing args and add voice-event to end of
;;       output list
;;   * - if only find dynamic and not swell and existing
;;       swell is not #f, then this is dynamic-delineated
;;       end of swell, so emit new Expression voice-event
;;       ending swell and recur with new dynamic, no swell,
;;       and add event to end of output list, could also '
;;       check relationship between begin and end dynamic
;;       matches direction of swell (softer/louder)
;;     - if existing swell is not #f, recur with
;;       new swell, new dynamic (if present, else
;;       current dynamic), add event to end of output
;;       list, NB: this is start of swell, so maybe
;;       examine duration and pass if shorter than
;;       quarter note, e.g. let ordinary midi rendering
;;       handle dynamic differences between notes, will
;;       be a bit tricky as other intermediate states
;;       will have to filter on note duration e.g. even
;;       if one of notes contained in swell is longer
;;     - else time to frame swell 
;;
;; Note: mechanism would also apply to espressivo swell
;;       by chopping note duration into two tied notes
;;       with two adjustExpression frames and an interpolated
;;       midpoint based on the starting dynamic
;;       tricky bit would be leaving ordinary durations
;;       for score and replacing those with two adjustExpression
;;       frames for midi only
;;
;;       right now the only unique processing I have for the
;;       midi section is to merge SplitStaff into PitchedVoice,
;;       this would require some sort of awareness during
;;       encoding to LilyPond about context that doesn't
;;       already exist except in collapsing SplitStaff to
;;       Pitched, maybe a special encode-espressivo pass
;;       unique to the midi part of score->lily where I
;;       encode only espressivo notes with adjustExpression
;;       frames
;;
;;       meanwhile it should be good enough to prefix the
;;       prelude and postlude adjustExpression with #midi-tag
;;       or better yet, only filter all the conversions at
;;       the point of the midi branch, which is simpler, see
;;       lily.rkt:voices-group->midi-voices-group
;;   
(define/contract (encode-voice-events-swells ves)
  (-> (listof voice-event/c) (listof voice-event/c))
  (let loop ([current-dyn 'MF]
             [contained-ves '()]
             [voice-events ves]
             [ret '()])
    (cond
      [(empty? voice-events)
       (reverse ret)]
      [else
       (let ([next-ve (car voice-events)])
         (match next-ve
           ;; for now only allow Notes in a swell, later Chord, Tuplet, Rest?
           [(Note _ _ _ ctrls _)
            (let ([m-swell (findf swell? ctrls)]
                  [m-new-dyn (findf dynamic? ctrls)])
              (match m-swell
                [(or 'Crescendo 'Decrescendo)
                 (if (empty? contained-ves)
                     ;; starting a new swell
                     (begin
                       #;(printf "encode-voice-events-swells start a new swell, m-new-dyn: ~v, next-ve: ~v\n" m-new-dyn next-ve)
                       (loop (if m-new-dyn m-new-dyn current-dyn) (list next-ve) (cdr voice-events) ret))
                     ;; new swell ends existing one, must have a new dynamic
                     (begin 
                       (when (not m-new-dyn)
                         (error 'encode-voice-events-swells "new swell ~v with no new dynamic ends existing swell ~v"
                                next-ve contained-ves))
                       (let ([swell (Swell current-dyn contained-ves m-new-dyn)])
                         #;(printf "encode-voice-events-swells conclude Swell: ~v, m-new-dym: ~v next-ve: ~v\n" swell m-new-dyn next-ve)
                         (loop m-new-dyn (list next-ve) (cdr voice-events) (cons swell ret)))))]
                ['SwellStop
                 ;; swell stop doesn't give you a dynamic, dump contained-ves along with next-ve
                 (loop current-dyn '() (cdr voice-events) (append (cons next-ve contained-ves) ret))]
                [#f ;; no swell in ctrls for this note, look for new dynamic
                 ;; rethink logic:  first order driver should be if contained-ves is empty or not
                 ;; - if it's not empty, then there's a swell in there, so push it out intact first
                 ;;   and cons it to start of ret
                 ;;   THEN deal with next-ve and m-new-dyn by consing it to ret again
                 (if m-new-dyn
                     ;; new dyn, does it end current swell?
                     (if (empty? contained-ves)
                         (begin
                           ;; no, no current swell, copy Note across
                           #;(printf "encode-voice-events-swells no current swell m-new-dyn: ~v contained-ves is empty\n" m-new-dyn)
                           (loop m-new-dyn '() (cdr voice-events) (cons next-ve ret)))
                         ;; yes, new dynamic ends current swell, add new Swell voice event with contained ves
                         ;; this is wrong, m-new-dyn is for next note AFTER swell note, which is in contained-ves,
                         (let ([swell (Swell current-dyn contained-ves m-new-dyn)])
                           #;(printf "encode-voice-events-swells m-new-dyn: ~v contained-ves is not empty: ~v, add swell: ~v\n" m-new-dyn contained-ves swell)
                           (loop current-dyn '() (cdr voice-events) (cons next-ve (cons swell ret)))))
                     ;; not a new dyn, if currently in swell, cons to contained-ves, else copy across
                     (if (empty? contained-ves)
                         (begin
                           ;; no, no current swell, copy across
                           #;(printf "encode-voice-events-swells not m-new-dyn, contained-ves is empty\n")
                           (loop current-dyn '() (cdr voice-events) (cons next-ve ret)))
                         ;; yes, current swell, add to contained-ves
                         (begin
                           #;(printf "encode-voice-events-swells not m-new-dyn, contained-ves is not empty\n" contained-ves)
                           (loop current-dyn (cons next-ve contained-ves) (cdr voice-events) ret))))]))]
           [other-ve
            ;; VoiceEvent other than Note, copy across to ret unless we're in a swell which is an error
            (if (empty? contained-ves)
                (loop current-dyn '() (cdr voice-events) (cons other-ve ret))
                (error 'encode-voice-event-swells "voice-event in swell ~v that is not a Note: ~v" contained-ves other-ve))]))])))

(define/contract (encode-voice-swells voice)
  (-> voice/c voice/c)
  (match voice
    [(PitchedVoice instr pan voice-events)
     (PitchedVoice instr pan (encode-voice-events-swells voice-events))]
    [(KeyboardVoice instr pan voice-events-pr)
     (KeyboardVoice instr pan voice-events-pr (cons (encode-voice-events-swells (car voice-events-pr))
                                                    (encode-voice-events-swells (cdr voice-events-pr))))]
    [(SplitStaffVoice instr pan voice-events)
     (SplitStaffVoice instr pan (encode-voice-events-swells voice-events))])
  )
    
(define (encode-voices-group-swells voices-group)
  (struct-copy VoicesGroup voices-group [voices (map encode-voice-swells (VoicesGroup-voices voices-group))]))

