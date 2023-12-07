#lang racket

;; sparse-workspace.rkt

(require racket/generator)

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/generators)

;; * start with sparse texture with delicate motifs spread over wide range,
;;   means lots of fixed-pitch sequences otherwise successive transposition fails
;;   - lots of tuplets
;;   - combine random sequences of fixed-pitch dancing tuplets with sequential
;;     transposing chord/pitch strings, maybe with starting intervals themselves
;;     drawn from weighted random distribution
;;   - start on offbeats, first beat, but keep integral unit of duration e.g.
;;     three quarter notes for each

;; want helper routines to fit e.g. pitch sequence to same/similar rhythm
;; or other ways to have a family of related motifs

;; possible to string together a series of transposing motifs e.g. as a list
;; of TupletMotif with maybe-intervalss-motif/c contents

;; or even to have a list of TupletMotif some with transposing motifs maybe
;; with intervening FixedPitchMaybeIntervalsMotif with customized start pitches

;; implies unit of work is (listof maybe-intervalss-motifs/c), and maybe
;; utility routines to generate components with this as unit

;; Routines to generate two classes of motifs:
;;
;; - transposable:  ka-chunka motor rhythms, mixed single pitch and chords,
;;   randomly-selected weighted list of rhythms, randomly-selected count
;;   of rhythms within limited range, maybe to grow longer/shorter?
;;   variable overall register drift maybe as varying initial interval?
;;
;; - fixed: arpeggio type, wide-register span, semi-regular interval motifs,
;;   lots of tuplets with varying ratios, moving pretty quick, quiet staccato
;;   but also with accents, ascending and descending in stages i.e. with some
;;   reverse direction, light-weight, lots of upbeat
;;
;; - density/clumping:  what about rests, distribution of two motif types
;;   between voices, count of voices?  mix two types in all voices, start
;;   with low-density isolated motifs with max of one overlap, lots of silence
;;   between motifs, grow irregularly to more clumped overlap, maybe stopping
;;   short of continuous before backing out?
;;
;; maybe impossible to say what overall shape will be without concrete motifs
;; to work with, see how they could expand/accrete, start with routines to do
;; two types and see what happens next
;;

;; Motor motifs:
;;
;; - how to arrange accents so they play off/enhance meter, is it enough to pick
;;   a division in twos vs. in threes or should there be a sense of the beats in
;;   a bar, or, is larger-rhythmic grouping like the bar or multiple bars part of
;;   growing clumps of motifs together as the piece progresses .. start simple with
;;   2 vs. 3, string together motifs with same choice .. maybe rewrite accents
;;   later with longer sequence, positioning with respect to bar, make 2 vs. 3 part
;;   of parameterization so e.g. generate count of repeats in 2s|3s, applying accents
;;   based on 2s|3s though not uniformly, with optional pickup, trailing beats
;;
;;   or not .. my impulse is to program *everything* which can lead to an endless
;;   amount of design and coding .. better to just manually put together a series
;;   of durations for the 2 and the 3 division patterns and randomly weight those
;;   then if that's good but not rich enough  I can consider coding .. note the
;;   rhythms will include rests
;;   
;; - do I have a separate weighted list of lists of pitches, and if I do how do I
;;   know there'll be enough to match all the non-rests in the durations lists ..
;;   for now just write down pitch lists including repetitions, put them into
;;   weighted lists, and pick as many as I need given the length of non-rests in
;;   the list of durations .. motifs will be preserved, if mapped on different
;;   rhythms .. and if that's too abstract then dial it back to manual lists of
;;   maybe pitch + duration (though actually these are all interval, not pitch)
;;

;; where do I want to carry rests .. with duration or interval .. ugh, for this to
;; work where I can write an upbeat/pickup motif, I have to explicitly code it all,
;; duration and maybe-interval .. only thing that gets preserved is the collection
;; of motifs with two-fold subdivision vs. three-fold subdivision .. I can still
;; keep them pretty atomized and have a generator that emits different length
;; lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; two sets of these, one with tuple subdivision, one with triple
;; these are all transposed
(define doubles-motifs/param
  (thunk
   (list (list 1 (list (list #f '() '(Q E S))
                       (list (list 0) '() '(S))
                       (list (list 2) '(Accent) '(S))
                       (list (list 0) '() '(S))
                       (list (list 3 5) '(Accent) '(S))
                       (list (list 0 5) '() '(S)))) ;; (upbeat) 3 Q, up 5 steps
         (list 1 (list (list (list 1) '() '(E))
                       (list (list -1) '(Accent) '(S))
                       (list #f '() '(S))
                       (list (list -1) '(Accent) '(S))
                       (list (list -1) '() '(S))
                       (list (list -1) '() '(E)))) ;; 2Q, down 3 steps
         (list 1 (list (list (list -1 -3) '() '(S))
                       (list (list  0 -3) '() '(S))
                       (list (list -1 -3) '() '(E))
                       (list (list  0 -3) '() '(S))
                       (list (list -1 -3) '() '(S))
                       (list (list  0 -3) '() '(E)))) ;; 2Q down 3 steps
         )))

;; Watch out, extra tricky with descending chords
;; inside a motif because transpose/successive
;; doesn't sort e.g. low-high. 
;;
;; On the whole though it works out pretty well.
;; When following an upward notated chord with
;; positive integers, the transposition continues
;; from the lowest pitch.
;; When following a downward notate dhcord with
;; negative integers, the transposition continues
;; from the highest pitch.
;;
;; (list 1 (list (list (list -1 -3) '() '(S))
;;               (list (list  0 -3) '() '(S))
;;               (list (list -1 -3) '() '(E))
;;               (list (list  0 -3) '() '(S))
;;               (list (list -1 -3) '() '(S))
;;               (list (list  0 -3) '() '(E)))) ;; 2Q down 3 steps
;;
;; where (list -1 -3) shows chord, is easier to follow as (list -3 -1)
;; because -3 is going to be lowest pitch for what follows (list -3 0)
;; except ... that's not what happens, ugh, this is bad
;;
;; ordering lowest to highest happens when you construct a chord
;; but what I have here is a list of intervals, which get passed
;; verbatim into transpose/unguarded
;;
;; but first, there's transpose/successive, where there's (op-maybe list-sum)
;; where I always choose the first element in the list for the sum to apply

(define tuplets-motifs/param
  (thunk
   (list (list 1 (TupletMaybeIntervalsMotif 3 2 'Q
                   (FixedPitchMaybeIntervalsMotif
                    (cons 'C '0va)
                    (list (list (list 0) '(PPPP Staccatissimo) '(E))
                          (list (list 2) '(Staccatissimo)       '(E))
                          (list #f       '(Staccatissimo)       '(E))
                          (list #f       '(Staccatissimo)       '(E))
                          (list (list 2) '(Staccatissimo)       '(E))
                          (list (list 2) '(Staccatissimo)       '(E))
                          (list (list 2) '(Accent Staccatissimo) '(E))
                          (list (list 2) '(Staccatissimo)       '(E))
                          (list (list 2) '(Staccatissimo)       '(E))))))
         (list 1 (list (list #f '() '(Q))))
         (list 1 (TupletMaybeIntervalsMotif 5 4 'Q
                   (FixedPitchMaybeIntervalsMotif
                    (cons 'C '15va)
                    (list (list #f           '() '(S))
                          (list (list 0)     '(PPPP Staccatissimo) '(S S))
                          (list (list -2 -4) '(Staccatissimo) '(S))
                          (list (list -2 -4) '(Staccatissimo) '(S))
                          ;; 32nds next?
                          (list (list -2 -4) '(Staccatissimo) '(T))
                          (list (list -2 -4) '(Staccatissimo) '(T))
                          (list (list -2 -4) '(Staccatissimo) '(S))
                          (list (list -2 -4) '(Staccatissimo) '(T))
                          (list (list -2 -4) '(Staccatissimo) '(T))
                          (list (list -2 -4) '(Staccatissimo) '(S))
                          (list #f           '(Staccatissimo) '(S))))))
         (list 1 (list (list #f '() '(Q))))
         )))

;; next: motif-generators, regular repetition of fixed-pitch motifs gets
;; tiresome quickly, need larger repertory of motifs that can be programed
;; to grow (maybe by accretion?) or change

(struct/contract VoiceParams ([instr       instr?]
                              [scale       Scale?]
                              [start-pitch pitch/c]
                              [motifs      weight&maybe-intervalss-motifs/c]))

(define (voiceparam-values vps)
  (values (VoiceParams-instr vps)
          (VoiceParams-scale vps)
          (VoiceParams-start-pitch vps)
          (VoiceParams-motifs vps)))

(define/contract (voice-events->generator/VoiceParams voice-params)
  (-> VoiceParams? generator?)
  (let-values ([(_ scale start-pitch motifs) (voiceparam-values voice-params)])
    (weighted-maybe-intervalss-motifs/generator scale start-pitch motifs)))

(define start-pitch/param (make-parameter (cons 'C '0va)))

(define voice/param
  (thunk (VoiceParams (piano/param)
                      (scale/param)
                      (start-pitch/param)
                      (doubles-motifs/param))))

(define voices-params/param (make-parameter (list (voice/param))))

(define/contract voice-param-voices/parameterized
  (-> (listof voice/c))
  (thunk
   (let* ([notes-or-restss/gens (map voice-events->generator/VoiceParams (voices-params/param))]
          [notes-or-restss      (map (lambda (gen)
                                       (apply append (while/generator->list (sum<=? (const 1) (count/param)) gen)))
                                     notes-or-restss/gens)]
          [voice-eventss        (map add-key-signature/parameterized notes-or-restss)]
          [instrs               (map VoiceParams-instr (voices-params/param))])
     (map (lambda (instr ves) (SplitStaffVoice instr ves)) instrs voice-eventss))))

;; Whole-tone is much more interesting to listen to than C-major
#;(parameterize
  ((count/param 20)
   (start-pitch/param (cons 'As '8va))
   (voices-params/param (list (VoiceParams (piano/param) C-whole-tone (start-pitch/param) (tuplets-motifs/param)))))
  (gen-score-file (score/parameterized (voice-param-voices/parameterized))))

