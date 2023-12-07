#lang racket

(require (only-in algorithms repeat))

(require racket/generator)

(require lily-gen/lib/workspace)

(require lily-gen/lib/scale)

(require lily-gen/lib/score)

(require lily-gen/lib/utils)

(require lily-gen/lib/generators)

;; descending-intervals-workspace.rkt

;; Experiment with octatonic, whole-tone, and mixed diatonic scales
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
