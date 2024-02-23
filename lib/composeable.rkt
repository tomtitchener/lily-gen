#lang racket

;; composeable: methods that compose

(require lily-gen/lib/scale)
(require lily-gen/lib/score)

;; start as simple:
;; - other config e.g. list of ints for transposition, durations, etc.
;;   gets passed as initial args with Context as last curried so only
;;   Context gets emitted and accepted at each stage in the pipeline
;; - a single voice chunked by stages in the pipeline to allow inspection
;;   of begin and end pitches for each previous stage e.g. for start pitch
;;   for this call
(struct/contract Context ([scale Scale?] 
                          [voices (listof (listof voice-event/c))]
                          )
                 #:transparent)

;; Where do durations enter into this?  Do I want this to be in terms of motifs instead?
(define/contract (comp-xpose start-pitch m-int-or-ints m-durs context)
  (-> pitch/c (listof maybe-interval-or-intervals/c) any/c Context? Context?)
  (let* ([scale (Context-scale context)]
         [pitch-range-pair (scale->pitch-range-pair scale)]
         [m-pitch-or-pitches (transpose/absolute scale pitch-range-pair start-pitch)])
    ;; tbd ...
    context))
