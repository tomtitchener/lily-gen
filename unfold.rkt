#lang racket

;; unfold.rkt:  experiment with recursive patterns via unfold
;; intent is generally to produce increasingly elaborate
;; variations on entire list, maybe up to list of empty lists?
;; model is something like a tree branching structure, with a
;; rapidly ascending line that flattens with a descending and
;; ascending tail that recurs half-way up with a replica,
;; and etc. again for say three generations
;; not sure how number ranges would signify, for proportions,
;; 

(require (only-in srfi/1 unfold))

(require (only-in seq iterate take))

(require relation/type)

(provide iter-sum)

;; srfi-1
;; unfold p f g [tail-gen] -> list
;; p determines when to stop unfolding
;; f maps each seed value to the corresponding list element
;; g maps each seed value to the next seed value
;; seed
;; [tail-gen] is an optional tail-of-list, default '()
;;

(define (p seed)
  (> seed 3))

(define (f seed)
  (make-list seed seed))

(define (g seed)
  (+ 1 seed))

(define (test seed)
  (unfold p f g seed))

;; trial.rkt> (test 0)
;; '(() (1) (2 2) (3 3 3))

(define (p2 seed)
  (null? seed))

(define (f2 seed)
  (make-list (car seed) seed))

(define (g2 seed)
  (cdr seed))

(define (test2 seed)
  (unfold p2 f2 g2 seed))

;; trial.rkt> (test2 '(1 2 3))
;; '(((1 2 3)) ((2 3) (2 3)) ((3) (3) (3)))

(define (f3 seed)
  (cons (car seed) (test3 (cdr seed))))

;; for elaboration of seed, cdr eats through 
;;   (1 2 3 4) as (2 3 4), (3 4), (4), '()
;;   seeds are (1 2 3 4), (2 3 4), (3 4), (4), '()
;; recursion on v4 -> test3 restarts with remainder, first (2 3 4)
;;   yielding (1 (2 (3 (4)))) and etc., previewing remainder of
;;   top-level output elements with nesting
;;
(define (test3 seed)
  (unfold null? f3 cdr seed))
;;
;; trial.rkt> (test3 '(1 2 3 4))
;; '(
;;    (1 (2 (3 (4)) (4)) (3 (4)) (4))
;;    (2 (3 (4)) (4))
;;    (3 (4))
;;    (4)
;;  )
;;
;; length of seed list must diminish on recursion to work with null?
;;   as termination predicate
;;

;; could capture generations with a pair for seed (<gen> . <list>),
;; then p could test (car seed) for the desired number of generations,
;; f would answer (h (cdr seed)) to omit <gen> from result, and
;; g would answer (add1 (car seed),h (cdr seed)) to advance both the
;; generation and the unfolding of seed, e.g.:
;;

;; to test for termination given seed, check if first value is zero
(define p4
  (compose1 zero? car))

;; for inner recursion, call g4 here
(define (f4 seed)
  (let [(lst   (cdr seed))
        (seed* (g4 seed))]
    (cons (car lst) (test4 seed*))))

;; to update seed, decrement generation count, take cdr of input list
(define (g4 seed)
  (let [(gen (car seed))
        (lst (cdr seed))]
    (cons (sub1 gen) (cdr lst))))

;; tbd: contract to verify generation count in seed is <= length of list
(define (test4 seed)
  (unfold p4 f4 g4 seed))

;; point-free:  bimap (compose1 sub1 cdr) (compose1 cdr cdr)
;; see racket/function or point-free modules, or write my
;; own macro e.g. for a pair constructor, or maybe there's
;; a simple way to duplicate a single input argument

(define (dup x)
  (cons x x))

(define (bimap f g p)
  (cons (f (car p)) (g (cdr p))))
                    
(define (g4pf p)
  (bimap sub1 cdr p))

;; good exercise to remind myself that this '(1 . (2 3 4))
;; is *literally* the same thing as '(1 2 3 4) because the
;; end of that '.' operation is a nil terminated list

;; let-over-lambda pattern works in Racket

(define dispatcher
  (let ((state 1))
    (lambda (msg)
      (case msg
        ((print) (lambda () (print state)))
        ((inc!)  (lambda () (set! state (add1 state))))))))

(define print-state (dispatcher 'print))
(define inc-state (dispatcher 'inc!))

;; cloned from Haskell in /Archive/Dechert/Development/OldHaskell/Test.hs:
;;
;;data VoiceGen = VoiceGen {
;;      voiceGenKernel :: [Int]		   -- kernel list
;;      , voiceGenInit :: [Int]		   -- initializer list
;;      , voiceGenIter :: Int		   -- iteration count
;;      , voiceGenOp   :: (Int -> Int -> Int) -- operation, e.g. (+)
;;      , voiceGenOff  :: Int		   -- offset
;;      , voiceGenDur  :: Duration.T          -- duration
;;      }
;;
;; --melodyFromVoiceGen :: VoiceGen -> Music.T (Melody.Note ())
;; melodyFromVoiceGen :: VoiceGen -> Melody.T ()
;; melodyFromVoiceGen vgen = 
;;     line notes
;;     where
;;       notes = map (\n -> note n dur ()) pitches
;;       pitches = map fromInt absPitches
;;       absPitches = concat $ Prelude.take iter absPitchIter
;;       absPitchIter = iterate absPitchGen init
;;       absPitchGen = (\abs -> [off + (k `op` i) | k <- kern, i <- abs]) 
;;       kern = voiceGenKernel vgen
;;       init = voiceGenInit vgen
;;       iter = voiceGenIter vgen
;;       off = voiceGenOff vgen
;;       dur = voiceGenDur vgen
;;       op = voiceGenOp vgen

;; I *cannot* figure out what contract should look like for iter-fun and iter-sum
(define (iter-fun offset kern)
  (lambda (inits)
    (for*/list ([k kern] [i inits])
      (+ offset k i))))

(define (iter-sum generations offset kern inits)
  (->list (take generations (iterate (iter-fun offset kern) inits))))

;; Racket/Haskell equivalents:
;; unfold.rkt> (iter-sum 3 '(1 5 1) '(1 1))
;; '((1 1) (2 2 6 6 2 2) (3 3 7 7 3 3 7 7 11 11 7 7 3 3 7 7 3 3))
;;
; ;ghci> take 3 $ iterate (\abs -> [(k + i) | k <- [1,5,1], i <- abs]) [1,1]
;; take 3 $ iterate (\abs -> [(k + i) | k <- [1,5,1], i <- abs]) [1,1]
;; [[1,1],[2,2,6,6,2,2],[3,3,7,7,3,3,7,7,11,11,7,7,3,3,7,7,3,3]]

;; lengths of generations:
;; > (map length (iter-sum 5 0 '(1 5 1) '(1 1)))
;; '(2 6 18 54 162)
;; 
;; note that lengths of generations are:
;; - length of inits
;; - length of inits * length of kern
;; - length of inits * length of kern * length of kern
;; - length of inits * length of kern * length of kern * length of kern
;; e.g.
;; - (1 1)                                   # 2          # 2 * 3^0
;; - (2 2 6 6 2 2)                           # 2 * 3      # 2 * 3^1
;; - (3 3 7 7 3 3 7 7 11 11 7 7 3 3 7 7 3 3) # 2 * 3 * 3  # 2 * 3^2
;; e.g.
;; (length of inits) * ((length of kern)^(generation - 1))
;;
;; so the multipliers in count from generation to generation in '(2 6 18 54 162) are all (* (#prev gen) 3) or 3x
;; so if you picked the '(2 6 18 54 162) generations to map into equal total durations and you started with
;; 128th notes for the 162 generation, then next slowest would be a dotted 64th note, then three of those would
;; be a tied (S HTE) pair, three of those a tied (E. SF.) pair then '(H E HTE) then '(W. Q. SF.) for a series
;; '(HTE SF. (S HTE) (E. SF.) (H E HTE)) which is pretty ugly, though maybe you'd want to bar it in 9/16 or something?
;; or maybe for sanity's sake you'd want to take the maximum generation count in 64th notes instead of 128ths, though
;; you'd probably keep the time signature in some sort of 6/32 or 9/32.
;; 
