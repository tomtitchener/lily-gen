#lang racket

;; quater-tone-ws.rkt:  quarter tones

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)

;; first, just create a quarter tone scale up and down to see this looks right and sounds right

;; 0va ascending quarter tones
(define quarter-tone-pitches-up (map (lambda (pc) (cons pc '0va)) (Scale-pitch-classes quarter-tones-up)))

;; 1va descending first two quarter tones (C, Cl), then 0va for rest starting with B
(define quarter-tone-pitches-down (append
                                   (map (lambda (pc) (cons pc '8va)) (take (Scale-pitch-classes quarter-tones-down) 2))
                                   (map (lambda (pc) (cons pc '0va)) (drop (Scale-pitch-classes quarter-tones-down) 2))))

(define quarter-tone-notes-up (map (lambda (p) (Note (car p) (cdr p) 'T '() #f)) quarter-tone-pitches-up))

(define quarter-tone-notes-down (map (lambda (p) (Note (car p) (cdr p) 'T '() #f)) quarter-tone-pitches-down))

(define quarter-tone-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                         (flatten (list (KeySignature 'C 'Major)
                                                        quarter-tone-notes-up
                                                        quarter-tone-notes-down))))

#;(define quarter-tone-notes-score (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                                  (tempo/param (TempoDur 'E 40)))
                                     (gen-score-file (score/parameterized (list quarter-tone-voice)))))


;; from https://en.wikipedia.org/wiki/Harmonic_series_(music):
;; second, consider an expanded "scale" that covers the first 16 harmonics with quarter tone approximations,
;; which gets you to a more-or-less diatonic scale in three octaves with cents difference equal temperment
;;
;; 1        2      3       4      5      6      7        8      9      10     11      12     13      14      15      16
;; C 15vb | C 8vb, G 8vb | C 0va, E 0va, G 0va, Bb 0va | C 1va, D 1va, E 1va, Gf 1va, G 1va, Af 1va, Bb 1va, B 1va | C 2va
;;                 +2             -14    +2     -31             +4     -14    -49     +2     +41     -31     -12

;; 100 cents per semi-tone means 50 cents per quarter-tone, so the differences range from 48 to 1
;;
;; 1        2      3       4      5      6      7        8      9      10     11      12     13      14      15      16
;; C 15vb | C 8vb, G 8vb | C 0va, E 0va, G 0va, Bb 0va | C 1va, D 1va, E 1va, Gf 1va, G 1va, Af 1va, Bb 1va, B 1va | C 2va
;;                 +2             -14    +2     -31             +4     -14    -49     +2     +41     -31     -12
;;                 48              36    48      19             46      36      1     48       9      19      38
;;                 A               B     C       D              E       F       G      H       I       J       K
;;
;; A, C, E, H: +2/+4 are hardly audible, ordinary pitches are fine as is
;; D/K, G, I, J: between 19 and 1 quarter tones are good enough
;; B, F: hard, if -14 is audible, which I think it is, then that sort of
;;       invalidates D, K, and J leaving only G (easy) and I (9)
;;
;; That divides into the easy group of
;; A, C, E, G, H, and maybe I (6) vs. the hard group of
;; B, D, F, J, and K (5)

;; Which returns to the question of how hard it is to create a pitch with a particular offset in cents, where the
;; answer unfortunately appears to be very hard despite the hints at a function make-pitch, but trying to follow
;; up on creating your own pitch leads down an infinite rat hole.

;; So, given the closest I can come with quarter tones, what would the series look like?
;; Differences are vs. offset from just intonation, so third harmonic at G 8vb is 2 cents higher
;; so without alterations G 8vb is 2 cents too low.  Fifth harmonic at E 0va is 14 cents lower
;; so unaltered E 0va is 14 cents too high.  Seventh harmonic at Bb 0va is 31 cents lower, so
;; Bfl at 50 cents low is 19 cents too low, asterisks show quarter tone pitches.
;;
;;                                              *                              *               *       *
;; 1        2      3       4      5      6      7         8      9      10     11       12     13      14       15      16
;; C 15vb | C 8vb, G 8vb | C 0va, E 0va, G 0va, Bbl 0va | C 1va, D 1va, E 1va, Gfl 1va, G 1va, Al 1va, Bbl 1va, B 1va | C 2va
;;                 -2             +14    -2     -19              -4     +14    -1       -2     +9      -19      +12


;; Total of absolute of all unadjusted differences for the first 16 overtones:  202
;; Total of absolute of all adjusted differences for the first 16 overtones:     98
;; For a difference of 104 cents, or more than half of the original sum of 202.

;; What would it mean to have a "scale" made up of the first 16 overtones?
;; The difference between adjacent pitches goes from a lot to a little fast,
;; with the 1 to 4 spanning two octaves, then 4 to 16 two more.
;; Some scales have uniform steps throughout:  chromatic and wholetone.
;; The remaining major and minor only vary between wholetone ahd halftone steps
;; in a couple places.
;;
;; That means for a series of transpositions as integers, which is how I program motifs,
;; there's integer distance and interval distance are more or less the same regardless
;; of where you are in the scale.
;; 
;; I'll want a way to specify a harmonic scale for different tonics, which means as computed against
;; a super-chromatic framework of quarter tone chromatic scales, proably with adjustments for good
;; choices of quarter-tone enharmonics on a case-by-case basis?
;;
;; Most importantely, all scales fall within a single octave, whereas this new one spreads over four octaves,
;; about as much as many instrument can achieve and more than others, e.g trumpet usually spans between 2 1/2,
;; maybe 3.
;;
;; This means the harmonic scale has to count quarter tones and rendering one has to know when the count
;; traverses octave boundaries.
;;
;; 1        2      3       4      5      6      7         8      9      10     11       12     13      14       15      16
;; C 15vb | C 8vb, G 8vb | C 0va, E 0va, G 0va, Bfl 0va | C 1va, D 1va, E 1va, Gfl 1va, G 1va, Al 1va, Bfl 1va, B 1va | C 2va
;; 0        24     38      48     56     62     67        72     76     80     83       86     89      91       94      96

;; Maybe first it'd be easiest to make say a 20 octave quarter tone scale with pitch name and octave, then to index into
;; that from a root to get the harmonics for e.g. the C harmonic scale, though I guess properly I can do that programmatically
;; given the root and the count of enharmonics or just a list of quarter tone offsets from the root.

;; generate first 16 pitches low to high spanning four octaves, uses quarter-tones-down
;; octave arithmetic:  count up from start octave (quotient <interval> 24) by octave-syms index
;;   watching for result that's > (length octave-syms)
;; restriction:  pc has to be member of chromatic-sharps or chromatic flats, no quarter-tones
;; restriction:  oct has to be 29vb, 22vb, 15vb, 8vb, or 0va, else there's not room for four octaves
;; note: this cannot be a Scale because a Scale only contains pitch-classes, not pitches
;;       also, a Scale is limited to pitch classes within one octave and this spans four octaves
;;

;; only works for C
(define/contract (make-harmonic-pitches-old start-pc start-oct)
  (-> pitch-class? octave? (non-empty-listof pitch/c))
  (when (not (member start-pc (append (Scale-pitch-classes chromatic-sharps) (Scale-pitch-classes chromatic-flats))))
    (error 'make-harmonic-pitches "pitch class argument ~v is not a member of chromatic sharps ~v or chromatic flats ~v"
           start-pc chromatic-sharps chromatic-flats))
  (define harmonic-quarter-tone-intervals '(0 24 38 48 56 62 67 72 76 80 83 86 89 91 94 96))
  (define restricted-octaves '(29vb 22vb 15vb 8vb 0va))
  (let* ([base-oct-index (index-of restricted-octaves start-oct)]
         [pitch-classes (reverse (Scale-pitch-classes quarter-tones-down))]
         [start-pitch-index (index-of pitch-classes start-pc)]
         [rooted-pitch-classes (rotate-list-by pitch-classes start-pitch-index)])
    (when (not base-oct-index)
      (error 'make-harmonic-pitches "octave argument ~v is not one of ~v" start-oct restricted-octaves))
    (map (lambda (qtn-cnt) (let* ([quot (quotient qtn-cnt 24)]
                                  [this-oct (list-ref octave-syms (+ base-oct-index quot))]
                                  [rem  (remainder qtn-cnt 24)]
                                  [this-pc (list-ref rooted-pitch-classes rem)])
                             (cons this-pc this-oct)))
         harmonic-quarter-tone-intervals)))

;; always uses flats even for sharp keys
(define/contract (make-harmonic-pitches-two start-pc start-oct)
  (-> pitch-class? octave? (non-empty-listof pitch/c))
  (when (not (member start-pc (append (Scale-pitch-classes chromatic-sharps) (Scale-pitch-classes chromatic-flats))))
    (error 'make-harmonic-pitches "pitch class argument ~v is not a member of chromatic sharps ~v or chromatic flats ~v"
           start-pc chromatic-sharps chromatic-flats))
  (define harmonic-quarter-tone-intervals '(0 24 38 48 56 62 67 72 76 80 83 86 89 91 94 96))
  (define restricted-octaves '(29vb 22vb 15vb 8vb 0va))
  (let* ([start-oct-index (index-of restricted-octaves start-oct)]
         [pitch-classes (rotate-list-by (reverse (Scale-pitch-classes quarter-tones-down)) -2)]
         [start-pitch-index (index-of pitch-classes start-pc)])
    (when (not start-oct-index)
      (error 'make-harmonic-pitches "octave argument ~v is not one of ~v" start-oct restricted-octaves))
    (map (lambda (qtn-cnt) (let* ([off-qtn-cnt (+ start-pitch-index qtn-cnt)]
                                  [quot (quotient off-qtn-cnt 24)]
                                  [this-oct (list-ref octave-syms (+ start-oct-index quot))]
                                  [rem  (remainder off-qtn-cnt 24)]
                                  [this-pc (list-ref pitch-classes rem)])
                             (cons this-pc this-oct)))
         harmonic-quarter-tone-intervals)))

#;(eq? #\s (last (string->list (symbol->string 'Fs))))

(define (pick-quarter-tones-pitch-classes pc)
  (match (last (string->list (symbol->string pc)))
    [#\s (Scale-pitch-classes quarter-tones-up)]
    [#\f (rotate-list-by (reverse (Scale-pitch-classes quarter-tones-down)) -2)]
    [(or #\G #\D #\A #\E #\B) (Scale-pitch-classes quarter-tones-up)]
    [(or #\C #\F) (rotate-list-by (reverse (Scale-pitch-classes quarter-tones-down)) -2)]))

(define/contract (make-harmonic-pitches start-pc start-oct)
  (-> pitch-class? octave? (non-empty-listof pitch/c))
  (when (not (member start-pc (append (Scale-pitch-classes chromatic-sharps) (Scale-pitch-classes chromatic-flats))))
    (error 'make-harmonic-pitches "pitch class argument ~v is not a member of chromatic sharps ~v or chromatic flats ~v"
           start-pc chromatic-sharps chromatic-flats))
  (define harmonic-quarter-tone-intervals '(0 24 38 48 56 62 67 72 76 80 83 86 89 91 94 96))
  (define restricted-octaves '(29vb 22vb 15vb 8vb 0va))
  (let* ([start-oct-index (index-of restricted-octaves start-oct)]
         [pitch-classes (pick-quarter-tones-pitch-classes start-pc)]
         [start-pitch-index (index-of pitch-classes start-pc)])
    (when (not start-oct-index)
      (error 'make-harmonic-pitches "octave argument ~v is not one of ~v" start-oct restricted-octaves))
    (map (lambda (qtn-cnt) (let* ([off-qtn-cnt (+ start-pitch-index qtn-cnt)]
                                  [quot (quotient off-qtn-cnt 24)]
                                  [this-oct (list-ref octave-syms (+ start-oct-index quot))]
                                  [rem  (remainder off-qtn-cnt 24)]
                                  [this-pc (list-ref pitch-classes rem)])
                             (cons this-pc this-oct)))
         harmonic-quarter-tone-intervals)))

;; keep pitches ordered between C and B to get the octaves right
;; find starting point in reversed quarter-tones-down then count up quarter tones from there,
;; wrapping to new octave for pitch class and counting octaves from base of C to B


(define (make-harmonics-notes pitches) (map (lambda (p) (Note (car p) (cdr p) 'H '() #f)) pitches))

(define (sustain-all-notes notes)
  (define (add-sust note) (match note [(Note p o d cs t) (Note p o d (cons 'SustainOn cs) t)]))
  (cons (add-sust (car notes)) (cdr notes)))

(define (make-voice notes) (PitchedVoice 'AcousticGrand 'PanCenter (cons (KeySignature 'C 'Major) notes)))

(define (score pc oct) (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                      (tempo/param (TempoDur 'Q 80)))
                         (let* ([notes (sustain-all-notes (make-harmonics-notes (make-harmonic-pitches pc oct)))]
                                [voice (make-voice notes)])
                           (gen-score-file (score/parameterized (list voice))))))

(module+ test
  (require rackunit)
  (define explicit-c-harmonics-pitches
    (list (cons 'C '15vb)
          (cons 'C '8vb) (cons 'G '8vb)
          (cons 'C '0va) (cons 'E '0va) (cons 'G '0va) (cons 'Bfl '0va)
          (cons 'C '8va) (cons 'D '8va) (cons 'E '8va) (cons 'Gfl '8va) (cons 'G '8va) (cons 'Al '8va) (cons 'Bfl '8va) (cons 'B '8va) (cons 'C '15va)))
  (check-equal? (make-harmonic-pitches 'C '15vb) explicit-c-harmonics-pitches) 
  )


