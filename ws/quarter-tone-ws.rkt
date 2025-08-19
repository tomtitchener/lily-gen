#lang racket
;; quater-tone-ws.rkt:  quarter tones

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)
(require lily-gen/lib/generators)

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
;;                  2              16    18      49             53      67    116    118     159     190     202

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
;;                  2              16    18      37              41      55    56       58     67       86       98

;; Total of absolute of all unadjusted differences for the first 16 overtones:  201
;; Total of absolute of all adjusted differences for the first 16 overtones:     98
;; For a difference of 104 cents better, or about half of the original sum of 202.

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
  (define restricted-octaves '(29vb 22vb 15vb 8vb 0va)) ;; has to have 4 octaves to go up from, range goes 8va, 15va, 22va.
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
  (define (add-sust note) (match note [(Note p o d cs t)
                                       (Note p o d (cons 'SustainOn cs) t)]))
  (cons (add-sust (car notes)) (cdr notes)))

(define (hold-last-note notes)
  (define (add-tie  note) (match note [(Note p o d cs _)
                                       (Note p o d cs #t)]))
  (let* ([inits (drop-right notes 1)]
         [last  (last notes)]
         [lasts (list (add-tie last) (add-tie last) (add-tie last) (add-tie last) last)])
    (append inits lasts)))

(define (make-voice instr pan clef notes) (PitchedVoice instr pan (cons clef notes)))

#;(define (score pc oct) (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                      (tempo/param (TempoDur 'Q 80)))
                         (let* ([notes (hold-last-note (sustain-all-notes (make-harmonics-notes (make-harmonic-pitches pc oct))))]
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

#;(define (make-swell-notess cnt-swells pitches indexes dur ctrlss end-dyn)
  (let ([pits (map (lambda (i) (list-ref pitches i)) indexes)])
    (map (lambda (pit)
           (append (map
                    (lambda (durs ctrls) (Note (car pit) (cdr pit) durs ctrls #t))
                    (drop-right durs 1) (drop-right ctrlss 1))
                   (list (Note (car pit) (cdr pit) (last durs) (last ctrlss) #f))))
         pits)))

#;(define (make-swell-notess cnt-swells pitches indexes dur ctrlss end-dyn)
  (let ([pits (map (lambda (i) (list-ref pitches i)) indexes)])
    (map (lambda (pit)
           (let* ([pc (car pit)]
                  [oct (cdr pit)]
                  [swell-up-ctrls (first ctrlss)]
                  [swell-down-ctrls (last ctrlss)]
                  [swells (flatten
                           (make-list
                            cnt-swells
                            (list (Note pc oct dur swell-up-ctrls #t)
                                  (Note pc oct dur swell-down-ctrls #t))))])
             (flatten (make-list cnt-swells (append swells (list (Note pc oct dur (list end-dyn) #f)))))))
         pits)))

(define (make-swell pit dur ctrlss)
  (let ([pc (car pit)]
        [oct (cdr pit)])
    (list (Note pc oct dur (first ctrlss) #t) (Note pc oct dur (last ctrlss) #t))))

(define (make-end pit dur end-dyn)
  (Note (car pit) (cdr pit) dur (list end-dyn) #f))

(define (make-swell-notess pitches indexes dur ctrlss end-dyn)
  (let* ([pits (map (lambda (i) (list-ref pitches i)) indexes)])
    (map (lambda (pit) (append (make-swell pit dur ctrlss) (list (make-end pit dur end-dyn)))) pits)))

(define (score pc oct) (parameterize ((time-signature/param (TimeSignatureSimple 4 'Q))
                                      (tempo/param (TempoDur 'Q 80))
                                      (instr/param 'Clarinet))
                         (let* ([pits (make-harmonic-pitches pc oct)]
                                [ixs (reverse   (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))]
                                [swell-ctrlss   (list (list 'PPPPP 'Crescendo) (list 'FFF 'Decrescendo))]
                                [swell-notess   (make-swell-notess pits ixs 'W swell-ctrlss 'PPPPP)]
                                [cresc-ctrlss   (list (list 'PPPPP 'Crescendo) (list 'FFF))]
                                [cresc-notess   (make-swell-notess pits ixs 'W cresc-ctrlss 'FFF)]
                                [decresc-ctrlss (list (list 'FFF 'Decrescendo) (list 'PPPPP))]
                                [decresc-notess (make-swell-notess pits ixs 'W decresc-ctrlss 'PPPPP)]
                                [all-notess (map append swell-notess cresc-notess decresc-notess)]
                                [pans   (voice-cnt->pan-distrib (length all-notess))]
                                [instrs (flatten (map (lambda (instr) (make-list 4 instr)) '(Flute Clarinet EnglishHorn Bassoon)))]
                                [clefs  (append (make-list 12 'Treble) (make-list 4 'Bass))])
                           (gen-score-file (score/parameterized (map make-voice instrs pans clefs all-notess))))))

;; Consider MD simulations and process of folding as model for morphing shape.

;; Messing with midiExpressionLevel or midiReverbLevel doesn't make a difference with Logic

;; Also, you have to careful letting a channel with pitch bend go to the end.
;; Interrupt the channel in mid-playback and you may get some wacky effects when you restart, lasting for duration of Logic session.

;; Also, dynamics aren't reliable, for example, swell to fff followed by a new attack on fff comes much louder the second time.
;; Lilypond says suppored dynamic range is ppppp to fffff, but ppppp isn't really soft.  Will need post-processing in Logic to;
; temper dynaics by instrument section.

;; 1 3 5 6 8 9 11 12 13 15
;; flute, flute, flute, clarinet, clarinet, clarinet, english horn, english horn, bassoon, bassoon

;; 2 4 6 7 9 10 12 14 16

;; routine to randomly walk over interval sequence
;;
;; - target is short phrases with longer rests between growing irregularly to longer phrases with shorter
;;   breaks that starts growing by adding to the previous phrase in bits until I reach a target length
;;   (approximate) at which point I drop from the the beginning of the pattern and add to the end, but
;;   also in irregular chunks so the total duration remains approximately as is, a little shorter or a
;;   little longer
;;
;; - range should wander while staying within limits, aggregate bits can include a rest but should always
;;   end in a note, so amount to grow / shrink should be relative to total length of previous snippet
;;
;; - snippet segments should be expressive, using accents, durations, and rests - maybe vary repetition
;;   of segments adding or removing dynamics, replacing some notes with rests, or reversing snippet
;;   segments
;;
;; - should be murmering patter less of a wall of sound than bittersweet chorus, with sour flavor of
;;   overtone series when I add bass voice swells, eventual shifting of harmony to adjacent tonal 
;;   centers by common upper overtones, ultimately a master generator to bring voices within two
;;   groups together internally, then the two together
;;
;; - take pitch classes from overtones 9 - 16 from a fundamental of C (D - C) as a scale with 8 elements
;;
;; - have solo voices in overlapping range:  flute, oboe, clarinet, horn.  The flute can cover the top two
;;   octaves.  The oboe goes from Bbb below middle C and up two octaves.  The clarinet goes two octaves from 
;;   E below middle C and up.  The horn covers the two octaves starting from the octave below middle C.
;;   Real flute and oboe don't do quarter tones at all, clarinet and horn a little more, so this is all fake.
;;
;; - have generators for: note vs. rest, interval, duration, accent with initial weights and starting pitch:
;;
;;   * generators have access to a sliding window of e.g the previous 10 notes-or-rests and adjust the weights
;;     for each iteration based on a score of the parameter (rest, interval, duration, accent) from those
;;     notes-or-rests (or maybe the master generator for all manages the list between iterations)
;;     but wait, see note-or-rest/generator, which seems to give me what I want, though each inner generator
;;     will have to manage history for its own parameter e.g. duration or accent
;;
;;   * interval gives range negative - positive for current position in range with weights initially
;;     set to prefer short hops, range reflects position of current or most recent pitch so if flute starts
;;     at C above middle C, range is -8..0..8 to cover two octaves, but if next note is up one to D, then
;;     the range shifts -9..0..7 and weights shift to favor return to center - except I don't want to skip
;;     more than say up or down a fourth, which limits the number of weights to 9 including no skip, though
;;     I have to take upper and lower limits in mind so that if as I approach the boundary the weights favor
;;     skipping in the other direction - 
;;
;; - top 10 or 11 pitches have small intervals, could have short segments strung together with rests in between,
;;   heuristics to determine duration, rest vs. note, interval direction +/-, interval size using sliding window
;;   with history of say previous 5 events?
;;
;; - or consider something completely different, like seeding each voice with a one or two example snippets and
;;   extending from there based on what's come before
;;
;; - first review motifs.rkt, which already approaches a lot of these ideas, then break it down to smaller pieces
;;

;; Just to get started from somewhere, consider weighted-maybe-intervalss-motifs/generator, where I supply a list
;; of motifs, probably FixedPitchMaybeIntervalsMotif and TupletMaybeIntervalsMotif
;;
;; Then spread those over a list of solo voices with short, quiet motifs, varying lengths and preceding and following
;; rests and then tackle swells in second chorus (note: I believe FixedPitch motifs won't transpose successively)
;;

;; first the flute starting at C an octave above middle C, these are to be randomly sequenced by weight
;; all will use "Scale" of last octave of overtone series, range of voice is two octaves

(define mur-ints1 (list '(#f ()               (E))  ;; RE DE DE EE RQ CE DE RQ (no quarter tones)a
                        '(1  (SlurOn)         (E))   
                        '(0  ()               (E))
                        '(1  (Accent SlurOff) (E))
                        '(#f ()               (Q))
                        '(-2 (Accent SlurOn)  (E))
                        '(1  (SlurOff)        (E))
                        '(#f ()               (Q))))

(define mur-ints2 (list '(#f ()               (E.))  ;; RE. BblS GE RS GflS ES EflS Es
                        '(6  (SlurOn)         (S))
                        '(-2 (Accent SlurOff) (E))
                        '(#f ()               (S))
                        '(-1 (Accent SlurOn)  (S))
                        '(2  ()               (S))
                        '(-2 (Accent)         (S))
                        '(2  (SlurOff)        (S))))

(define mur-ints3 (list '(#f ()               (Q))  ;; RQ EQ BblS CS BblS CS RQ
                        '(2  (SlurOn)         (Q))
                        '(-4 (Accent)         (S))
                        '(2  ()               (S))
                        '(-2 ()               (S))
                        '(2  (SlurOff)        (S))
                        '(#f ()               (Q))))

(define (mur-mots start)
  (list (list 1 (FixedPitchMaybeIntervalsMotif start mur-ints1))
        (list 1 (FixedPitchMaybeIntervalsMotif start mur-ints2))
        (list 1 (FixedPitchMaybeIntervalsMotif start mur-ints3))))

(define mur-mots-hi (mur-mots '(C . 8va)))

(define mur-mots-med (mur-mots '(G . 0va)))

(define mur-mots-lo (mur-mots '(D . 0va)))

(define top-octave-scale (Scale  (take (drop (map car (make-harmonic-pitches 'C '15vb)) 7) 8)))

(define mur-mots-hi/g (weighted-maybe-intervalss-motifs/generator top-octave-scale '(C . 8va) mur-mots-hi))
  
(define mur-mots-med/g (weighted-maybe-intervalss-motifs/generator top-octave-scale '(G . 0va) mur-mots-med))

(define mur-mots-lo/g (weighted-maybe-intervalss-motifs/generator top-octave-scale '(C . 0va) mur-mots-lo))

(define length<=? (lambda (m) (sum<=? (lambda (_) 1) m)))

(define (score-mots) (parameterize ((time-signature/param (TimeSignatureSimple 4 'Q))
                                    (tempo/param (TempoDur 'Q 80))
                                    (instr/param 'Flute))
                       (let* ([mur-notes-hi (flatten (while/generator->list (length<=? 10) mur-mots-hi/g))]
                              [mur-notes-med (flatten (while/generator->list (length<=? 10) mur-mots-med/g))]
                              [mur-notes-lo (flatten (while/generator->list (length<=? 10) mur-mots-lo/g))]
                              [voice-hi (PitchedVoice 'Flute 'PanLeft  (cons 'Treble  mur-notes-hi))]
                              [voice-med (PitchedVoice 'Flute 'PanCenter (cons 'Treble mur-notes-med))]
                              [voice-lo (PitchedVoice 'Clarinet 'PanRight (cons 'Treble mur-notes-lo))])
                         (gen-score-file (score/parameterized (list voice-hi voice-med voice-lo))))))

;; a failed experiment, twing-y sour sound is icky - looking back at Dennehy's Winter piece I can see and hear harmonies rooted
;; in octaves, fifths, and major thirds with careful blending in of high overtones against supporting tonal context so quarter
;; tones are color, with some step-over / step-back harmonization and some standard 3rd or 6th tonal relations with "revealed"
;; or "behold" effect from Romantics
