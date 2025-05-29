#lang racket

;; ravel-ws.rkt:  encode Ravel textures
;;

(require lily-gen/ws/workspace)
(require lily-gen/lib/utils)
(require lily-gen/lib/motifs)
(require lily-gen/lib/score)
(require lily-gen/lib/scale)
(require lily-gen/lib/pan-utils)

;; Une barque sur l'ocean

(define arpeggio-pitches-1 (transpose/successive
                            Fs-minor
                            (scale->pitch-range-pair Fs-minor)
                            (cons 'Fs '8vb)
                            '(0 4 2 3 2  2 3 -3 -2 -2 -3 -2)))

(define arpeggio-pitches-2 (transpose/successive
                            Fs-minor
                            (scale->pitch-range-pair Fs-minor)
                            (cons 'Fs '8vb)
                            '(0 4 2 3 -3 -2 -4 4 2 3 -3 -2)))

(define arpeggio-controlss-1 '((SustainOn PPP SlurOn) () () (Accent) () ()
                               (Accent) () () (Accent) () (SlurOff)))

(define arpeggio-controlss-2 '((SustainOn PPP SlurOn) () ()
                               (Accent) () () 
                               (Accent) () ()
                               (Accent) () (SlurOff)))

(define (gen-notes pitches controls)
  (map (lambda (p cs) (Note (car p) (cdr p) 'T cs #f)) pitches controls))

(define arpeggio-notes-1 (gen-notes arpeggio-pitches-1 arpeggio-controlss-1))

(define arpeggio-notes-2 (gen-notes arpeggio-pitches-2 arpeggio-controlss-2))

(define arpeggio-tuplet-1 (Tuplet 12 8 'Q arpeggio-notes-1))

(define arpeggio-tuplet-2 (Tuplet 12 8 'Q arpeggio-notes-2))

(define arpeggios-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                      (list (KeySignature 'Fs 'Minor)
                                            arpeggio-tuplet-1
                                            arpeggio-tuplet-2)))

(define chords-pitches (transpose/successive
                        Fs-minor
                        (scale->pitch-range-pair Fs-minor)
                        (cons 'A '8va)
                        '((0 4) (-1 3) (0 3) (-4 4) (2 1))))

(define chords-controlss '((SustainOn PPP SlurOn) () () () (SlurOff)))

(define chords-ties '(#f #t #f #f #f))

(define chords (map (lambda (ps cs t) (Chord ps 'E cs t)) chords-pitches chords-controlss chords-ties))

(define chords-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                   (list (KeySignature 'Fs 'Minor)
                                         (first chords)
                                         (second chords)
                                         (Tuplet 3 2 'Q (list (third chords) (fourth chords) (fifth chords))))))


#;(define arpeggios-score (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                       (tempo/param (TempoDur 'E 40)))
                          (gen-score-file (score/parameterized (list arpeggios-voice chords-voice)))))
  

;; generalize:  descending/ascending single-pitch and multi-pitch patterns
;;
;; ascending/descending single-pitch patterns:
;; - currently ascend/descend split only, but second half of measure is easy derivation of first
;;   in 2 : 1 : 1 proportions rhythmically
;; - for that, divide into three-note segments, with second half twice repeating first three-note
;;   components of first and third from the start but with octave lower starting pitch for third

(define fs-minor-range (scale->pitch-range-pair Fs-minor))

(define treble-start-1 (cons 'Fs '8vb))

(define treble-start-2 (cons 'A  '0va))

(define treble-start-3 (cons 'A  '15va))

(define treble-arp-1 '(0 4 2))

(define treble-arp-2 '(0 2 2))

(define treble-arp-3 '(0 -3 -2))

(define treble-seg-1 (transpose/successive Fs-minor fs-minor-range treble-start-1 treble-arp-1))

(define treble-seg-2 (transpose/successive Fs-minor fs-minor-range treble-start-2 treble-arp-2))

(define treble-seg-3 (transpose/successive Fs-minor fs-minor-range treble-start-3 treble-arp-3))

(define treble-seg-4 (transpose/successive Fs-minor fs-minor-range treble-start-2 treble-arp-3))

;; or you could take low Fs and high A and have middle just expand and reverse an A major chord
;;   Fs-Cs-E-(A-Cs-E)-A-(E-Cs-A)-E-Cs
;;   Fs-(Cs-E-A-E-Cs) Fs-(Cs-E-A-E-Cs)
;; emphasizing repeated rhythm of (2:1:1)

;; the goal would be build a routine that contracted a pattern, in this case dividing the
;; arpeggio in half, truncating it so it turned around and repeated in half the time
;; the data would be the start and midpoint pitches and the arpeggio would be a retograde
;; unfolding of the pitches completing the time duration, e.g:
;; - (Fs 8vb) (4 2 3 2 2) (A 8va) for the first arpeggio where a routine generates the
;;   pitches up to the second pitch, then reverses them for the count of transpositions
;;   in the first, ascending list of intervals, e.g. accumulating the ascending pitches
;;   after the first in the generation of the ascending portion, then reversing them
;;   in the second half
;; - (Fs 8vb) (4 2) (A 0va) for the second half

(define fs-minor-ascending-arpeggio (transpose/successive Fs-minor fs-minor-range (cons 'Fs '8vb) '(0 4 2 3 2 2 3)))

(define retro-arpeggio-pitches-1 (squash-to-retro (sub1 (length fs-minor-ascending-arpeggio)) fs-minor-ascending-arpeggio))

(define retro-arpeggio-pitches-2 (squash-to-retro 3 retro-arpeggio-pitches-1))

(define retro-arpeggio-notes-1 (gen-notes retro-arpeggio-pitches-1 arpeggio-controlss-1))

(define retro-arpeggio-notes-2 (gen-notes (append retro-arpeggio-pitches-2 retro-arpeggio-pitches-2) arpeggio-controlss-2))

(define retro-arpeggios-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                            (list (KeySignature 'Fs 'Minor)
                                                  (Tuplet 12 8 'Q retro-arpeggio-notes-1)
                                                  (Tuplet 12 8 'Q retro-arpeggio-notes-2))))

;; what about the chords, does it abstract to anything simpler?

;; intervals of chords are 5 4 5 1, successive intervals of top voice are -3 -4 -2 (or -2 -3 -1),
;; of bottom voice -2 -5 3 (or -1 -4 2)

;; effect of chords is a bit of overlap in the swells up and down - bottom voice goes consistently up then down,
;; top voice goes down, then on the last two notes the bottom half starts back up again

;; also rhythm is charged in last two notes making them a pick up to return to high register starting all over,
;; resulting in down then back up with ascending bit overlapping from the end of of the bar to descent at the
;; start of the next bar

;; top is a complement/contrast to the bottom, with phase shift (down/up) and period shift (slower), as of two
;; different swells across a common surface

;; bit of energy interplay with top voice dissipating until short pick up at the end of the bar, bottom voice
;; surging at start then dissipating in second with repeat of shorter arpeggios in rhythm of strong quarter then
;; two weaker eighths

;; or maybe in both voices mainly a strong shove off, with just the hint of a pick up in bottom half of top voice
;; at very end of bar, especially with major second dissonance, or maybe it's just all those missed beats in the
;; top voice with the tied triplet at the end?

;; however it works, a strong rocking feeling like being in a boat with small swells, and there's a sense of
;; energy dissipating from the start to the end of the bar

;; note the three repetitions of the first bar before starting the next part of the texture with the slowest yet
;; swell echoing in the inner voice first up, then up and down

(define arpeggios-score (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                         (tempo/param (TempoDur 'E 40)))
                            (gen-score-file (score/parameterized (list retro-arpeggios-voice chords-voice)))))

;; looking forward: whole-tone descent from fs minor ninth to e minor ninth requires g natural, but there's
;; no way for me with my scale framework to depart from the key signature

;; the only way to do it would be to change the key signature from fs minor to e minor, dropping the g and c sharps

;; and then ditto for the drop from e minor to d minor with f and c natural but also b natural at the end of bars
;; 11 and 12 and all of bar 13 for a modal feel - note the f sharp / f natural within bars 11 and 12

;; maybe I could just shift keys in my generative software, using the scale based intervals, but leaving the key
;; signature alone and letting lilypond figure out the accidentals

;; that has the appeal of raising another control as a component of the matrix, e.g. shifting key, which seems appropriate
;; here given all the parallel chords, e.g. preserving intervals but shifting chords, so just shift key, leave key
;; signature as is, show pitches with accidentals as I already do and let Lilyond figure out where to put in the naturals

;; though it's nice to figure out how to reduce the arpeggios voice to just that one span with with two applications
;; of the same retro-squash routine, it's harder to see how to reduce the two-part chords the same way

;; and besides, the choice of the A major chord in every pitch but the first weakens the low Fs as tonal center, which
;; it asserts by range and repetition only - the pitches are an Fs minor 9th, but the skip up to A at the top of the
;; sweep instead of Fs and the grind of the two Gs against the A in the chord voice sound more like an A major 7, less
;; like an extension of the G minor 9th to an 11th -- so there's a tension between the relatively weaker flat seventh
;; of the Fs/E vs. the sharper half-step Gs/A

;; the rhythm of the phrasing is where the chord and arpeggio voices relate to -- and what elicits the reveal of the inner
;; third voice of chimes, and the slight leaning down to the final Fs in bar 10 to pull the tonal balance toward Fs minor
;; vs. A major 

;; from the start, a strong quarter-eighth-eighth of the arpeggios grinds agains the offbeat accents in the chords with
;; the suspension across the middle beat tied to the triplets in the second beat -- nicely mixing the energy of the
;; 2+ octave span of the first beat arpeggio with the suspension of synchrony with the tie over the middle beat and the
;; triplets in the chord voice -- the sweep and crash of water as a wave arrives followed by by the softer hiss of the
;; reflection rushing back to collide with the next wave

;; the exact repetition in the second measure and third measures establish a two-bar rhythm expecting a fourth repeat
;; in bar 4 only for a further bit of imbalance when the inner voice appears, first as a two note ascending fourth
;; emphasizing that Gs again this time an octave lower -- at first just as a threat to the four-bar pattern as, for
;; the first to tries, it doesn't lead to anything in the next measure, which is postponed until the last two bars
;; where it spills over to the repetition of the ostinato in bars 10 - 11 -- a total of 5 two-bar phrases where the
;; rocking motion is gradually amplified through repetition and elaboration into four continuous bars

;; this stretching seems like the point as the swells in one bar establish a two bar pattern, then try for a longer
;; grouping two more times before growing to the four bars in 7-8-9-10, not through any harmonic motion, but by
;; offsetting the regularity within the kernel, letting that build first by repetition, then extension to the last
;; four bars before the first break

;; note the break lasts a total of 8 eight notes, but with as 3 + 3 + 2

;; the repetition and additive layering makes me think of the minimalists, though with softer corners -- which was
;; part of my motivation trying my own hand at algorithms for gradual change

;;

(define quarter-tone-pitches-up (map (lambda (pc) (cons pc '0va)) (Scale-pitch-classes quarter-tones-up)))
  
(define quarter-tone-pitches-down (append
                                   (map (lambda (pc) (cons pc '8va)) (take (Scale-pitch-classes quarter-tones-down) 2))
                                   (map (lambda (pc) (cons pc '0va)) (drop (Scale-pitch-classes quarter-tones-down) 2))))

(define quarter-tone-notes-up (map (lambda (p) (Note (car p) (cdr p) 'T '() #f)) quarter-tone-pitches-up))

(define quarter-tone-notes-down (map (lambda (p) (Note (car p) (cdr p) 'T '() #f)) quarter-tone-pitches-down))

(define quarter-tone-voice (PitchedVoice 'AcousticGrand 'PanCenter
                                         (flatten (list (KeySignature 'C 'Major)
                                                        quarter-tone-notes-up
                                                        quarter-tone-notes-down))))

(define quarter-tone-notes--score (parameterize ((time-signature/param (TimeSignatureSimple 2 'Q))
                                                 (tempo/param (TempoDur 'E 40)))
                                    (gen-score-file (score/parameterized (list quarter-tone-voice)))))

  
