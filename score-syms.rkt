#lang racket

;; symbol lists and predicates, get used in text to create atoms in Score

(provide
 ;; all are either (listof symbol?) or (-> symbol? boolean?)
 pitch-class-syms
 pitch-class?
 octave-syms
 octave?
 duration-syms
 duration?
 accent-syms
 accent?
 dynamic-syms
 dynamic?
 swell-syms
 swell?
 sustain-syms
 sustain?
 sostenuto-syms
 sostenuto?
 slur-syms
 slur?
 instr-syms
 instr?
 clef-syms
 clef?
 mode-syms
 mode?)

;; - - - - - - - - -
;; implementation
(require srfi/1)

(define pitch-class-syms
 '(Bs  C  Dff
   Bss Cs  Df
   Css D  Eff
   Ds  Ef Fff
   Dss E  Ff
   Es  F  Gff
   Ess Fs Gf
   Fss G  Aff
   Gs  Af
   Gss A  Bff
   As  Bf Cff
   Ass B  Cf))

(define (pitch-class? p) (and (symbol? p) (member p pitch-class-syms)))

(define octave-syms
 '(29vb
   22vb
   15vb
   8vb
   0va
   8va
   15va
   22va))

(define (octave? o) (and (symbol? o) (member o octave-syms)))

(define duration-syms
 '(W.  W
   H.  H
   Q.  Q
   E.  E
   S.  S
   T.  T
   SF. SF
   HTE))

(define (duration? d) (and (symbol? d) (member d duration-syms)))

(define accent-syms
  '(Marcato
    Tenuto
    Staccatissimo
    Staccato
    Accent
    Portato))

(define (accent? a) (and (symbol? a) (member a accent-syms)))

(define dynamic-syms
  '(PPPPP
    PPPP
    PPP
    PP
    Piano
    MP
    MF
    Forte
    FF
    FFF
    FFFF
    FFFFF
    FP
    SF
    SFF
    SP
    SPP
    SFZ
    RFZ))

(define (dynamic? d) (and (symbol? d) (member d dynamic-syms)))

(define swell-syms
  '(Crescendo
    Decrescendo
    Espressivo
    SwellStop))

(define (swell? s) (and (symbol? s) (member s swell-syms)))

(define sustain-syms
  '(SustainOn
    SustainOff))

(define (sustain? s) (and (symbol? s) (member s sustain-syms)))

(define sostenuto-syms
  '(SostenutoOn
    SostenutoOff))

(define (sostenuto? s) (and (symbol? s) (member s sostenuto-syms)))

(define slur-syms
  '(SlurOn
    SlurOff))

(define (slur? s) (and (symbol? s) (member s slur-syms)))

(define instr-syms
  '(AcousticGrand            Contrabass          LeadFfths 
    BrightAcoustic           TremoloStrings      LeadBassPlusLead
    ElectricGrand            PizzicatoStrings    PadNewAge
    HonkyTonk                OrchestralHarp      PadWarm
    ElectricPiano1           Timpani             PadPolysynth
    ElectricPiano2           StringEnsemble1     PadChoir
    Harpsichord              StringEnsemble2     PadBowed
    Clav                     Synthstrings1       PadMetallic
    Celesta                  Synthstrings2       Halo
    Glockenspiel             ChoirAahs           Sweep
    MusicBox                 VoiceOohs           Rain
    Vibraphone               SynthVoice          Soundtrack
    Marimba                  OrchestraHit        Crystal
    Xylophone                Trumpet             Atmosphere
    TubularTells             Trombone            Bbrightness
    Dulcimer                 Tuba                Goblins
    DrawbarOrgan             MutedTrumpet        Echoes
    PercussiveOrgan          FrenchHorn          SciFi
    RockOrgan                BrassSection        Sitar
    ChurchOrgan              Synthbrass1         Banjo
    ReedOrgan                Synthbrass2         Shamisen
    Accordion                SopranoSax          Koto
    Harmonica                AltoSax             Kalimba
    Concertina               TenorSax            Bagpipe
    AcousticGuitarNylon      BaritoneSax         Fiddle
    AcousticGuitarSteel      Oboe                Shanai
    ElectricGuitarJazz       EnglishHorn         TinkleBell
    ElectricGuitarClean      Bassoon             Agogo
    ElectricGuitarMuted      Clarinet            SteelDrums
    OverdrivenGuitar         Piccolo             Woodblock
    DistortedGuitar          Flute               TaikoDrum
    GuitarHarmonics          Recorder            MelodicTom
    AcousticBass             PanFlute            SynthDrum
    ElectricBassFinger       BlownBottle         ReverseCymbal
    ElectricBassPick         Shakuhachi          GuitarFretNoise
    FretlessBass             Whistle             BreathNoise
    SlapBass1                Ocarina             Seashore
    SlapBass2                LeadSquare          BirdTweet
    SynthBass1               LeadSawtooth        TelephoneRing
    SynthBass2               LeadCalliope        Helicopter
    Violin                   LeadChiff           Applause
    Viola                    LeadCharang         Gunshot
    Cello                    LeadVoice))

(define (instr? i) (and (symbol? i) (member i instr-syms)))

(define clef-syms
  '(Bass8VB
    Bass
    Tenor
    Alto
    Treble
    Treble8VA))

(define (clef? c) (and (symbol? c) (member c clef-syms)))

(define mode-syms
  '(Major
    Minor))

(define (mode? m) (and (symbol? m) (member m mode-syms)))
