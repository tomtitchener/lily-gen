#lang racket

;; convert symbols for lilypond atoms to lilypond strings

(provide
 ;; all these are the same signature, (-> symbol? string?)
 ;; all lookup symbol in list from score.rkt (except ann->lily)
 pitch-class->lily
 octave->lily
 duration->lily
 accent->lily
 dynamic->lily
 swell->lily
 sustain->lily
 sostenuto->lily
 slur->lily
 ann->lily          ;; formats annotation with markup bracket
 instr->lily        ;; full midi instrument name
 instr->short-lily  ;; shortened instrument name for left margin
 clef->lily
 mode->lily
 ;; contracts 
 (contract-out
  [duration->int  (-> symbol? natural-number/c)]
  [int->durations (-> natural-number/c (listof natural-number/c))]))

;; - - - - - - - - -
;; implementation
(require racket/contract)

;; indirectly imports all from score-syms.rkt
(require "score.rkt")

(define pitch-lily-strings
  '("bis"   "c"   "deses"
    "bisis" "cis" "des"
    "cisis" "d"   "eeses"
    "dis"   "ees" "feses"
    "disis" "e"   "fes"
    "eis"   "f"   "geses"
    "eisis" "fis" "ges"
    "fisis" "g"   "aeses"
    "gis"   "aes"
    "gisis" "a"   "beses"
    "ais"   "bes" "ceses"
    "aisis" "b"   "ces"))

(define sym-pitch-hash (make-hash (map cons pitch-class-syms pitch-lily-strings)))

(define (pitch-class->lily pitch)
  (hash-ref sym-pitch-hash pitch))

(define octave-lily-strings
  '(",,,"
    ",,"
    ","
    ""
    "'"
    "''"
    "'''"
    "''''"))

(define sym-octave-hash (make-hash (map cons octave-syms octave-lily-strings)))

(define (octave->lily octave)
  (hash-ref sym-octave-hash octave))

(define duration-lily-strings
  '("1."  "1"
    "2."  "2"
    "4."  "4"
    "8."  "8"
    "16." "16"
    "32." "32"
    "64." "64"
    "128"))

;; in 128ths 
(define duration-vals
 '(192 128
    96  64
    48  32
    24  16
    12   8
     6   4
     3   2
     1))
  
(define sym-duration-hash (make-hash (map cons duration-syms duration-lily-strings)))

(define (duration->lily duration)
  (hash-ref sym-duration-hash duration))

(define duration-int-hash (make-hash (map cons duration-syms duration-vals)))

(define (duration->int duration)
  (hash-ref duration-int-hash duration))

(define int-duration-hash (make-hash (map cons duration-vals duration-syms)))

;; totval is any non-zero positive integer,
;; answer the list of durations large -> small to exhaust totval
(define/contract (int->durations totval)
  (-> natural-number/c (listof duration?))
  (define (int->duration duration)
    (hash-ref int-duration-hash duration))
  (let inner ([tot totval]
              [ret '()])
    (if (zero? tot)
        (reverse ret)
        (let* ([nextval (findf ((curry >=) tot) duration-vals)]
               [nextdur (int->duration nextval)])
          (inner (- tot nextval) (cons nextdur ret))))))

(define accent-lily-strings '("-^" "--" "-!" "-."  "->" "-_"))

(define sym-accent-hash (make-hash (map cons accent-syms accent-lily-strings)))

(define (accent->lily accent)
  (hash-ref sym-accent-hash accent))

(define dynamic-lily-strings '("\\ppppp" "\\pppp" "\\ppp" "\\pp" "\\p" "\\mp" "\\mf" "\\f" "\\ff" "\\fff" "\\ffff" "\\fffff" "\\fp" "\\sf" "\\sff" "\\sp" "\\spp" "\\sfz" "\\rfz"))

(define sym-dynamic-hash (make-hash (map cons dynamic-syms dynamic-lily-strings)))

(define (dynamic->lily dynamic)
  (hash-ref sym-dynamic-hash dynamic))

(define swell-lily-strings '("\\<", "\\>", "\\espressivo", "\\!"))

(define sym-swell-hash (make-hash (map cons swell-syms swell-lily-strings)))

(define (swell->lily swell)
  (hash-ref sym-swell-hash swell))

(define sustain-lily-strings '("\\sustainOn" "\\sustainOff"))

(define sym-sustain-hash (make-hash (map cons sustain-syms sustain-lily-strings)))

(define (sustain->lily sustain)
  (hash-ref sym-sustain-hash sustain))

(define sostenuto-lily-strings '("\\sostenutoOn" "\\sostenutoOff"))

(define sym-sostenuto-hash (make-hash (map cons sostenuto-syms sostenuto-lily-strings)))

(define (sostenuto->lily sostenuto)
  (hash-ref sym-sostenuto-hash sostenuto))

(define slur-lily-strings '("(" ")"))

(define sym-slur-hash (make-hash (map cons slur-syms slur-lily-strings)))

(define (slur->lily slur)
  (hash-ref sym-slur-hash slur))

(define (ann->lily ann)
  (string-append "^\\markup { \\italic \"" ann "\" }"))

;; copied from midi instrument list for lilypond conversion to midi
(define instrs
  '("acoustic grand"            "contrabass"         "lead 7 (fifths)"
    "bright acoustic"           "tremolo strings"    "lead 8 (bass+lead)"
    "electric grand"           "pizzicato strings"  "pad 1 (new age)"
    "honky-tonk"               "orchestral harp"    "pad 2 (warm)"
    "electric piano 1"         "timpani"            "pad 3 (polysynth)"
    "electric piano 2"         "string ensemble 1"  "pad 4 (choir)"
    "harpsichord"              "string ensemble 2"  "pad 5 (bowed)"
    "clav"                     "synthstrings 1"     "pad 6 (metallic)"
    "celesta"                  "synthstrings 2"     "pad 7 (halo)"
    "glockenspiel"             "choir aahs"         "pad 8 (sweep)"
    "music box"                "voice oohs"         "fx 1 (rain)"
    "vibraphone"               "synth voice"        "fx 2 (soundtrack)"
    "marimba"                  "orchestra hit"      "fx 3 (crystal)"
    "xylophone"                "trumpet"            "fx 4 (atmosphere)"
    "tubular bells"            "trombone"           "fx 5 (brightness)"
    "dulcimer"                 "tuba"               "fx 6 (goblins)"
    "drawbar organ"            "muted trumpet"      "fx 7 (echoes)"
    "percussive organ"         "french horn"        "fx 8 (sci-fi)"
    "rock organ"               "brass section"      "sitar"
    "church organ"             "synthbrass 1"       "banjo"
    "reed organ"               "synthbrass 2"       "shamisen"
    "accordion"                "soprano sax"        "koto"
    "harmonica"                "alto sax"           "kalimba"
    "concertina"               "tenor sax"          "bagpipe"
    "acoustic guitar (nylon)"  "baritone sax"       "fiddle"
    "acoustic guitar (steel)"  "oboe"               "shanai"
    "electric guitar (jazz)"   "english horn"       "tinkle bell"
    "electric guitar (clean)"  "bassoon"            "agogo"
    "electric guitar (muted)"  "clarinet"           "steel drums"
    "overdriven guitar"        "piccolo"            "woodblock"
    "distorted guitar"         "flute"              "taiko drum"
    "guitar harmonics"         "recorder"           "melodic tom"
    "acoustic bass"            "pan flute"          "synth drum"
    "electric bass (finger)"   "blown bottle"       "reverse cymbal"
    "electric bass (pick)"     "shakuhachi"         "guitar fret noise"
    "fretless bass"            "whistle"            "breath noise"
    "slap bass 1"              "ocarina"            "seashore"
    "slap bass 2"              "lead 1 (square)"    "bird tweet"
    "synth bass 1"             "lead 2 (sawtooth)"  "telephone ring"
    "synth bass 2"             "lead 3 (calliope)"  "helicopter"
    "violin"                   "lead 4 (chiff)"     "applause"
    "viola"                    "lead 5 (charang)"   "gunshot"
    "cello"                    "lead 6 (voice)"))

(define instrs-short
  '( "piano"                    "contra"             "ffths"
     "piano"                    "strs"               "basld"
     "piano"                    "pizzs"              "pad1"
     "piano"                    "harp"               "pad2"
     "piano"                    "timp"               "pad3"
     "piano"                    "strs"               "pad4"
     "hpscd"                    "strs"               "pad5"
     "clav"                     "synstr"             "pad6"
     "clsta"                    "synstr"             "pad7"
     "glock"                    "aahs"               "pad8"
     "mbox"                     "oohs"               "fx1"
     "vibes"                    "synv"               "fx2"
     "marimba"                  "orcht"              "fx3"
     "xyl"                      "tpt"                "fx4"
     "tube"                     "tbn"                "fx5"
     "dulc"                     "tuba"               "fx6"
     "organ"                    "mutpt"              "fx7"
     "organ"                    "horn"               "fx8"
     "organ"                    "brass"              "sitr"
     "organ"                    "synbr"              "banj"
     "organ"                    "synbr"              "sham"
     "accrd"                    "sopsx"              "koto"
     "harmo"                    "altsx"              "klmb"
     "ctina"                    "tensx"              "bagp"
     "guitr"                    "barsx"              "fddl"
     "guitr"                    "oboe"               "shni"
     "guitr"                    "enhrn"              "tnkl"
     "guitr"                    "bsn"                "aggo"
     "guitr"                    "clnt"               "stldr"
     "guitr"                    "picc"               "wdblk"
     "guitr"                    "fl"                 "tiko"
     "guitr"                    "rec"                "mtom"
     "bass"                     "pan"                "syndr"
     "bass"                     "bot"                "rvcbl"
     "bass"                     "shaki"              "fret"
     "bass"                     "whstl"              "brth"
     "bass"                     "ocrna"              "sea"
     "bass"                     "sqre"               "btwt"
     "bass"                     "sawth"              "ring"
     "bass"                     "call"               "cptr"
     "vln"                      "chiff"              "appl"
     "va"                       "chang"              "gun"
     "cello"                    "voice"))

(define sym-instr-hash (make-hash (map cons instr-syms instrs)))

(define sym-instr-short-hash (make-hash (map cons instr-syms instrs-short)))

(define (instr->lily instr)
  (hash-ref sym-instr-hash instr))

(define (instr->short-lily instr)
  (hash-ref sym-instr-short-hash instr))

(define clef-lily-strings '("bass_8" "bass" "tenor" "alto" "treble" "treble^8"))

(define sym-clef-hash (make-hash (map cons clef-syms clef-lily-strings)))

(define (clef->lily clef)
  (format "\\clef ~a" (hash-ref sym-clef-hash clef)))

(define mode-lily-strings '("major" "minor"))

(define sym-mode-hash (make-hash (map cons mode-syms mode-lily-strings)))

(define (mode->lily mode)
  (hash-ref sym-mode-hash mode))

