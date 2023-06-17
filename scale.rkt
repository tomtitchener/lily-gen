#lang racket

;; 1) Scale struct, and scales:
;; - chromatic
;; - major and harmonic minor
;; - diatonic
;; 2) scale-based utilities e.g. transposition by steps in a scale
;; - ascending or descending lists of notes
;; - concatenation two ascending/descending or descending/ascending lists of Noes
;; - inclusive range by pitches for a Scale to fit the range of octaves

(provide
 ;; Scale
 (struct-out Scale)

 ;; - - - - - - - - -
 ;; Scales 
 ;; all scales are instances of Scale
 ;; C-anchored chromatic scale with one sharp
 chromatic-sharps
 
 ;; C-anchored chromatic scale with one flat
 chromatic-flats

 ;; C-anchored diatonic scales
 C-whole-tone
 Df-whole-tone
 
 ;; diatonic major scales from Cff-major to Fss-major
 (matching-identifiers-out #rx".*-major" (all-defined-out))

 ;; harmonic minor scales from Aff to Dss
 (matching-identifiers-out #rx".*-minor" (all-defined-out))
 
 ;; - - - - - - - - -
 ;; Utilities
 ;; transpose up (positive) or down (negative) from (pitch-class . octave) pair by scale steps
 ;; guard range by min max relative range [0..(scale->max-idx scale)] but possibly narrower
 ;; note: regular arithmetic so 0 is the same as unison, 1 is the same as a second, 2 is a third, etc.
 (contract-out
  [xpose (-> Scale? pitch-range/c pitch/c integer? maybe-pitch/c)])
 
 ;; transpose from the starting pitch using the list of integers to transpose
 ;; the result of the previous step, e.g. (1 1 1 ...) for a step-by-step progression
 (contract-out
  [transpose/successive (-> Scale?
                            pitch-range/c
                            pitch/c
                            (or/c integer? (listof integer?))
                            (or/c maybe-pitch/c (listof maybe-pitch/c)))])

 ;; transpose from the starting pitch using a single integer? or a list of integer?
 ;; guarding resulting pitch/c against min and max pitches
 ;; for a single integer?, just provide single pitch/c that is transpose/absolute called once
 ;; else for a list of integer?, answer multiple pitch/c to transpose repeatedly from the
 ;; starting pitch, e.g. (1 2 3 ...) for a step-by-step progression
 (contract-out
  [transpose/absolute (-> Scale?
                          pitch-range/c
                          pitch/c
                          (or/c integer? (listof integer?))
                          (or/c maybe-pitch/c maybe-pitch/c))])
 
 ;; convert a range of integer? (default step of 1) into a list of note of equal duration
 ;; starting at 
 (contract-out
  [note-range (->* (Scale? duration? pitch-range/c pitch/c integer? integer?)
                   (integer?)
                   (listof Note?))])
 
 ;; concatenate forward and reverse-direction note-range by swapping start, stop, and step for ranges
 (contract-out 
  [inverted-note-ranges (->*
                         (Scale? duration? pitch-range/c pitch/c integer? integer?)
                         (integer?)
                         (listof Note?))])

 ;; minimum and maximum pitches for a scale
 (contract-out
  [scale->pitch-range (-> Scale? pitch-range/c)]))

;; - - - - - - - - -
;; implementation
(require (only-in racket/provide matching-identifiers-out))

(require (only-in seq find))

(require (only-in algorithms scanl))

(require (only-in srfi/1 list-index))

(require (only-in "score.rkt" octave-syms octave? pitch-class? duration? pitch/c maybe-pitch/c Note Note?))

(require (only-in "utils.rkt" rotate-list-by))

(define/contract (octave-list-idx oct)
  (-> octave? natural-number/c)
  (list-index ((curry eq?) oct) octave-syms))

(define/contract (octave-list-ref idx)
  (-> natural-number/c octave?)
  (when (or (< idx 0) (>= idx (length octave-syms)))
    (error 'octave-list-ref "idx: ~v out of range for octave-syms ~v" idx octave-syms))
  (list-ref octave-syms idx))

(define/contract (pitch-class-list-idx pitch-class-syms pitch-class)
  (-> (non-empty-listof pitch-class?) pitch-class? natural-number/c)
  (let ([ret (list-index ((curry eq?) pitch-class) pitch-class-syms)])
    (when (not ret)
      (error 'pitch-class-list-idx "pitch-class ~v is not in list ~v" pitch-class pitch-class-syms))
    ret))

(define/contract (pitch-class-list-ref pitch-class-syms idx)
  (-> (non-empty-listof pitch-class?) natural-number/c pitch-class?)
  (when (>= idx (length pitch-class-syms))
    (error 'pitch-class-list-ref "idx: ~v out of range for pitch-class-syms ~v" idx pitch-class-syms))
  (list-ref pitch-class-syms idx))

(define no-duplicates/c
  (make-flat-contract #:name 'no-duplicates/c #:first-order (compose not check-duplicates)))

(struct/contract Scale ([pitch-classes (and/c (non-empty-listof pitch-class?) no-duplicates/c)]) #:transparent)

(define chromatic-sharps (Scale '(C Cs D Ds E F Fs G Gs A As B)))

(define chromatic-flats (Scale '(C Df D Ef E F Gf G Af A Bf Cf)))

(define C-whole-tone (Scale '(C D E Fs Gs As)))

(define Df-whole-tone (Scale '(Df Ef F G A B)))

;; used to order diatonic scale pitch classes ascending pitch-class
;; closest to C to match groupings on keyboard which in turn matches
;; octaves in lilypond
(define c-ordered-enharmonic-pitch-class-symss
 '((Bs  C  Dff)
   (Bss Cs  Df)
   (Css D  Eff)
   (Ds  Ef Fff)
   (Dss E  Ff)
   (Es  F  Gff)
   (Ess Fs Gf)
   (Fss G  Aff) 
   (Gs  Af)
   (Gss A  Bff) 
   (As  Bf Cff) 
   (Ass B  Cf)))

(define fifths-ordered-pitch-class-syms
  '(Fff Cff Gff Dff Aff Eff Bff 
    Ff  Cf  Gf  Df  Af  Ef  Bf 
    F   C   G   D   A   E   B 
    Fs  Cs  Gs  Ds  As  Es  Bs
    Fss Css Gss Dss Ass Ess Bss))

(define (fifths-ordered-pitch-class-index sym)
  (list-index ((curry eq?) sym) fifths-ordered-pitch-class-syms))

(define (fifths-ordered-pitch-class-ref sym)
  (list-ref fifths-ordered-pitch-class-syms sym))

(define/contract (compare-pitch-class-sym-indices cmp sym1 sym2)
  (-> (-> natural-number/c natural-number/c boolean?) pitch-class? pitch-class? boolean?)
  (cmp (fifths-ordered-pitch-class-index sym1) (fifths-ordered-pitch-class-index sym2)))

;; when pitch-classes are ordered by fifths, collect the list of seven
;; pitch-classes starting from one below the tonic gives you the major
;; scale pitches e.g. (F C G D A E B)
;; then indexing into list starting at index 1 incrementing by 2 modulo
;; the length of the list gives the pitches in order (C D E F G A B)
(define/contract (major-scale root-pc [err-sym 'major-scale])
  (->* (pitch-class?) (symbol?) Scale?)
  (when (compare-pitch-class-sym-indices < root-pc 'Cff)
    (error err-sym "root pitch ~v is below minimum ~a" root-pc 'Cff))
  (when (compare-pitch-class-sym-indices > root-pc 'Fss)
    (error err-sym "root pitch ~v is above maximum ~a" root-pc 'Fss))
  (let* ([first (pitch-class-list-idx fifths-ordered-pitch-class-syms root-pc)]
         [cnt-fifths (length fifths-ordered-pitch-class-syms)]
         [start (modulo (sub1 first) cnt-fifths)]
         [stop  (modulo (+ start 7) cnt-fifths)]
         [ixs (map (lambda (n) (modulo n cnt-fifths)) (range start stop))]
         [fifths-pcs (map ((curry list-ref) fifths-ordered-pitch-class-syms) ixs)]
         [pcs (map ((curry list-ref) fifths-pcs) (map (lambda (i) (modulo i 7))(range 1 14 2)))])
    (Scale pcs)))

;; the harmonic minor scale for a root pitch class (A) is the same as the
;; relative-minor for the scale for the pitch-class three fifths below it
;; (A -> D -> G -> C)
(define/contract (minor-scale root-pc)
  (-> pitch-class? Scale?)
  (when (compare-pitch-class-sym-indices < root-pc 'Aff)
    (error 'minor-scale "root pitch ~v is below minimum ~v" root-pc 'Aff))
  (when (compare-pitch-class-sym-indices > root-pc 'Dss)
    (error 'minor-scale "root pitch ~v is above maximum ~v" root-pc 'Dss))
  (let* ([temp-idx (- (fifths-ordered-pitch-class-index root-pc) 3)]
         [temp-pc (fifths-ordered-pitch-class-ref temp-idx)])
    (relative-minor-scale temp-pc)))

;; the relative minor scale is the same pitches as the major scale rotated forward by 5
(define/contract (relative-minor-scale root-pc)
  (-> pitch-class? Scale?)
  (Scale (rotate-list-by (Scale-pitch-classes (major-scale root-pc 'relative-minor-scale)) 5)))

;; macros to create diatonic major and harmonic minor scales named e.g. C-major C-minor ...
;; tbd: first macro attempt, could probably be simpler
(require (for-syntax racket/syntax))

(define-syntax (make-major-scale stx)
  (syntax-case stx ()
    [(_ a)
     ;; tbd: gave up trying to make lower-case pitch-class
     ;; e.g. c-major instead of C-major
     (with-syntax ([name (format-id #'a "~a-major" #'a)])
       #'(define name (major-scale 'a)))]
    [(_ a b ...)
     #'(begin
         (make-major-scale a)
         (make-major-scale b ...))]))

(define-syntax (make-minor-scale stx)
  (syntax-case stx ()
    [(_ a)
     ;; tbd: gave up trying to make lower-case pitch-class
     ;; e.g. c-minor instead of C-minor
     (with-syntax ([name (format-id #'a "~a-minor" #'a)])
       #'(define name (minor-scale 'a)))]
    [(_ a b ...)
     #'(begin
         (make-minor-scale a)
         (make-minor-scale b ...))]))

(make-major-scale     Cff Gff Dff Aff Eff Bff 
                  Ff  Cf  Gf  Df  Af  Ef  Bf 
                  F   C   G   D   A   E   B 
                  Fs  Cs  Gs  Ds  As  Es  Bs
                  Fss)

(make-minor-scale                 Aff Eff Bff 
                  Ff  Cf  Gf  Df  Af  Ef  Bf 
                  F   C   G   D   A   E   B 
                  Fs  Cs  Gs  Ds  As  Es  Bs
                  Fss Css Gss Dss)

;; what is the index in c-ordered-enharmonic-pitch-class-symss for pitch-class?
(define/contract (pitch-class->chromatic-enharmonic-index pitch-class)
  (-> pitch-class? natural-number/c)
  (index-where c-ordered-enharmonic-pitch-class-symss (lambda (enh-syms) (member pitch-class enh-syms))))

;; order pitch classes in a scale for transposition relative to C octaves for lilypond notation
(define/contract (scale->c-ordering scale)
  (-> Scale? Scale?)
  (Scale (sort (Scale-pitch-classes scale) < #:key pitch-class->chromatic-enharmonic-index)))

(define/contract (idx->pitch scale idx)
  (-> Scale? natural-number/c pitch/c)
  (let* ([pcs     (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt (length pcs)])
    (let-values ([(q r) (quotient/remainder idx pcs-cnt)])
      (cons (pitch-class-list-ref pcs r) (octave-list-ref q)))))

;; pitch index is the count of pitches from the lowest possible pitch
(define/contract (pitch->idx scale pitch)
  (-> Scale? pitch/c natural-number/c)
  (let* ([pcs     (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt (length pcs)]
         [pcs-idx (pitch-class-list-idx pcs (car pitch))]
         (oct-idx (octave-list-idx (cdr pitch))))
    (+ (* oct-idx pcs-cnt) pcs-idx)))

(define pitch-range/c
  (make-flat-contract #:name 'pitch-range/c #:first-order (cons/c pitch/c pitch/c)))

;; to compare two pitches from the same scale compare their pitch indices 
(define/contract (compare-pitches op scale pitch-1 pitch-2)
  (-> (-> any/c any/c boolean?) Scale? pitch/c pitch/c boolean?)
  (op (pitch->idx scale pitch-1) (pitch->idx scale pitch-2)))

;; pitch-range is inclusive
(define/contract (pitch-in-range? scale pitch-range pitch)
  (-> Scale? pitch-range/c pitch/c boolean?)
  (let ([min-pitch (car pitch-range)]
        [max-pitch (cdr pitch-range)])
    (and (compare-pitches >= scale pitch min-pitch)
         (compare-pitches <= scale pitch max-pitch))))

;; called by xpose to answer either pitch or #f given a range
(define/contract (filter-pitch-in-range scale pitch-range pitch)
  (-> Scale? pitch-range/c pitch/c maybe-pitch/c)
  (if (pitch-in-range? scale pitch-range pitch) pitch #f))

(define/contract (pitch-is-scale-member? scale pitch)
  (-> Scale? pitch/c boolean?)
  (if (memq (car pitch) (Scale-pitch-classes scale)) #t #f))

(define/contract (pitch-range-pitches-are-scale-members? scale pitch-range)
  (-> Scale? pitch-range/c boolean?)
  (and (pitch-is-scale-member? scale (car pitch-range))
       (pitch-is-scale-member? scale (cdr pitch-range))))

;; guard xpose, transpose/successive, transpose-absolute inputs
;; scale, starting pitch and range to make sure all pitches are
;; members of scale
(define/contract (pitch-range&pitch-in-scale? scale pitch-range pitch)
  (-> Scale? pitch-range/c pitch/c  boolean?)
  (and (pitch-range-pitches-are-scale-members? scale pitch-range)
       (pitch-is-scale-member? scale pitch)))

;; (-> Scale? pitch-range/c pitch/c integer? maybe-pitch/c)
(define (xpose scale pitch-range pitch interval)
  (when (not (pitch-range&pitch-in-scale? scale pitch-range pitch))
    (error 'xpose "one of pitch-range ~v or pitch ~v are not members of scale ~v" pitch-range pitch scale))
  (let* ([pcs       (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt   (length pcs)]
         [pitch-idx (pitch->idx scale pitch)]
         [xp-pitch-idx (+ pitch-idx interval)])
    (let-values ([(q r) (quotient/remainder xp-pitch-idx pcs-cnt)])
      (let ([xp-pitch (cons (pitch-class-list-ref pcs r) (octave-list-ref q))])
        (filter-pitch-in-range scale pitch-range xp-pitch)))))

;; (-> Scale? pitch/c pitch-range/c (or/c integer? (listof integer?)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/successive scale pitch-range pitch interval/intervals)
  (when (not (pitch-range&pitch-in-scale? scale pitch-range pitch))
    (error 'transpose/successive "one of pitch-range ~v or pitch ~v are not members of scale ~v" pitch-range pitch scale))
  (if (list? interval/intervals)
      (transpose/absolute scale pitch-range pitch (scanl + interval/intervals))
      (xpose scale pitch-range pitch interval/intervals)))

;; (-> Scale? pitch/c pitch-range/c (or/c integer? (listof integer?)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/absolute scale pitch-range pitch interval/intervals)
  (when (not (pitch-range&pitch-in-scale? scale pitch-range pitch))
    (error 'transpose/absolute "one of pitch-range ~v or pitch ~v are not members of scale ~v" pitch-range pitch scale))
  (if (list? interval/intervals)
      (map ((((curry xpose) scale) pitch-range) pitch) interval/intervals)
      (xpose scale pitch-range pitch interval/intervals)))

;; (->* (Scale? duration? pitch-range/c pitch/c integer? integer?) (integer?) (listof Note?))
(define (note-range scale duration pitch-range pitch start stop [step 1])
  (when (not (pitch-range&pitch-in-scale? scale pitch-range pitch))
    (error 'note-range "one of pitch-range ~v or pitch ~v are not members of scale ~v" pitch-range pitch scale))
  (let ([pitches (transpose/absolute scale pitch-range pitch (range start stop step))])
    (map (lambda (p) (Note (car p) (cdr p) duration '() #f)) pitches)))

;; (->* (Scale? duration? pitch-range/c pitch/c integer? integer?) (integer?) (listof Note?))
(define (inverted-note-ranges scale duration pitch-range pitch start stop [step 1])
  (when (not (pitch-range&pitch-in-scale? scale pitch-range pitch))
    (error 'inverted-note-range "one of pitch-range ~v or pitch ~v are not members of scale ~v" pitch-range pitch scale))
  (let ([beginning (note-range scale duration pitch-range pitch start pitch stop step)]
        [ending    (note-range scale duration pitch-range pitch stop start (- step))])
    (append beginning ending)))

;; what's the maximum index in the range of all pitches for a given scale
;; same for all diatonic scales (major, harmonic minor): 55
;; whole-tone scales max index is 47
;; chromatic scales max index is 95
(define (scale->max-idx scale)
  (let ([cnt-pcs  (length (Scale-pitch-classes scale))]
        [cnt-octs (length octave-syms)])
    (sub1 (* cnt-octs cnt-pcs))))

;; full pitch range for scale is [0..(scale->max-idx scale)] inclusive
(define (scale->pitch-range scale)
  (cons (idx->pitch scale 0) (idx->pitch scale (scale->max-idx scale))))

(module+ test
  (require rackunit)
  (check-equal?
   (transpose/successive C-major (scale->pitch-range C-major) (cons 'C '0va) '(0 -1 -2 -3 -4))
   '((C . 0va) (B . 8vb) (G . 8vb) (D . 8vb) (G . 15vb)))
  (check-exn exn:fail? (lambda () (xpose C-major (cons 'C '0va) 1000)))
  (for-each (lambda (sc)
              (for-each (lambda (n) (check-equal? n (pitch->idx sc (idx->pitch sc n))))
                        (range (scale->max-idx sc))))
            (list C-major chromatic-sharps chromatic-flats A-minor A-major)))

#|
tbd:
(define ascending-thirds (repeat-list 10 (note-range C-major 'S. (cons 'C '0va) -9 15 2)))
(define ascending-thirds-voice (SplitStaffVoice 'AcousticGrand ascending-thirds))
(define ascending-descending-thirds (repeat-list 10 (inverted-note-ranges D-major 'S (cons 'D '8vb) -7 18 2)))
(define ascending-descending-thirds-voice (SplitStaffVoice 'AcousticGrand ascending-descending-thirds))
(define voices-group (VoicesGroup
                      (TempoDur 'Q 120)
                      (TimeSignatureSimple 3 'Q)
                      (list ascending-thirds-voice ascending-descending-thirds-voice)))
(define simple-score (Score "simple" "" (list voices-group)))
|#
