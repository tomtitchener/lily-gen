#lang racket

;; scale:
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

 ;; min/max pitches for a range
 pitch-range-pair/c
 
 ;; - - - - - - - - -
 ;; Utilities
 (contract-out
  [idx->pitch (-> Scale? natural-number/c pitch/c)]
  
  [pitch->idx (-> Scale? pitch/c natural-number/c)]

  [pitch->chromatic-idx (-> pitch/c natural-number/c)]
 
  ;; transpose up (positive) or down (negative) from (pitch-class . octave) pair by scale steps
  ;; guard range by min max relative range [0..(scale->max-idx scale)] but possibly narrower
  ;; note: regular arithmetic so 0 is the same as unison, 1 is the same as a second, 2 is a third, etc.
  [xpose (-> Scale? pitch-range-min-max-pair/c pitch/c signed-integer/c maybe-pitch/c)]
 
  ;; transpose from the starting pitch using the list of integers to transpose
  ;; the result of the previous step, e.g. (1 1 1 ...) for a step-by-step progression
  [transpose/successive (-> Scale?
                            pitch-range-min-max-pair/c
                            pitch/c
                            (or/c signed-integer/c (listof signed-integer/c))
                            (or/c maybe-pitch/c (listof maybe-pitch/c)))]

  ;; transpose from the starting pitch using a single signed-integer/c or a list of signed-integer/c
  ;; guarding resulting pitch/c against min and max pitches
  ;; for a single signed-integer/c, just provide single pitch/c that is transpose/absolute called once
  ;; else for a list of signed-integer/c, answer multiple pitch/c to transpose repeatedly from the
  ;; starting pitch, e.g. (1 2 3 ...) for a step-by-step progression
  [transpose/absolute (-> Scale?
                          pitch-range-min-max-pair/c
                          pitch/c
                          (or/c signed-integer/c (listof signed-integer/c))
                          (or/c maybe-pitch/c maybe-pitch/c))]

  ;; minimum and maximum pitches for a scale
  [scale->pitch-range-min-max-pair (-> Scale? pitch-range-min-max-pair/c)]

  ;; all pitches for a scale in ascending order
  ;; - if optional pitch-range-min-max-pair/c then ascending or descending sub-range within all pitches for scale
  ;; - if additional natural-number/c then in steps by value (default 1)
  [scale->pitch-range (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))]))

;; - - - - - - - - -
;; implementation
(require (only-in racket/provide matching-identifiers-out))

(require (only-in seq find))

(require (only-in algorithms scanl))

(require (only-in srfi/1 list-index))

(require binary-class/contract)

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

(define/contract (pitch-fifths-ordered-pitch-class-sym-indices cmp sym1 sym2)
  (-> (-> natural-number/c natural-number/c boolean?) pitch-class? pitch-class? boolean?)
  (cmp (fifths-ordered-pitch-class-index sym1) (fifths-ordered-pitch-class-index sym2)))

;; when pitch-classes are ordered by fifths, collect the list of seven
;; pitch-classes starting from one below the tonic gives you the major
;; scale pitches e.g. (F C G D A E B)
;; then indexing into list starting at index 1 incrementing by 2 modulo
;; the length of the list gives the pitches in order (C D E F G A B)
(define/contract (major-scale root-pc [err-sym 'major-scale])
  (->* (pitch-class?) (symbol?) Scale?)
  (when (pitch-fifths-ordered-pitch-class-sym-indices < root-pc 'Cff)
    (error err-sym "root pitch ~v is below minimum ~a" root-pc 'Cff))
  (when (pitch-fifths-ordered-pitch-class-sym-indices > root-pc 'Fss)
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
  (when (pitch-fifths-ordered-pitch-class-sym-indices < root-pc 'Aff)
    (error 'minor-scale "root pitch ~v is below minimum ~v" root-pc 'Aff))
  (when (pitch-fifths-ordered-pitch-class-sym-indices > root-pc 'Dss)
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
     (with-syntax ([name (format-id #'a "~a-major" #'a)])
       #'(define name (major-scale 'a)))]
    [(_ a b ...)
     #'(begin
         (make-major-scale a)
         (make-major-scale b ...))]))

(define-syntax (make-minor-scale stx)
  (syntax-case stx ()
    [(_ a)
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

;; (-> Scale? natural-number/c pitch/c)
(define (idx->pitch scale idx)
  (when (> idx (scale->max-idx scale))
    (error 'idx->pitch "idx ~v is > max ~v for scale ~v\n" idx (scale->max-idx scale) scale))
  (let* ([pitch-classes       (Scale-pitch-classes (scale->c-ordering scale))]
         [pitch-classes-count (length pitch-classes)])
    (let-values ([(q r) (quotient/remainder idx pitch-classes-count)])
      (cons (pitch-class-list-ref pitch-classes r) (octave-list-ref q)))))

;; index for pitch is the count of pitches from the lowest possible pitch of c-ordered scale
;; (-> Scale? pitch/c natural-number/c)
(define (pitch->idx scale pitch)
  (let* ([octave-idx          (octave-list-idx (cdr pitch))]
         [pitch-classes       (Scale-pitch-classes (scale->c-ordering scale))]
         [pitch-classes-idx   (pitch-class-list-idx pitch-classes (car pitch))]
         [pitch-classes-count (length pitch-classes)])
    (+ (* octave-idx pitch-classes-count) pitch-classes-idx)))

;; (-> pitch/c natural-number/c)
(define (pitch->chromatic-idx pitch)
  (let ([octave-idx (octave-list-idx (cdr pitch))]
        [pitch-classes-idx (pitch-class->chromatic-enharmonic-index (car pitch))]
        [pitch-classes-count (length c-ordered-enharmonic-pitch-class-symss)])
    (+ (* octave-idx pitch-classes-count) pitch-classes-idx)))

(define pitch-range-pair/c
  (make-flat-contract #:name 'pitch-range-pair/c #:first-order (cons/c pitch/c pitch/c)))

;; tbd: make into structs to verify construction, e.g. pitch-range-min-max-pair/c should test first is lower than second
(define pitch-range-min-max-pair/c
  (make-flat-contract #:name 'pitch-range-min-max-pair/c #:first-order (cons/c pitch/c pitch/c)))

;; to compare two pitches from the same scale compare their pitch indices 
(define/contract (compare-pitches op scale pitch-1 pitch-2)
  (-> (-> any/c any/c boolean?) Scale? pitch/c pitch/c boolean?)
  (op (pitch->idx scale pitch-1) (pitch->idx scale pitch-2)))

;; pitch-range-pair is inclusive
(define/contract (pitch-in-range? scale pitch-range-pair pitch)
  (-> Scale? pitch-range-min-max-pair/c pitch/c boolean?)
  (let ([min-pitch (car pitch-range-pair)]
        [max-pitch (cdr pitch-range-pair)])
    (and (compare-pitches >= scale pitch min-pitch)
         (compare-pitches <= scale pitch max-pitch))))

(define/contract (pitch-is-scale-member? scale pitch)
  (-> Scale? pitch/c boolean?)
  (if (memq (car pitch) (Scale-pitch-classes scale)) #t #f))

(define/contract (pitch-range-pair-pitches-are-scale-members? scale pitch-range-pair)
  (-> Scale? pitch-range-min-max-pair/c boolean?)
  (and (pitch-is-scale-member? scale (car pitch-range-pair))
       (pitch-is-scale-member? scale (cdr pitch-range-pair))))

;; guard xpose, transpose/successive, transpose/absolute inputs
;; scale, starting pitch and range to make sure all pitches are
;; members of scale
(define/contract (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch)
  (-> Scale? pitch-range-min-max-pair/c pitch/c  boolean?)
  (and (pitch-range-pair-pitches-are-scale-members? scale pitch-range-pair)
       (pitch-is-scale-member? scale pitch)))

;; Note: slightly expensive due to call to (scale->pitch-range scale),
;; but guards input ranges
;; (-> Scale? pitch-range-min-max-pair/c pitch/c signed-integer/c maybe-pitch/c)
(define (xpose scale pitch-range-pair pitch interval)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'xpose "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (xpose/internal scale pitch (scale->pitch-range scale) interval))

(define (xpose/internal scale pitch pitch-range interval)
  (let ([pitch-idx (list-index ((curry equal?) pitch) pitch-range)])
    (idx->pitch scale (+ pitch-idx interval))))

;; (-> Scale? pitch/c pitch-range-min-max-pair/c (or/c signed-integer/c (listof signed-integer/c)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/successive scale pitch-range-pair pitch interval/intervals)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'transpose/successive "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (let ([pitch-range (scale->pitch-range scale)])
    (if (list? interval/intervals)
        (map (lambda (interval) (xpose/internal scale pitch pitch-range interval)) (scanl + interval/intervals))
        (xpose/internal scale pitch pitch-range interval/intervals))))

;; (-> Scale? pitch/c pitch-range-min-max-pair/c (or/c signed-integer/c (listof signed-integer/c)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/absolute scale pitch-range-pair pitch interval/intervals)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'transpose/absolute "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (let ([pitch-range (scale->pitch-range scale)])
    (if (list? interval/intervals)
        (map (lambda (interval) (xpose/internal scale pitch pitch-range interval)) interval/intervals)
        (xpose/internal scale pitch pitch-range interval/intervals))))

;; the max index for a list of pitches for the scale
;; starting from 0 to the final index, inclusive
;; - diatonic scales (major, harmonic minor): (0 . 55) 
;; - whole-tone scales: (0 . 47)
;; - chromatic scales: (0 . 95)]
(define/contract (scale->max-idx scale)
  (-> Scale? unsigned-integer/c)
  (let ([cnt-pcs  (length (Scale-pitch-classes scale))]
        [cnt-octs (length octave-syms)])
    (sub1 (* cnt-octs cnt-pcs))))

;; full pitch range for scale is [0..(scale->max-idx scale)] inclusive
;; (-> Scale? pitch-range-min-max-pair/c)
(define (scale->pitch-range-min-max-pair scale)
  (cons (idx->pitch scale 0) (idx->pitch scale (scale->max-idx scale))))

(define/contract (scale->all-pitches scale)
  (-> Scale? (listof pitch/c))
  (for*/list ([oct octave-syms]
              [pit-cls (Scale-pitch-classes (scale->c-ordering scale))])
    (cons pit-cls oct)))

;; (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))
(define (scale->pitch-range scale [pitch-range-pair-or-f #f] [step 1])
  (let ([all-pitches (scale->all-pitches scale)]
        [pitch-range-pair (if pitch-range-pair-or-f pitch-range-pair-or-f (scale->pitch-range-min-max-pair scale))])
    (let ([start-idx (list-index ((curry equal?) (car pitch-range-pair)) all-pitches)]
          [stop-idx  (list-index ((curry equal?) (cdr pitch-range-pair)) all-pitches)])
      (when (= start-idx stop-idx)
        (error 'scale-pitch-range "pitch-range-pair ~v have equal indexes" pitch-range-pair))
      (let ([inner-range-pitches
             (if (< start-idx stop-idx)
                 (take (drop all-pitches start-idx)
                       (add1 (- stop-idx start-idx)))
                 (take (drop (reverse all-pitches) (sub1 (- (length all-pitches) start-idx)))
                       (add1 (- start-idx stop-idx))))]
            [next-step? (let ([c 0])
                          (lambda (_)
                            (let ([ret (zero? (modulo c step))])
                              (set! c (add1 c))
                              ret)))])
        (filter next-step? inner-range-pitches)))))

(module+ test
  (require rackunit)
  (define (scale->pitch-range/old scale)
    (let ([max-idx (scale->max-idx scale)]
          [start-pitch (idx->pitch scale 0)])
      (transpose/absolute scale (scale->pitch-range-min-max-pair scale) start-pitch (range 0 (add1 max-idx)))))
  (check-equal? (scale->pitch-range C-major) (scale->pitch-range/old C-major))
  (check-equal? (scale->pitch-range Cff-major) (scale->pitch-range/old Cff-major))
  (check-equal? (scale->pitch-range Fss-major) (scale->pitch-range/old Fss-major))
  (check-equal? (scale->pitch-range C-major) (scale->pitch-range/old C-major))
  (define/contract (filter-pitch-in-range scale pitch-range-pair pitch)
    (-> Scale? pitch-range-min-max-pair/c pitch/c maybe-pitch/c)
    (if (pitch-in-range? scale pitch-range-pair pitch) pitch #f))
  (define (xpose/old scale pitch-range-pair pitch interval)
    (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
      (error 'xpose "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
    (let* ([pcs       (Scale-pitch-classes (scale->c-ordering scale))]
           [pcs-cnt   (length pcs)]
           [pitch-idx (pitch->idx scale pitch)]
           [xp-pitch-idx (+ pitch-idx interval)])
      (let-values ([(q r) (quotient/remainder xp-pitch-idx pcs-cnt)])
        (let ([xp-pitch (cons (pitch-class-list-ref pcs r) (octave-list-ref q))])
          (filter-pitch-in-range scale pitch-range-pair xp-pitch)))))
  (check-equal? (xpose C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) 1)
                (xpose/old C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) 1))
  (check-equal? (xpose C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) -1)
                (xpose/old C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) -1))
  (check-equal? (xpose C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) 15)
                  (xpose/old C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) 15))
  (check-equal? (xpose C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) -15)
                (xpose/old C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) -15))
  (check-equal?
   (transpose/successive C-major (scale->pitch-range-min-max-pair C-major) (cons 'C '0va) '(0 -1 -2 -3 -4))
   '((C . 0va) (B . 8vb) (G . 8vb) (D . 8vb) (G . 15vb)))
  (check-exn exn:fail? (lambda () (xpose C-major (cons 'C '0va) 1000)))
  (for-each (lambda (sc)
              (for-each (lambda (n) (check-equal? n (pitch->idx sc (idx->pitch sc n))))
                        (range (scale->max-idx sc))))
            (list C-major chromatic-sharps chromatic-flats A-minor A-major)))
