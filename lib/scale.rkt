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

 ;; pitches for a range, may be low / high or high / low
 pitch-range-pair/c
 
 ;; - - - - - - - - -
 ;; Utilities
 (contract-out
  [scale->key-signature-values  (-> Scale? (values pitch-class? mode?))]
  
  [pitch->chromatic-index (-> pitch/c natural-number/c)]
    
  ;; what is the index in c-ordered-enharmonic-pitch-class-symss for pitch-class?
  [pitch-class->chromatic-enharmonic-index (-> pitch-class? natural-number/c)]
  
  ;; pitch for index from full-range of pitches for scale for all pitches for all octaves
  [index->pitch (-> Scale? natural-number/c pitch/c)]
  
  ;; index for pitch is the count of pitches from the lowest possible pitch of c-ordered scale
  [pitch->index (-> Scale? pitch/c natural-number/c)]

  ;; compare two pitches from the same scale by the relational operator in the first argument
  [compare-pitches (-> (-> any/c any/c boolean?) pitch/c pitch/c boolean?)]

  ;; pitch-range-pair is inclusive
  [pitch-in-range? (-> PitchRangeMinMaxPair? pitch/c maybe-pitch/c)]
  
  ;; transpose up (positive) or down (negative) from (pitch-class . octave) pair by scale steps
  ;; guard range by min max relative range [0..(scale->max-idx scale)] but possibly narrower
  ;; note: regular arithmetic so 0 is the same as unison, 1 is the same as a second, 2 is a third, etc.
  [xpose (-> Scale? PitchRangeMinMaxPair? pitch/c exact-integer? maybe-pitch/c)]
 
  ;; transpose from the starting pitch using the list of integers to transpose
  ;; the result of the previous step, e.g. (1 1 1 ...) for a step-by-step progression
  [transpose/successive (-> Scale?
                            PitchRangeMinMaxPair?
                            pitch/c
                            (or/c exact-integer? (listof exact-integer?))
                            (or/c maybe-pitch/c (listof maybe-pitch/c)))]

  ;; transpose from the starting pitch using a single exact-integer? or a list of exact-integer?
  ;; guarding resulting pitch/c against min and max pitches
  ;; for a single exact-integer?, just provide single pitch/c that is transpose/absolute called once
  ;; else for a list of exact-integer?, answer multiple pitch/c to transpose repeatedly from the
  ;; starting pitch, e.g. (1 2 3 ...) for a step-by-step progression
  [transpose/absolute (-> Scale?
                          PitchRangeMinMaxPair?
                          pitch/c
                          (or/c exact-integer? (listof exact-integer?))
                          (or/c maybe-pitch/c (listof maybe-pitch/c)))]

  ;; minimum and maximum pitches for a scale
  [scale->PitchRangeMinMaxPair (-> Scale? PitchRangeMinMaxPair?)]

  ;; all pitches for a scale in ascending order
  ;; - if optional PitchRangeMinMaxPair? then ascending or descending sub-range within all pitches for scale
  ;; - if additional natural-number/c then in steps by value (default 1)
  [scale->pitch-range (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))]

  ;; iterate transposition of list of pitches by list of list-comprehension over sums of list of exact-integer?
  ;; for the number of generations in the first arg,
  ;; answer generations each in its own list
  [transpose/iterate (-> natural-number/c
                         Scale?
                         PitchRangeMinMaxPair?
                         pitch/c
                         exact-integer?
                         (listof exact-integer?)
                         (listof exact-integer?)
                         (listof (listof maybe-pitch/c)))]
  ))

;; - - - - - - - - -
;; implementation
(require (only-in racket/provide matching-identifiers-out))

(require (only-in algorithms scanl))

(require (only-in srfi/1 list-index))

(require (only-in lily-gen/lib/score-syms octave-syms octave-list-ref octave-list-idx pitch-class? duration? pitch/c maybe-pitch/c mode?))

(require (only-in lily-gen/lib/utils rotate-list-by))

(require (only-in lily-gen/lib/unfold iterate-list-comprehension-sum))

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

;; TBD: update macros to include tonic and mode fields in Scale so they don't have to be computed
(struct/contract Scale ([pitch-classes (and/c (non-empty-listof pitch-class?) no-duplicates/c)]) #:transparent)

;; no good way to capture chromatic or whole-tone pitches as a scale,
;; choice of e.g. Cs vs Df depends on context for both

;; chromatic "scales" are enharmonically identical
(define chromatic-sharps (Scale '(C Cs D Ds E F Fs G Gs A As B)))

(define chromatic-flats (Scale '(C Df D Ef E F Gf G Af A Bf Cf)))

;; only two unique whole-tone scales
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

;; (-> pitch/c natural-number/c)
(define (pitch->chromatic-index pitch)
  (let ([octave-idx (octave-list-idx (cdr pitch))]
        [pitch-classes-idx (pitch-class->chromatic-enharmonic-index (car pitch))]
        [pitch-classes-count (length c-ordered-enharmonic-pitch-class-symss)])
    (+ (* octave-idx pitch-classes-count) pitch-classes-idx)))

;; (-> pitch/c natural-number/c)
(define (pitch-class->chromatic-enharmonic-index pitch-class)
  (index-where c-ordered-enharmonic-pitch-class-symss (lambda (enh-syms) (member pitch-class enh-syms))))

;; order pitch classes in a scale for transposition relative to C octaves for lilypond notation
(define/contract (scale->c-ordering scale)
  (-> Scale? Scale?)
  (Scale (sort (Scale-pitch-classes scale) < #:key pitch-class->chromatic-enharmonic-index)))

(define/contract (scale->fifths-ordering scale)
  (-> Scale? Scale?)
  (Scale (sort (Scale-pitch-classes scale) < #:key fifths-ordered-pitch-class-index)))

;; special-cases for chromatic and whole-tone scales:  neutral key signature, no flats, no sharps
;; (-> Scale? (values pitch-class? mode?))
(define (scale->key-signature-values scale)
  (cond
    ;; no key signature for any of C-major, whole-tone or chromatic scales
    [(or (equal? scale C-major) (equal? scale C-whole-tone) (equal? scale Df-whole-tone) (equal? scale chromatic-sharps) (equal? scale chromatic-flats))
     (values 'C 'Major)]
    [else
     ;; make sure order of pitches in scale hasn't been re-arranged to match octaves so tonic isn't first in list of pitches
     (when (and (andmap symbol=? (Scale-pitch-classes scale) (Scale-pitch-classes (scale->c-ordering scale)))
                (not (andmap symbol=? (Scale-pitch-classes scale) (Scale-pitch-classes C-major))))
       (error 'scale->key-signature-values "scale ~v is in c-ordering" scale))
     ;; tonic is just first of pitch-classes in scale,
     ;; major is when scale as ordered by fifths has tonic as second in list, minor when it's fourth
     (let* ([tonic (car (Scale-pitch-classes scale))]
            [fifths-ordered-scale (scale->fifths-ordering scale)]
            [fifths-ordered-scale-pitches (Scale-pitch-classes fifths-ordered-scale)])
       (let ([mode
                 (cond
                   [(symbol=? tonic (list-ref fifths-ordered-scale-pitches 1)) 'Major]
                   [(symbol=? tonic (list-ref fifths-ordered-scale-pitches 4)) 'Minor]
                   ;; TBD: if using modal scales, will need to adjust
                   [else (error 'scale->key-signature-values
                                "cannot find tonic ~v in second or fourth element of fifths-ordering ~v of scale ~v"
                                tonic fifths-ordered-scale scale)])])
         (values tonic mode)))]))
      
(define (index->pitch scale idx)
  (when (> idx (scale->max-idx scale))
    (error 'index->pitch "idx ~v is > max ~v for scale ~v\n" idx (scale->max-idx scale) scale))
  (let* ([pitch-classes       (Scale-pitch-classes (scale->c-ordering scale))]
         [pitch-classes-count (length pitch-classes)])
    (let-values ([(q r) (quotient/remainder idx pitch-classes-count)])
      (cons (pitch-class-list-ref pitch-classes r) (octave-list-ref q)))))

;; (-> Scale? pitch/c natural-number/c)
(define (pitch->index scale pitch)
  (let* ([octave-idx          (octave-list-idx (cdr pitch))]
         [pitch-classes       (Scale-pitch-classes (scale->c-ordering scale))]
         [pitch-classes-idx   (pitch-class-list-idx pitch-classes (car pitch))]
         [pitch-classes-count (length pitch-classes)])
    (+ (* octave-idx pitch-classes-count) pitch-classes-idx)))

;; to compare two pitches from the same scale compare their pitch indices 
(define (compare-pitches op pitch-1 pitch-2)
  (op (pitch->chromatic-index pitch-1) (pitch->chromatic-index pitch-2)))

;; pitch-range-pair is inclusive
(define (pitch-in-range? pitch-range-pair pitch)
  (let ([min-pitch (PitchRangeMinMaxPair-min pitch-range-pair)]
        [max-pitch (PitchRangeMinMaxPair-max pitch-range-pair)])
    (if (and (compare-pitches >= pitch min-pitch) (compare-pitches <= pitch max-pitch))
        pitch
        #f)))

(define pitch-range-pair/c
  (make-flat-contract #:name 'pitch-range-pair/c #:first-order (cons/c pitch/c pitch/c)))

(define/contract (pitch-range-min-max-pair-ctor scale min max type-name)
  (-> Scale? pitch/c pitch/c symbol? (values Scale? pitch/c pitch/c))
  (when (compare-pitches >= min max)
    (error 'PitchRangeMinMaxPair "min %v is not < max %v" min max))
  (values scale min max))

(struct PitchRangeMinMaxPair (scale min max) #:guard pitch-range-min-max-pair-ctor #:transparent)

(define/contract (pitch-is-scale-member? scale pitch)
  (-> Scale? pitch/c boolean?)
  (if (memq (car pitch) (Scale-pitch-classes scale)) #t #f))

(define/contract (pitch-range-pair-pitches-are-scale-members? scale pitch-range-pair)
  (-> Scale? PitchRangeMinMaxPair? boolean?)
  (and (pitch-is-scale-member? scale (PitchRangeMinMaxPair-min pitch-range-pair))
       (pitch-is-scale-member? scale (PitchRangeMinMaxPair-max pitch-range-pair))))

;; guard xpose, transpose/successive, transpose/absolute inputs
;; scale, starting pitch and range to make sure all pitches are
;; members of scale
(define/contract (pitch-range-pair&pitch-in-scale? scale pitch-range-min-max-pair pitch)
  (-> Scale? PitchRangeMinMaxPair? pitch/c boolean?)
  (and (pitch-range-pair-pitches-are-scale-members? scale pitch-range-min-max-pair)
       (pitch-is-scale-member? scale pitch)))

;; (-> Scale? PitchRangeMinMaxPair? pitch/c exact-integer? maybe-pitch/c)
(define (xpose scale pitch-range-min-max-pair pitch interval)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-min-max-pair pitch))
    (error 'xpose "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-min-max-pair pitch scale))
  (xpose/internal scale pitch (scale->pitch-range scale) pitch-range-min-max-pair interval))

;; pitch and pitch-range-min-max-pair already guarded, so
;; xpose, then answer either xposed-pitch or #f if xposed-pitch is not in range
(define/contract (xpose/internal scale pitch pitch-range-min-max-pair interval)
  (-> Scale? pitch/c PitchRangeMinMaxPair? exact-integer? maybe-pitch/c)
  (let* ([pitch-idx (pitch->index scale pitch)]
         [xposed-pitch (index->pitch scale (+ pitch-idx interval))])
    (pitch-in-range? pitch-range-min-max-pair xposed-pitch)))

;; (-> Scale? PitchRangeMinMaxPair? pitch/c (or/c exact-integer? (listof exact-integer?)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/successive scale pitch-range-min-max-pair pitch interval/intervals)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-min-max-pair pitch))
    (error 'transpose/successive "pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-min-max-pair pitch scale))
  (if (list? interval/intervals)
      (map (lambda (interval) (xpose/internal scale pitch pitch-range-min-max-pair interval)) (scanl + interval/intervals))
      (xpose/internal scale pitch pitch-range-min-max-pair interval/intervals)))

;; (-> Scale? PitchRangeMinMaxPair? pitch/c (or/c exact-integer? (listof exact-integer?)) (or/c maybe-pitch/c (listof maybe-pitch/c)))
(define (transpose/absolute scale pitch-range-min-max-pair pitch interval/intervals)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-min-max-pair pitch))
    (error 'transpose/absolute "pitch-range-min-max-pair ~v or pitch ~v are not members of scale ~v" pitch-range-min-max-pair pitch scale))
  (if (list? interval/intervals)
      (map (lambda (interval) (xpose/internal scale pitch pitch-range-min-max-pair interval)) interval/intervals)
      (xpose/internal scale pitch pitch-range-min-max-pair interval/intervals)))

;; the max index for a list of pitches for the scale
;; starting from 0 to the final index, inclusive
;; - diatonic scales (major, harmonic minor): (0 . 55) 
;; - whole-tone scales: (0 . 47)
;; - chromatic scales: (0 . 5)]
(define/contract (scale->max-idx scale)
  (-> Scale? natural-number/c)
  (let ([cnt-pcs  (length (Scale-pitch-classes scale))]
        [cnt-octs (length octave-syms)])
    (sub1 (* cnt-octs cnt-pcs))))

;; full pitch range for scale is [0..(scale->max-idx scale)] inclusive
;; (-> Scale? PitchRangeMinMaxPair?)
(define (scale->PitchRangeMinMaxPair scale)
  (PitchRangeMinMaxPair scale (index->pitch scale 0) (index->pitch scale (scale->max-idx scale))))

(define/contract (scale->all-pitches scale)
  (-> Scale? (listof pitch/c))
  (for*/list ([oct octave-syms]
              [pit-cls (Scale-pitch-classes (scale->c-ordering scale))])
    (cons pit-cls oct)))

;; (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))
(define (scale->pitch-range scale [pitch-range-pair-or-f #f] [step 1])
  (let ([all-pitches (scale->all-pitches scale)]
        [pitch-range-pair (if pitch-range-pair-or-f pitch-range-pair-or-f (scale->PitchRangeMinMaxPair scale))])
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
  (check-equal?
   (transpose/successive C-major (scale->PitchRangeMinMaxPair C-major) (cons 'C '0va) '(0 -1 -2 -3 -4))
   '((C . 0va) (B . 8vb) (G . 8vb) (D . 8vb) (G . 15vb)))
  (check-exn exn:fail? (lambda () (xpose C-major (cons 'C '0va) 1000)))
  (for-each (lambda (sc)
              (for-each (lambda (n) (check-equal? n (pitch->index sc (index->pitch sc n))))
                        (range (scale->max-idx sc))))
            (list C-major chromatic-sharps chromatic-flats A-minor A-major)))


;; Returned (listof (listof maybe-pitch/c)) is by generation (0 1 2 ..)
;;(-> natural-number/c
;;    Scale?
;;    PitchRangeMinMaxPair?
;;    pitch/c exact-integer?
;;    (listof exact-integer?)
;;    (listof exact-integer?)
;;    (listof (listof maybe-pitch/c)))
(define (transpose/iterate generations scale pitch-range-min-max-pair start-pitch offset kernel-intervals init-intervals)
  (let ([self-sim-indexess (iterate-list-comprehension-sum generations offset kernel-intervals init-intervals)])
    (map (curry transpose/absolute scale pitch-range-min-max-pair start-pitch) self-sim-indexess)))
        
(module+ test
  (require rackunit)
  
  (check-equal?
   (second (transpose/iterate 3 C-major (scale->PitchRangeMinMaxPair C-major) (cons 'C '0va) 0 '(0 5 2) '(1 2 3)))
   '((D . 0va) (E . 0va) (F . 0va) (B . 0va) (C . 8va) (D . 8va) (F . 0va) (G . 0va) (A . 0va))))