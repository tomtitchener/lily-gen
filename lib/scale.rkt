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

 ;; C-anchored quarter tone scale ascending
 quarter-tones-up

 ;; C-anchored quarter tone scale descending
 quarter-tones-down

 ;; C-anchored diatonic scales
 C-whole-tone
 F-whole-tone 
 
 ;; diatonic major scales from Cff-major to Fss-major
 (matching-identifiers-out #rx".*-major" (all-defined-out))

 ;; harmonic minor scales from Aff to Dss
 (matching-identifiers-out #rx".*-minor" (all-defined-out))

 ;; pitches for a range, may be low / high or high / low
 pitch-range-pair/c

 ;; interval is exact-integer? for positive and negative
 interval/c

 ;; non-empty-listof interval/c
 intervals/c

 ;; maybe-interval is (or/c interval/c false/c)
 maybe-interval/c
 
 ;; maybe-intervals is (or/c (non-empty-listof interval/c) false/c)
 maybe-intervals/c
 
 ;; maybe-interval-or-intervals is (or/c interval/c (non-empty-listof interval/c) false/c)
 maybe-interval-or-intervals/c

 ;; non-empty-listof maybe-interval-or-intervals/c
 maybe-interval-or-intervalss/c
 
 ;; - - - - - - - - -
 ;; Utilities
 (contract-out
  [octatonic-whole-scale (-> pitch-class? Scale?)]
  
  [octatonic-half-scale (-> pitch-class? Scale?)]
  
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
  [pitch-in-range? (-> pitch-range-pair/c pitch/c maybe-pitch/c)]
  
  ;; transpose up (positive) or down (negative) from (pitch-class . octave) pair by scale steps
  ;; guard range by min max relative range [0..(scale->max-idx scale)] but possibly narrower
  ;; note: regular arithmetic so 0 is the same as unison, 1 is the same as a second, 2 is a third, etc.
  [xpose (-> Scale? pitch-range-pair/c pitch/c exact-integer? maybe-pitch/c)]
 
  ;; transpose from the starting pitch using the list of maybe-interval/c to transpose
  ;; the result of the previous step, e.g. (1 1 1 ...) for a step-by-step progression
  ;; guarding resulting pitch/c against min and max pitches
  [transpose/successive (-> Scale?
                            pitch-range-pair/c
                            pitch/c
                            maybe-interval-or-intervalss/c
                            (non-empty-listof maybe-pitch-or-pitches/c))]

  ;; transpose from the starting pitch using the list of maybe-interval/c to transpose
  ;; the result from origin e.g. (1 2 3 ...) for a step-by-step progression
  ;; guarding resulting pitch/c against min and max pitches
  [transpose/absolute (-> Scale?
                          pitch-range-pair/c
                          pitch/c
                          maybe-interval-or-intervalss/c
                          (non-empty-listof maybe-pitch-or-pitches/c))]

  ;; minimum and maximum pitches for a scale
  [scale->pitch-range-pair (-> Scale? pitch-range-pair/c)]

  ;; maximum pitches for a scale
  [scale->max-idx (-> Scale? natural-number/c)]
  
  ;; all pitches for a scale in ascending order
  ;; - if optional pitch-range-pair/c then ascending or descending sub-range within all pitches for scale
  ;; - if additional natural-number/c then in steps by value (default 1)
  [scale->pitch-range (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))]

  ;; iterate transposition of list of pitches by list of list-comprehension over sums of list of exact-integer?
  ;; for the number of generations in the first arg, answers generations each in its own list
  [transpose/iterate (-> natural-number/c
                         Scale?
                         pitch-range-pair/c
                         pitch/c
                         exact-integer?
                         (listof exact-integer?)
                         (listof exact-integer?)
                         (listof (listof maybe-pitch/c)))]
  ))

;; - - - - - - - - -
;; implementation
(require (only-in seq find))

(require (only-in racket/set set=?))

(require (only-in racket/provide matching-identifiers-out))

(require (only-in algorithms scanl))

(require (only-in srfi/1 list-index))

(require lily-gen/lib/score-syms)

(require (only-in lily-gen/lib/utils rotate-list-by carry-op-maybe))

(require (only-in lily-gen/lib/unfold iterate-list-comprehension-sum))

(define interval/c
  (make-flat-contract #:name 'interval/c #:first-order exact-integer?))

(define intervals/c
  (make-flat-contract #:name 'intervals/c #:first-order (non-empty-listof interval/c)))

(define maybe-interval/c
  (make-flat-contract #:name 'maybe-interval/c #:first-order (or/c interval/c false/c)))

(define maybe-intervals/c
  (make-flat-contract #:name 'maybe-intervals/c #:first-order (or/c (non-empty-listof interval/c) false/c)))


;; interval/c => Note, (non-empty-listof interval/c) => Chord, false/c => Rest
(define maybe-interval-or-intervals/c
  (make-flat-contract #:name 'maybe-interval-or-intervals/c #:first-order (or/c interval/c (non-empty-listof interval/c) false/c)))

(define maybe-interval-or-intervalss/c
  (make-flat-contract #:name 'maybe-interval-or-intervalss/c #:first-order (non-empty-listof maybe-interval-or-intervals/c)))

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

;;                                0 1  2  3   4 5  6  7   8 9  10 11 12 13  14 15  16 17  18 19 20 21  22 23
(define quarter-tones-up (Scale '(C Ch Cs Csh D Dh Ds Dsh E Eh F  Fh Fs Fsh G  Gh  Gs Gsh A  Ah As Ash B  Bh)))

(define quarter-tones-down (Scale '(C Cl B Bl Bf Bfl A Al Af Afl G  Gl Gf Gfl F  Fl E  El Ef Efl D  Dl  Df Dfl)))

;; TBD: generate whole tone scales given tonic

;; only two unique whole-tone scales
(define C-whole-tone (Scale '(C D E Fs Gs As)))

(define F-whole-tone (Scale '(F G A B Cs Ds)))

;; octatonic scales that start with whole tone

;; three octatonic scales that start with a whole step
(define octatonic-whole-pitch-classes-list (list '(Ef F Fs Gs A B C D)
                                                 '(D E F G Gs As B Cs)
                                                 '(Cs Ds E Fs G A Bf C)))

;; three octatonic scales that start with a half step
(define octatonic-half-pitch-classes-list (list '(D Ef F Fs Gs A B C)
                                                '(Cs D E F G Gs As B)
                                                '(C Cs Ds E Fs G A Bf)))

;; search each of lists of octatonic pitch classes  where root is at index 0, 2, 4, 6
(define/contract (octatonic-scale root pcs-list)
  (-> pitch-class? (listof (listof pitch-class?)) Scale?)
  (let ([index-scale-list 
         (find (lambda (pr)
                 (let ([index (first pr)]
                       [pcs (second pr)])
                   (symbol=? root (list-ref pcs index))))
               (for*/list ([pcs pcs-list]
                           [index '(0 2 4 6)])
                 (list index pcs)))])
    (if (not index-scale-list)
        (error 'octatonic-scale "no root ~v for octatonic pitch classes list ~v pcs-list" root pcs-list)
        (Scale (rotate-list-by (second index-scale-list) (first index-scale-list))))))

(define (octatonic-whole-scale root) (octatonic-scale root octatonic-whole-pitch-classes-list))

(define (octatonic-half-scale root) (octatonic-scale root octatonic-half-pitch-classes-list))

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
(require (for-syntax racket/syntax))

(define-syntax (make-major-scale stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax ([name (format-id #'a "~a-major" (syntax-e #'a))])
       #'(define name (major-scale 'a)))]
    [(_ a b ...)
     #'(begin
         (make-major-scale a)
         (make-major-scale b ...))]))

(define-syntax (make-minor-scale stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax ([name (format-id #'a "~a-minor" (syntax-e #'a))])
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
    ;; no key signature for any of C-major, whole-tone chromatic, or quarter tone scales
    [(or (equal? scale C-major)
         (equal? scale C-whole-tone)
         (equal? scale F-whole-tone)
         (equal? scale chromatic-sharps)
         (equal? scale chromatic-flats)
         (equal? scale quarter-tones-up)
         (equal? scale quarter-tones-down)
         (member (Scale-pitch-classes scale) octatonic-whole-pitch-classes-list set=?)
         (member (Scale-pitch-classes scale) octatonic-half-pitch-classes-list set=?))
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

;;(-> Scale? natural-number/c pitch/c)
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
  (let ([first-pitch (car pitch-range-pair)]
        [second-pitch (cdr pitch-range-pair)])
    (if (or
         (and (compare-pitches >= pitch first-pitch) (compare-pitches <= pitch second-pitch))
         (and (compare-pitches >= pitch second-pitch) (compare-pitches <= pitch first-pitch)))
        pitch
        #f)))

(define pitch-range-pair/c
  (make-flat-contract #:name 'pitch-range-pair/c #:first-order (cons/c pitch/c pitch/c)))

(define/contract (pitch-is-scale-member? scale pitch)
  (-> Scale? pitch/c boolean?)
  (if (memq (car pitch) (Scale-pitch-classes scale)) #t #f))

(define/contract (pitch-range-pair-pitches-are-scale-members? scale pitch-range-pair)
  (-> Scale? pitch-range-pair/c boolean?)
  (and (pitch-is-scale-member? scale (car pitch-range-pair))
       (pitch-is-scale-member? scale (cdr pitch-range-pair))))

;; guard xpose, transpose/successive, transpose/absolute inputs
;; scale, starting pitch and range to make sure all pitches are
;; members of scale
(define/contract (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch)
  (-> Scale? pitch-range-pair/c pitch/c boolean?)
  (and (pitch-range-pair-pitches-are-scale-members? scale pitch-range-pair)
       (pitch-is-scale-member? scale pitch)))

;; TBD: raw transpose without ranges, only errors if
;; - pitch isn't in scale
;; - sum of pitch index and interval is negative
(define/contract (xp scale pitch interval)
  (-> Scale? pitch/c interval/c pitch/c)
  (index->pitch scale (+ (pitch->index scale pitch) interval)))

;; (-> Scale? pitch-range-pair/c pitch/c exact-integer? maybe-pitch/c)
(define (xpose scale pitch-range-pair pitch interval)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'xpose "one of pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (transpose/unguarded scale pitch pitch-range-pair interval))

;; pitch and pitch-range-pair already guarded for scale, so
;; xpose, then answer either xposed-pitch or #f if xposed-pitch is not in range
;; inputs #f, interval, list of intervals for maybe-interval-or-intervals
(define/contract (transpose/unguarded scale pitch pitch-range-pair maybe-interval-or-intervals)
  (-> Scale? pitch/c pitch-range-pair/c maybe-interval-or-intervals/c maybe-pitch-or-pitches/c)
  #;(printf "transpose/unguarded pitch ~v maybe-interval-or-intervals ~v\n" pitch maybe-interval-or-intervals)
  (match maybe-interval-or-intervals
    [#f ;; input is #f so output #f is interpreted as a rest
     #;(printf "#f\n")
     #f] 
    [(cons first-interval rest-intervals) ;; input is chord, xpose root, then rest of chord from root
     (let* ([root (transpose/unguarded scale pitch pitch-range-pair first-interval)]
            [chord (cons root (map (curry transpose/unguarded scale root pitch-range-pair) rest-intervals))])
       #;(printf "first-interval: ~v rest-intervals: ~v\n" first-interval rest-intervals)
       #;(printf "root: ~v chord: ~v\n" root chord)
       chord)]
    [(var maybe-interval) ;; input is single interval
     (let* ([pitch-idx (pitch->index scale pitch)]
            [pitch-off (+ pitch-idx maybe-interval)])
       (if (or (< pitch-off 0) (> pitch-off (scale->max-idx scale)))
           (error 'transpose/unguarded "transposition ~v for pitch idx ~v to ~v exceeds max range" maybe-interval pitch-idx pitch-off)
           (let ([xposed-pitch (index->pitch scale (+ pitch-idx maybe-interval))])
             (if (pitch-in-range? pitch-range-pair xposed-pitch)
                 (begin
                   #;(printf "single interval ~v tranposed ~v\n" maybe-interval xposed-pitch)
                   xposed-pitch
                   )
                 (error 'transpose/unguarded "transposed pitch ~v failed range check ~v" xposed-pitch pitch-range-pair)))))]))

;; converts list of successive intervals into absolute intervals
;; LHS  RHS
;; list list => take first element from LHS and sum all elements on RHS
;; list item => take first element from LHS and sum with item on RHS
;; item list => sum itm from LHS against all elementson RHS
;; item item => sum two items as ordinary +
(define (list-sum rhs lhs)
  #;(printf "list-sum lhs: ~v rhs: ~v\n" lhs rhs)
  (cond
    [(or (not lhs) (not rhs)) ;; redundant when op for carry-op-maybe
     #;(printf "list-sum #f #f  ~v ~v => #f\n" rhs lhs)
     #f]
    [(and (list? lhs) (list? rhs))
     #;(printf "list-sum #t #t ~v ~v => ~v\n" rhs lhs (cons (+ (first lhs) (car rhs)) (cdr rhs)))
     (cons (+ (first lhs) (car rhs)) (cdr rhs))]
    [(and (list? lhs) (not (list? rhs)))
     #;(printf "list-sum #t #f ~v ~v => ~v\n" rhs lhs (+ (car lhs) rhs))
     (+ (car lhs) rhs)]
    ;;;
    [(and (not (list? lhs)) (list? rhs))
     #;(printf "list-sum #f #t ~v ~v => ~v\n" rhs lhs (cons (+ lhs (car rhs)) (cdr rhs)))
     (cons (+ lhs (car rhs)) (cdr rhs))]
    [else
     #;(printf "list-sum else ~v ~v => ~v\n" rhs lhs (+ lhs rhs))
     (+ lhs rhs)]))

;; bug: only transposes chord when *first* element in maybe-interval-or-intervalss is a list
;;      and then it repeats that chord with each successive transposition
;;      forgets chords inside of list
;; correct behavior is to take starting pitch and apply each of maybe-interval-or-intervalss
;; but (scanl (carry-op-maybe list-sum) maybe-interval-or-intervalss) swallows 

;; (-> Scale? pitch-range-pair/c pitch/c (non-empty-listof maybe-interval-or-intervals/c) (non-empty-listof maybe-pitch-or-pitches/c))]
(define (transpose/successive scale pitch-range-pair pitch maybe-interval-or-intervalss)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'transpose/successive "pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (map (curry transpose/unguarded scale pitch pitch-range-pair) (scanl (carry-op-maybe list-sum) maybe-interval-or-intervalss)))

;; (-> Scale? pitch-range-pair/c pitch/c (non-empty-listof maybe-interval-or-intervals/c) (non-empty-listof maybe-pitch-or-pitches/c))]
(define (transpose/absolute scale pitch-range-pair pitch maybe-interval-or-intervalss)
  (when (not (pitch-range-pair&pitch-in-scale? scale pitch-range-pair pitch))
    (error 'transpose/absolute "pitch-range-pair ~v or pitch ~v are not members of scale ~v" pitch-range-pair pitch scale))
  (map (curry transpose/unguarded scale pitch pitch-range-pair) maybe-interval-or-intervalss))

;; the max index for a list of pitches for the scale
;; starting from 0 to the final index, inclusive
;; - diatonic scales (major, harmonic minor): (0 . 55) 
;; - whole-tone scales: (0 . 47)
;; - chromatic scales: (0 . 5)]
(define (scale->max-idx scale)
  (let ([cnt-pcs  (length (Scale-pitch-classes scale))]
        [cnt-octs (length octave-syms)])
    (sub1 (* cnt-octs cnt-pcs))))

;; full pitch range for scale is [0..(scale->max-idx scale)] inclusive
;; (-> Scale? pitch-range-pair/c)
(define (scale->pitch-range-pair scale)
  (cons (index->pitch scale 0) (index->pitch scale (scale->max-idx scale))))

(define/contract (scale->all-pitches scale)
  (-> Scale? (listof pitch/c))
  (for*/list ([oct octave-syms]
              [pit-cls (Scale-pitch-classes (scale->c-ordering scale))])
    (cons pit-cls oct)))

;; (->* (Scale?) (pitch-range-pair/c natural-number/c) (listof pitch/c))
(define (scale->pitch-range scale [pitch-range-pair-or-f #f] [step 1])
  (let ([all-pitches (scale->all-pitches scale)]
        [pitch-range-pair (if pitch-range-pair-or-f pitch-range-pair-or-f (scale->pitch-range-pair scale))])
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
   (transpose/successive C-major (scale->pitch-range-pair C-major) (cons 'C '0va) '(0 -1 -2 -3 -4))
   '((C . 0va) (B . 8vb) (G . 8vb) (D . 8vb) (G . 15vb)))
  (check-exn exn:fail? (lambda () (xpose C-major (cons 'C '0va) 1000)))
  (for-each (lambda (sc)
              (for-each (lambda (n) (check-equal? n (pitch->index sc (index->pitch sc n))))
                        (range (scale->max-idx sc))))
            (list C-major chromatic-sharps chromatic-flats A-minor A-major)))


;; Returned (listof (listof maybe-pitch/c)) is by generation (0 1 2 ..)
;;(-> natural-number/c
;;    Scale?
;;    pitch-range-pair/c
;;    pitch/c exact-integer?
;;    (listof exact-integer?)
;;    (listof exact-integer?)
;;    (listof (listof maybe-pitch/c)))
(define (transpose/iterate generations scale pitch-range-pair start-pitch offset kernel-intervals init-intervals)
  (let ([self-sim-indexess (iterate-list-comprehension-sum generations offset kernel-intervals init-intervals)])
    (map (curry transpose/absolute scale pitch-range-pair start-pitch) self-sim-indexess)))
        
(module+ test
  (require rackunit)
  
  (check-equal?
   (second (transpose/iterate 3 C-major (scale->pitch-range-pair C-major) (cons 'C '0va) 0 '(0 5 2) '(1 2 3)))
   '((D . 0va) (E . 0va) (F . 0va) (B . 0va) (C . 8va) (D . 8va) (F . 0va) (G . 0va) (A . 0va)))
  )
