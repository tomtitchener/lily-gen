#lang racket

(require br/macro)
(require racket/provide)
(require srfi/1)
(require (only-in srfi/1 list-index))
(require "score.rkt")
(require "utils.rkt")

(provide (struct-out Scale)
         chromatic-sharps
         chromatic-flats
         C-whole-tone
         Df-whole-tone
         xpose
         transpose/successive
         transpose/absolute
         note-range
         inverted-note-ranges
         (matching-identifiers-out
             #rx".*-major"
             (all-defined-out))
         (matching-identifiers-out
             #rx".*-minor"
             (all-defined-out)))

(define/contract (octave-list-idx oct)
  (-> octave? natural?)
  (list-index ((curry eq?) oct) octave-syms))

(define/contract (octave-list-ref idx)
  (-> natural? octave?)
  (when (or (< idx 0) (>= idx (length octave-syms)))
    (error 'octave-list-ref "idx: ~s out of range for octave-syms ~s" idx octave-syms))
  (list-ref octave-syms idx))

(define/contract (pitch-class-list-idx pitch-class-syms pitch-class)
  (-> (non-empty-listof pitch-class?) pitch-class? natural?)
  (let ([ret (list-index ((curry eq?) pitch-class) pitch-class-syms)])
    (when (not ret)
      (error 'pitch-class-list-idx "pitch-class ~s is not in list ~s" pitch-class pitch-class-syms))
    ret))

(define/contract (pitch-class-list-ref pitch-class-syms idx)
  (-> (non-empty-listof pitch-class?) natural? pitch-class?)
  (when (or (< idx 0) (>= idx (length pitch-class-syms)))
    (error 'pitch-class-list-ref "idx: ~s out of range for pitch-class-syms ~s" idx pitch-class-syms))
  (list-ref pitch-class-syms idx))

(define no-duplicates/c
  (make-flat-contract #:name 'no-duplicates/c #:first-order (compose not check-duplicates)))

(struct/contract Scale ([pitch-classes (and/c (non-empty-listof pitch-class?) no-duplicates/c)]) #:transparent)

(define chromatic-sharps (Scale '(C Cs D Ds E F Fs G Gs A As B)))

(define chromatic-flats (Scale '(Cf C Df D Ef E F Gf G Af A Bf)))

(define C-whole-tone (Scale '(C D E Fs Gs As)))

(define Df-whole-tone (Scale '(Df Ef F G A B)))

;; octave-ordered enharmonic equivalents in ascending order C to C
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
  (-> (-> natural? natural? boolean?) pitch-class? pitch-class? boolean?)
  (cmp (fifths-ordered-pitch-class-index sym1) (fifths-ordered-pitch-class-index sym2)))

(define cnt-fifths (length fifths-ordered-pitch-class-syms))

(define/contract (major-scale root-pc [err-sym 'major-scale])
  (->* (pitch-class?) (symbol?) Scale?)
  (when (compare-pitch-class-sym-indices < root-pc 'Cff)
    (error err-sym "root pitch ~s is below minimum ~a" root-pc 'Cff))
  (when (compare-pitch-class-sym-indices > root-pc 'Fss)
    (error err-sym "root pitch ~s is above maximum ~a" root-pc 'Fss))
  (let* ([first (pitch-class-list-idx fifths-ordered-pitch-class-syms root-pc)]
         [start (modulo (sub1 first) cnt-fifths)]
         [stop  (modulo (+ start 7) cnt-fifths)]
         [ixs (map (lambda (n) (modulo n cnt-fifths)) (range start stop))]
         [fifths (map ((curry list-ref) fifths-ordered-pitch-class-syms) ixs)]
         [pcs (map ((curry list-ref) fifths) (map (lambda (i) (modulo i 7))(range 1 14 2)))])
    (Scale pcs)))

(define/contract (minor-scale root-pc)
  (-> pitch-class? Scale?)
  (when (compare-pitch-class-sym-indices < root-pc 'Aff)
    (error 'minor-scale "root pitch ~s is below minimum ~a" root-pc 'Aff))
  (when (compare-pitch-class-sym-indices > root-pc 'Dss)
    (error 'minor-scale "root pitch ~s is above maximum ~a" root-pc 'Dss))
  (let* ([temp-idx (- (fifths-ordered-pitch-class-index root-pc) 3)]
         [temp-pc (fifths-ordered-pitch-class-ref temp-idx)])
    (Scale (rotate-list-by (Scale-pitch-classes (major-scale temp-pc 'minor-scale)) 5))))

(define/contract (relative-minor-scale root-pc)
  (-> pitch-class? Scale?)
  (Scale (rotate-list-by (Scale-pitch-classes (major-scale root-pc 'relative-minor-scale)) 5)))

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

(define/contract (pitch-class->chromatic-enharmonic-index pitch-class)
  (-> pitch-class? natural?)
  (index-where c-ordered-enharmonic-pitch-class-symss (lambda (enh-syms) (member pitch-class enh-syms))))

;; order pitch classes in a scale for transposition relative to C octaves for lilypond notation
(define/contract (scale->c-ordering scale)
  (-> Scale? Scale?)
  (Scale (sort (Scale-pitch-classes scale) < #:key pitch-class->chromatic-enharmonic-index)))

(define/contract (idx->pitch scale idx)
  (-> Scale? natural? pitch/c)
  (let* ([pcs     (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt (length pcs)])
    (let-values ([(q r) (quotient/remainder idx pcs-cnt)])
      (cons (pitch-class-list-ref pcs r) (octave-list-ref q)))))

(define/contract (pitch->idx scale pitch)
  (-> Scale? pitch/c natural? )
  (let* ([pcs     (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt (length pcs)]
         [pcs-idx (pitch-class-list-idx pcs (car pitch))]
         (oct-idx (octave-list-idx (cdr pitch))))
    (+ (* oct-idx pcs-cnt) pcs-idx)))

(define/contract (xpose scale pitch interval)
  (-> Scale? pitch/c integer? pitch/c)
  (let* ([pcs       (Scale-pitch-classes (scale->c-ordering scale))]
         [pcs-cnt   (length pcs)]
         [pitch-idx (pitch->idx scale pitch)])
    (let-values ([(q r) (quotient/remainder (+ pitch-idx interval) pcs-cnt)])
      (cons (pitch-class-list-ref pcs r) (octave-list-ref q)))))

;; supply a single pitch transposed relative to the input pitch or 
;; supply a list pitches each transposed relative to the previous one, 
;; e.g. to get a sequence in scale order, you'd supply (1 1 1 ...)
;; (or call transpose/absolute scale pitch (1 2 3 ...))
(define/contract (transpose/successive scale pitch interval/intervals)
  (-> Scale? pitch/c (or/c integer? (listof integer?)) (or/c pitch/c (listof pitch/c)))
  (if (not (list? interval/intervals))
      (xpose scale pitch interval/intervals)
      (let inner ([p pitch]
                  [is interval/intervals]
                  [ret '()])
        (if (null? is)
            (reverse ret)
            (let* ([next-p (xpose scale p (car is))]
                   [next-is (cdr is)]
                   [next-ret (cons next-p ret)])
              (inner next-p next-is next-ret))))))

;; supply a single pitch transposed relative to the input pitch or
;; supply list of pitches each transposed relative to the input pitch
(define/contract (transpose/absolute scale pitch interval/intervals)
  (-> Scale? pitch/c (or/c integer? (listof integer?)) (or/c pitch/c (listof pitch/c)))
  (if (not (list? interval/intervals))
      (xpose scale pitch interval/intervals)
      (let inner ([is interval/intervals]
                  [ret '()])
        (if (null? is)
            (reverse ret)
            (let* ([next-p (xpose scale pitch (car is))]
                   [next-is (cdr is)]
                   [next-ret (cons next-p ret)])
              (inner next-is next-ret))))))

(define/contract (scale->max-idx scale)
  (-> Scale? natural?)
  (let ([cnt-pcs  (length (Scale-pitch-classes scale))]
        [cnt-octs (length octave-syms)])
    (sub1 (* cnt-octs cnt-pcs))))

;; A simple pitch range into a list of notes of equal duration, no rests.
;; Ascending or descending depending on start, stop, and step.
(define/contract (note-range scale duration pitch start stop [step 1])
  (->* (Scale? duration? pitch/c integer? integer?) (integer?) (listof Note?))
  (when (or (eq? step 0)
            (eq? start stop) 
            (and (< start stop) (< step 0))
            (and (> start stop) (> step 0)))
    (error 'note-range "invalid start ~v, stop ~v, or step ~v" start stop step))
  (let ([pitches (transpose/absolute scale pitch (range start stop step))])
    (map (lambda (p) (Note (car p) (cdr p) duration '() #f)) pitches)))

;; Append ordinary and reverse direction note ranges, up-down or down-up.
(define/contract (inverted-note-ranges scale duration pitch start stop [step 1])
  (->* (Scale? duration? pitch/c integer? integer?) (integer?) (listof Note?))
  (let ([beginning (note-range scale duration pitch start stop step)]
        [ending    (note-range scale duration pitch stop start (- step))])
    (append beginning ending)))
#|
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

(module+ test
  (require rackunit)
  (check-equal?
   '((C . 0va) (B . 8vb) (G . 8vb) (D . 8vb) (G . 15vb))
   (transpose/successive C-major (cons 'C '0va) '(0 -1 -2 -3 -4)))
  (check-exn exn:fail? (lambda () (xpose C-major (cons 'C '0va) 1000)))
  (for-each (lambda (sc)
              (for-each (lambda (n) (check-equal? n (pitch->idx sc (idx->pitch sc n))))
                        (range (scale->max-idx sc))))
            (list C-major chromatic-sharps chromatic-flats A-minor A-major)))
