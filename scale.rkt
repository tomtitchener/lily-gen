#lang racket

(require br/macro)
(require srfi/1)
(require "score.rkt")
(require "score-syms.rkt")
(require "utils.rkt")

(provide (all-defined-out))


(define/contract (scale-ctor-guard pitch-classes type-name)
  (-> (listof pitch-class?) symbol? (listof pitch-class?))
  (cond [(null? pitch-classes)
         (error type-name "empty list of pitch-classes")]
        [(check-duplicates pitch-classes)
         (error type-name "pitch-classes not unique: ~e" pitch-classes)]
        [else pitch-classes]))

(struct Scale (pitch-classes) #:transparent #:guard scale-ctor-guard)

#;(struct Scale (pitch-classes)
  #:transparent
  #:guard (lambda (pitch-classes type-name)
            (cond [(null? pitch-classes)
                   (error type-name "empty list of pitch-classes")]
                  [(not (andmap pitch-class? pitch-classes))
                   (error type-name "not only pitch-classes in list ~e" pitch-classes)]
                  [(check-duplicates pitch-classes)
                   (error type-name "pitch-classes not unique: ~e" pitch-classes)]
                  [else pitch-classes])
                  ))

(define chromatic-sharps (Scale '(C Cs D Ds E F Fs G Gs A As B)))

(define chromatic-flats (Scale '(Cf C Df D Ef E F Gf G Af A Bf)))

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
