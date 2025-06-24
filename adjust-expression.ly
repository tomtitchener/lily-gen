\version "2.19.46"


% Default expression level equalizer function.
#(define adjustExpression:equalizer default-dynamic-absolute-volume)

% Default minimum absolute expression level.
#(define adjustExpression:minLevel 0.0)

% Default maximum absolute expression level.
#(define adjustExpression:maxLevel 1.0)


% Helper function for finding the equalized expression level corresponding
% to a dynamic string, similar to how MIDI velocity equalization works in
% Dynamic_performer.  The dynamic-string argument can be any string
% recognized by the adjustExpression:equalizer function, or a fractional
% value between 0.0 and 1.0 (specified as a string).
#(define adjustExpression:equalize-expression
  (lambda (dynamic-string min-level max-level)
   (let ((absolute-range (- max-level min-level))
         (fraction (adjustExpression:equalizer dynamic-string)))
    (if (not (number? fraction))
     ;; Dynamic string was not recognized by the equalizer function, try to
     ;; treat the dynamic string as a stringified fractional value.
     (set! fraction (string->number dynamic-string)))
    (if (not (number? fraction))
     (ly:error
      "adjustExpression:equalize-expression: invalid dynamic string ~s"
      dynamic-string))
    (if (and (>= fraction 0.0) (<= fraction 1.0))
     (+ min-level (* fraction absolute-range))
     (ly:error
      "adjustExpression:equalize-expression: expression level for dynamic ~s out of range"
      dynamic-string)))))


setExpression =
#(define-music-function (dynamic) (string?)

  "Sets the MIDI expression level to the fraction identified by the given
dynamic string between @code{adjustExpression:minLevel} and
@code{adjustExpression:maxLevel}.  The dynamic string can be any string
recognized by the @code{adjustExpression:equalizer} function, or a
fractional value between 0.0 and 1.0 specified as a string."
					      
  (let ((expr
	 (adjustExpression:equalize-expression
	  dynamic
	  adjustExpression:minLevel
	  adjustExpression:maxLevel)))
   #{ \set Staff.midiExpression = #expr #}))


adjustExpression =
#(define-music-function (step
                         initial-dynamic
                         initial-pad
                         main-music
                         final-pad
			 omit-final-adjustment
                         final-dynamic)
                        ((ly:moment? (ly:make-moment 1 64))
                         string?
                         ly:music?
                         ly:music?
                         ly:music?
			 (boolean? #f)
                         string?)

   "Adjusts the MIDI expression level starting from the level identified
by @var{initial-dynamic} to that identified by @var{final-dynamic} during
the music expression @var{main-music} via a linear sequence of changes to
the @code{Staff.midiExpression} context property, using the
@code{adjustExpression:equalizer} function to map fractional expression
levels between 0.0 and 1.0 to the absolute range between
@code{adjustExpression:minLevel} and @code{adjustExpression:maxLevel}.
The @var{initial-dynamic} and @var{final-dynamic} parameters can be any
strings recognized by the @code{adjustExpression:equalizer} function, or
fractional values between 0.0 and 1.0 specified as strings.

The adjustment will span the portion of @var{main-music} bounded from the
beginning by the duration of @var{initial-pad}, and from the end by the
duration of @var{final-pad}.  (The combined duration of @var{initial-pad}
and @var{final-pad} should be less than the duration of @var{main-music}.)

The frequency of emitting context property changes within the span of the
adjustment can be specified using the optional @var{step} moment argument;
by default, changes in the context property will be separated by 1/64
rests.  (It is recommended that the duration of the adjustment span is an
exact multiple of the duration of @var{step}.)

If the optional @var{omit-final-adjustment} flag is not set (the default),
the MIDI expression level will be explicitly set to the level corresponding
to @var{final-dynamic} at the end of the adjustment span bounded by
@var{initial-pad} and @var{final-pad}.  In case @var{final-pad} has zero
duration, and there are other changes to the MIDI expression level
occurring at the musical moment at which @var{main-music} ends (created by,
for example, another call to this function at that musical moment), it is
recommended to raise this flag for the first call to avoid the emission of
multiple or competing changes to the expression level at the boundary."

  (let* ((adjustment-duration
	  (ly:moment-sub
	   (ly:music-length main-music)
	   (ly:moment-add
	    (ly:music-length initial-pad)
	    (ly:music-length final-pad))))
         (n (floor (ly:moment-main (ly:moment-div adjustment-duration step))))
         (silence (ly:music-compress #{ s1 #} step)))
   (if (<= n 0)
    (ly:error "adjustExpression: invalid span for expression adjustment"))
   (let* ((initial-level
           (adjustExpression:equalize-expression
	    initial-dynamic
	    adjustExpression:minLevel
	    adjustExpression:maxLevel))
          (final-level
           (adjustExpression:equalize-expression
	    final-dynamic
	    adjustExpression:minLevel
	    adjustExpression:maxLevel))
          (unit (/ (- final-level initial-level) n)))
    (letrec ((adjust-expression-at-endpoint
              (lambda (expr)
               (list #{ \set Staff.midiExpression = #expr #})))
             (adjust-expression-between-endpoints
              (lambda (i)
               (if (< i n)
                (cons
                 #{
                  \set Staff.midiExpression = #(+ initial-level (* i unit))
                  #silence
                 #}
                 (adjust-expression-between-endpoints (+ i 1)))
                '()))))
     (make-simultaneous-music
      (list main-music
            (make-sequential-music
             (append
              (adjust-expression-at-endpoint initial-level)
              (list (ly:music-compress #{ s1 #} (ly:music-length initial-pad)))
              (adjust-expression-between-endpoints 0)
              (if omit-final-adjustment
               '()
               (adjust-expression-at-endpoint final-level))))))))))
