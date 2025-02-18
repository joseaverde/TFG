; ==== Syntax =============================================================== ;

(define-syntax srfi-26-internal-cut
  (syntax-rules (<> <...>)

    ;; construct fixed- or variable-arity procedure:
    ;;   (begin proc) throws an error if proc is not an <expression>
    ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
     (lambda (slot-name ...) ((begin proc) arg ...)))
    ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
     (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))

    ;; process one slot-or-expr
    ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
     (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
    ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
     (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

(define-syntax cut
  (syntax-rules ()
    ((cut . slots-or-exprs)
     (srfi-26-internal-cut () () . slots-or-exprs))))

(define-syntax when
  (syntax-rules ()
    ((when cond code ...)
     (if cond (begin code ...) #f))))

(define-syntax for
  (syntax-rules (in .. =>)
    ((for x in from .. to =>
        code ...)
     (letrec [(last to)
              (helper (lambda (x)
                        (when (<= x last)
                          code ...
                          (helper (+ x 1)))))]
       (helper from)))))

(define-syntax vfor
  (syntax-rules (in =>)
    ((for i x in vec =>
        code ...)
     (for i in 0 .. (- (vector-length vec) 1) =>
        (let [(x (vector-ref vec i))]
           code ...)))))

; ==== Vectors ============================================================== ;

(define (reduce func acc lst)
  (if (null? lst) acc
    (reduce func (func acc (car lst)) (cdr lst))))

(define (vector-append . rest)
  (let [(result (make-vector (reduce + 0 (map vector-length rest))))
        (index 0)]
    (for-each
      (lambda (vec)
        (vfor k x in vec =>
           (vector-set! result (+ index k) x))
        (set! index (+ index (vector-length vec))))
      rest)
    result))

(define (subvector vec from to)
  (let [(result (make-vector (- to from)))]
    (vfor k x in result =>
      (vector-set! result k (vector-ref vec (+ from k))))
    result))

(define (make-list size val)
  (if (positive? size)
    (cons val (make-list (- size 1) val))
    '()))

; ==== Bytes ================================================================ ;

(define byte= eq?)
(define %byte-array (make-vector 256))

(define (shift-right num k)
  (if (positive? k)
    (shift-right (quotient num 2) (- k 1))
    num))

(define (shift-left num k)
  (if (positive? k)
    (shift-left (* num 2) (- k 1))
    num))

(define (bit-at num k)
  (modulo (shift-right num k) 2))

(define (to-bits num)
  (list->vector (map (cut bit-at num <>) '(0 1 2 3 4 5 6 7))))

(for i in 0 .. 255 =>
   (vector-set! %byte-array i (cons (to-bits i)
                                    (integer->char i))))

(define (char->byte num)
  (display "byte = ") (display (char->integer num)) (newline)
  (vector-ref %byte-array (char->integer num)))

(define (integer->byte num)
  (char->byte (integer->char num)))

(define byte->char cdr)

(define (byte->integer byte)
  (char->integer (byte->char byte)))

(define (read-byte port)
  (char->byte (read-char port)))

(define byte->bits car)

(define (%bits->integer bits last index result)
  (if (>= index last) result
    (%bits->integer bits last (+ index 1) (+ (* result 2)
                                             (vector-ref bits index)))))

(define (bits->unsigned bits)
  (%bits->integer bits (vector-length bits) 0 0))

(define (bits->signed bits)
  (let [(result (bits->unsigned bits))]
    (if (= 0 (vector-ref bits 0)) result
      (- result 256))))

; ==== Deserialisation ====================================================== ;

(define (read-uint8 port)
  (byte->integer (read-byte port)))

(define (read-8-bits port)
  (byte->bits (read-byte port)))

(define (read-uint32 port)
  (let* [(b0 (read-uint8 port))
         (b1 (read-uint8 port))
         (b2 (read-uint8 port))
         (b3 (read-uint8 port))]
    (+ (* b0 #x1)
       (* b1 #x100)
       (* b2 #x10000)
       (* b3 #x1000000))))

(define (exp2 pow)
  (if (negative? pow)
    (/ (shift-left 1 (abs pow)))
    (shift-left 1 (abs pow))))

(define (%ieee754-reader bytes exp-bits)
  (let* [(float-bits    (* bytes 8))
         (mantissa-bits (- float-bits exp-bits 1))
         (exp-offset    (- (shift-left 1 (- exp-bits 1)) 1))
         (range         (make-list bytes 0))
         (den           (shift-left 1 mantissa-bits))]
    (lambda (port)
      (let* [(bits (apply vector-append
                          (map (lambda (x) (read-8-bits port)) range)))
             (sign (if (zero? (vector-ref bits 0)) 1 -1))
             (expn (- (bits->unsigned (subvector bits 1 (+ 1 exp-bits)))
                      exp-offset))
             (mantissa (/ (bits->unsigned
                            (subvector bits (+ 1 exp-bits) float-bits))
                          den))]
        (display bits) (newline)
        (display (bits->unsigned bits)) (newline)
        (display "sign     = ") (display sign) (newline)
        (display "expn     = ") (display expn) (newline)
        (display "mantissa = ") (display mantissa) (newline)
        (display "exponent = ") (display (exp2 expn)) (newline)
        (newline)
        (if (zero? (+ expn exp-offset))
          (* mantissa sign)
          (* (+ 1 mantissa) (exp2 expn)))))))

(define read-float (%ieee754-reader 4 8))
(define read-double (%ieee754-reader 8 11))

; ==== Signals ============================================================== ;

(define (signal? obj)
  obj)

(define in (open-input-file "../common/chb03.bin"))

(display (read-uint32 in)) (newline)
(display (read-uint32 in)) (newline)
(display (exact->inexact (read-double in))) (newline)
(display (exact->inexact (read-double in))) (newline)
(display (exact->inexact (read-double in))) (newline)
(display (exact->inexact (read-double in))) (newline)
(display (exact->inexact (read-double in))) (newline)
