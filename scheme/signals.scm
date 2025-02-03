;; Signals

(define (signal? . obj)
  (and (pair?    obj)
       (pair?    (car obj))
       (vector?  (cdr obj))
       (integer? (caar obj))
       (integer? (cdar obj))))

(define signal-first  caar)
(define signal-last   cdar)
(define signal-vector cdr)

(define (signal-ref . (signal k))
  (vector-ref (signal-vector signal)
              (+ k (signal-first signal))))

(define (%signal-reduce% . (func vector last index acc))
  (if (>= index last) acc
    (%signal-reduce% func signal last (+ 1 index)
                     (func acc (vector-ref vector index)))))

(define (signal-reduce . (func signal acc))
  (%signal-reduce% func
                   (signal-vector signal)
                   (signal-last signal)
                   (signal-first signal)
                   acc))

;; Load the file

(define-syntax for
  (syntax-rules (in .. =>)
    ((for index in first .. last =>
        proc
        procs ...)
     (letrec [(the-last last)
              (helper
                (lambda (index)
                  (if (<= index the-last)
                    (begin
                      proc
                      procs ...
                      (helper (+ index 1))))))]
       (helper first)))))

(define (vector-copy . (vec))
  (let [(res (make-vector (vector-length vec)))]
    (for k in 0 .. (- (vector-length vec) 1) =>
       (vector-set! res k (vector-ref vec k)))
    res))

(define %char->bits-map% (make-vector 256))

(for b7 in 0 .. 2 
