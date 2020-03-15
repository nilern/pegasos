(define builtin-types (make-vector 48))

;;; FIXME: `(not (eq? (type-hash 23) (%identity-hash <fixnum>)))` etc.

(define <fixnum> (make-rtd '<fixnum> #()))
(vector-set! builtin-types 0 <fixnum>)

(define <flonum> (make-rtd '<flonum> #()))
(vector-set! builtin-types 2 <flonum>)

(define <char> (make-rtd '<char> #()))
(vector-set! builtin-types 7 <char>)

(define <boolean> (make-rtd '<boolean> #()))
(vector-set! builtin-types 11 <boolean>)

(define <null> (make-rtd '<null> #()))
(vector-set! builtin-types 16 <null>)

(define <unbound> (make-rtd '<unbound> #()))
(vector-set! builtin-types 17 <unbound>)

(define <unspecified> (make-rtd '<unspecified> #()))
(vector-set! builtin-types 18 <unspecified>)

(define <eof-object> (make-rtd '<eof-object> #()))
(vector-set! builtin-types 19 <eof-object>)

(define <string> (make-rtd '<string> #()))
(vector-set! builtin-types 32 <string>)

(define <symbol> (make-rtd '<symbol> #()))
(vector-set! builtin-types 33 <symbol>)

(define <pair> (make-rtd '<pair> #()))
(vector-set! builtin-types 34 <pair>)

(define <vector> (make-rtd '<vector> #()))
(vector-set! builtin-types 35 <vector>)

(define <procedure> (make-rtd '<procedure> #()))
(vector-set! builtin-types 36 <procedure>)

(define <environment> (make-rtd '<environment> #()))
(vector-set! builtin-types 37 <environment>)

(define <syntax> (make-rtd '<syntax> #()))
(vector-set! builtin-types 38 <syntax>)

(define type
  (lambda (v)
    (let ((i (%immediate-type-index v)))
      (if (eq? i 1)
        (let ((h (%heap-type-index v)))
          (if (eq? h 7)
            (%slot-ref v 0)
            (vector-ref builtin-types (fx+ 32 h))))
        (vector-ref builtin-types i)))))

(define type-hash
  (lambda (v)
    (let ((i (%immediate-type-index v)))
      (if (eq? i 1)
        (let ((h (%heap-type-index v)))
          (if (eq? h 7)
            (%identity-hash (%slot-ref v 0))
            (fx+ 32 h)))
        i))))

;; OPTIMIZE: builtin types
(define instance?
  (lambda (t v)
    (eq? (type v) t)))

