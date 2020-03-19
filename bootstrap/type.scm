(define builtin-types (make-vector 16))

;;; FIXME: `(not (eq? (type-hash 23) (%identity-hash <fixnum>)))` etc.

(define <fixnum> (make-rtd '<fixnum> #()))
(vector-set! builtin-types 0 <fixnum>)

(define <flonum> (make-rtd '<flonum> #()))
(vector-set! builtin-types 2 <flonum>)

(define <char> (make-rtd '<char> #()))
(vector-set! builtin-types 3 <char>)

(define <boolean> (make-rtd '<boolean> #()))
(vector-set! builtin-types 4 <boolean>)

(define <null> (make-rtd '<null> #()))
(vector-set! builtin-types 5 <null>)

(define <unbound> (make-rtd '<unbound> #()))
(vector-set! builtin-types 6 <unbound>)

(define <unspecified> (make-rtd '<unspecified> #()))
(vector-set! builtin-types 7 <unspecified>)

(define <string> (make-rtd '<string> #()))
(vector-set! builtin-types 8 <string>)

(define <symbol> (make-rtd '<symbol> #()))
(vector-set! builtin-types 9 <symbol>)

(define <pair> (make-rtd '<pair> #()))
(vector-set! builtin-types 10 <pair>)

(define <vector> (make-rtd '<vector> #()))
(vector-set! builtin-types 11 <vector>)

(define <procedure> (make-rtd '<procedure> #()))
(vector-set! builtin-types 12 <procedure>)

(define <environment> (make-rtd '<environment> #()))
(vector-set! builtin-types 13 <environment>)

(define <syntax> (make-rtd '<syntax> #()))
(vector-set! builtin-types 14 <syntax>)

(define type
  (lambda (v)
    (let* ((i (%immediate-type-index v)))
      (if (eq? i 1)
        (let* ((h (%heap-type-index v)))
          (if (eq? h 7)
            (%slot-ref v 0)
            (vector-ref builtin-types (fx+ 8 h))))
        (vector-ref builtin-types i)))))

(define type-hash
  (lambda (v)
    (let* ((i (%immediate-type-index v)))
      (if (eq? i 1)
        (let* ((h (%heap-type-index v)))
          (if (eq? h 7)
            (%identity-hash (%slot-ref v 0))
            (fx+ 8 h)))
        i))))

;; OPTIMIZE: builtin types
(define instance?
  (lambda (t v)
    (eq? (type v) t)))
(define pair? (lambda (v) (instance? <pair> v)))
(define syntax? (lambda (v) (instance? <syntax> v)))

