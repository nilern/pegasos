(define builtin-types (make-vector 9))

;;; FIXME: `(not (eq? (type-hash 23) (%identity-hash <fixnum>)))` etc.

(define <string> (make-rtd '<string> #()))
(vector-set! builtin-types 0 <string>)

(define <symbol> (make-rtd '<symbol> #()))
(vector-set! builtin-types 1 <symbol>)

(define <pair> (make-rtd '<pair> #((mutable car) (mutable cdr))))
(vector-set! builtin-types 2 <pair>)
 
(define <vector> (make-rtd '<vector> #()))
(vector-set! builtin-types 3 <vector>)

(define <procedure> (make-rtd '<procedure> #()))
(vector-set! builtin-types 4 <procedure>)

(define <environment> (make-rtd '<environment> #()))
(vector-set! builtin-types 5 <environment>)

(define <syntax> (make-rtd '<syntax> #((immutable datum) (immutable scopes)
                                       (immutable source) (immutable line) (immutable column))))
(vector-set! builtin-types 6 <syntax>)

(define <type> (make-rtd '<type> #((immutable parent) (immutable name) (immutable fields))))
(vector-set! builtin-types 8 <type>)

(define type
  (lambda (v)
    (let* ((t (##immediate-type v)))
      (if t
        t
        (let* ((h (##heap-tag v)))
          (if (eq? h 7)
            (##slot-ref v 0)
            (vector-ref builtin-types h)))))))

(define type-hash
  (lambda (v)
    (let* ((i (##immediate-tag v)))
      (if (eq? i 1)
        (let* ((h (##heap-tag v)))
          (if (eq? h 7)
            (##identity-hash (##slot-ref v 0))
            (fx+ 8 h)))
        i))))

;; OPTIMIZE: builtin types
(define instance?
  (lambda (t v)
    (eq? (type v) t)))
(define pair? (lambda (v) (instance? <pair> v)))
(define syntax? (lambda (v) (instance? <syntax> v)))

