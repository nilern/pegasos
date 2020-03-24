;;; FIXME: `(not (eq? (type-hash 23) (%identity-hash <fixnum>)))` etc.

;; OPTIMIZE: immediate types
(define instance?
  (lambda (t v)
    (eq? (type v) t)))
(define pair? (lambda (v) (instance? <pair> v)))
(define syntax? (lambda (v) (instance? <syntax> v)))

