(define instance?
  (lambda (t v)
    (eq? (type v) t)))

