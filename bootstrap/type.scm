(define type ##type)

(define instance?
  (lambda (t v)
    (eq? (type v) t)))

;; OPTIMIZE: immediate types

(define symbol? (lambda (v) (instance? <symbol> v)))
(define pair? (lambda (v) (instance? <pair> v)))
(define vector? (lambda (v) (instance? <vector> v)))
(define syntax? (lambda (v) (instance? <syntax> v)))

