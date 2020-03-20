(define apply ##apply)
(define values ##values)
(define call-with-values ##call-with-values)

(define void ##void)

(define eq? ##eq?)

(define not (lambda (v) (eq? v #f)))

(define fx<? ##fx<?)
(define fx+ ##fx+)
(define fx- ##fx-)
(define bitwise-and ##bitwise-and)
(define bitwise-ior ##bitwise-ior)
(define bit-count ##bit-count)
(define arithmetic-shift ##arithmetic-shift)

(define symbol?
  (lambda (v)
    (if (eq? (##immediate-tag v) 1)
      (eq? (##heap-tag v) 1)
      #f)))

(define vector?
  (lambda (v)
    (if (eq? (##immediate-tag v) 1)
      (eq? (##heap-tag v) 3)
      #f)))

(define make-vector ##make-vector)

(include "list.scm")
(include "vector.scm")
(include "records.scm")
(include "type.scm")
(include "comparator.scm")
(include "hash-table.scm")
(include "set.scm")
(include "expander.scm")

