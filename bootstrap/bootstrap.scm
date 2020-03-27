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

(define make-vector ##make-vector)

(include "type.scm")
(include "list.scm")
(include "vector.scm")
;(include "records.scm")
;(include "comparator.scm")
;(include "hash-table.scm")
;(include "set.scm")
;(include "expander.scm")

