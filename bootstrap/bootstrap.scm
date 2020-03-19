(define not (lambda (v) (eq? v #f)))

(define symbol?
  (lambda (v)
    (if (eq? (%immediate-type-index v) 1)
      (eq? (%heap-type-index v) 9)
      #f)))

(include "list.scm")
(include "vector.scm")
(include "records.scm")
(include "type.scm")
(include "comparator.scm")
(include "hash-table.scm")
(include "set.scm")
(include "expander.scm")

