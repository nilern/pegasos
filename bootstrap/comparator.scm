(define <comparator>
  (make-rtd '<comparator> #((immutable type-test)
                            (immutable equality)
                            (immutable ordering)
                            (immutable hash))))

(define make-comparator (rtd-constructor <comparator>))

(define symbol-hash ##symbol-hash)

(define comparator-type-test-predicate (rtd-accessor <comparator> 'type-test))
(define comparator-equality-predicate (rtd-accessor <comparator> 'equality))
(define comparator-ordering-predicate (rtd-accessor <comparator> 'ordering))
(define comparator-hash-function (rtd-accessor <comparator> 'hash))

