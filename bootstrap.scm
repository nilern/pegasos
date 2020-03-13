(define list (lambda xs xs))

(define vector-length %length)

(define vector-map
  (lambda (f vec)
    (let ((len (vector-length vec)))
      (let ((vec* (make-vector len))
            (loop #f))
        (begin
          (set! loop (lambda (i)
                       (if (fx<? i len)
                         (vector-set! vec* i (f (vector-ref vec i)))
                         vec*)))
          (loop 0))))))

(define <record-type> #f)

(define make-rtd
  (lambda (name fieldspecs . args)
    (let ((parent (if (null? args)
                    #f
                    (car args)))
          (fields (vector-map (lambda (fieldspec)
                                (if (symbol? fieldspec)
                                  (list 'mutable fieldspec)
                                  fieldspec))
                               fieldspecs)))
      (%record <record-type> parent fields name))))

(set! <record-type>
  (make-rtd '<record-type>
    #((immutable parent)
      (immutable fields)
      (immutable name))))

